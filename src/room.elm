module Main where

import Json.Decode as Json exposing ((:=))
import Json.Encode
import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)

import Date exposing (Date)
import Time exposing (Time, every, second)
import String
import Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Signal exposing (..)

import Lib.API as API exposing (PeerId, User)
import Lib.Header as Header
import Lib.WebSocket as WS
import Lib.WebRTC as WebRTC
import Lib.VideoControl as VideoControl
import Lib.ChatView as ChatView

import Debug exposing (log)


-- Models

type alias Context = { roomName:String
                      , address: Signal.Address Action
                      , rtc : WebRTC.Model
                      , chat: ChatView.Model}
type alias MediaType = String
type alias Connection = (PeerId, MediaType)

mediaTypes = ["mic", "video", "screen"]
initialContext : Context
initialContext = { roomName = "　"
  , address = actions.address
  , rtc = WebRTC.init
  , chat = ChatView.init }

nullInitialData : API.InitialData
nullInitialData = { room= {id="", peers=[], users= []}, user={name="", email=""}}

-- Data access

fetchRoom : String -> Task err ()
fetchRoom roomId = (API.getInitialData roomId)
    `andThen` (\initial -> (Signal.send actions.address (InitRoom initial)))
    `onError` (\err -> log "err" (succeed ()))

port clientId : String
port roomName : String


port updateRoom : Signal String

port runner : Signal (Task err ())
port runner = Signal.map fetchRoom updateRoom

port websocketRunner : Signal ()

port websocketRunner' : Signal (Task () ())
port websocketRunner' = Signal.map (\_ -> WS.connect "wss://localhost:9999/ws") websocketRunner


port runTasks : Signal (Task () ())
port runTasks =
  let f action opened = (case log "runTasks" (opened, action) of
      (True, InitRoom initial) -> WS.send joinToJson
      (_, RTCAction (WebRTC.Request x)) -> WS.send (signalToJson x)
      (_, RTCAction x) -> WebRTC.doTask x
      (_, ChatAction (ChatView.Send x)) -> WS.send (messageToJson x 0) -- TODO
      (_, FullScreen x) -> VideoControl.requestFullScreen x
      (_, StartStreaming x) -> WebRTC.doTask <| WebRTC.StartStreaming x
      (_, EndStreaming x) -> WebRTC.doTask <| WebRTC.EndStreaming x
      _ -> Task.succeed ()
    )
  in Signal.map2 f actionSignal WS.opened


signalToJson : (String, String, String) -> String
signalToJson (type_, to, data_) =
  let value = Json.Encode.object [
    ("room", Json.Encode.string roomName),
    ("from", Json.Encode.string clientId),
    ("to", Json.Encode.string to),
    ("type", Json.Encode.string type_),
    ("data", Json.Encode.string data_)
  ]
  in Json.Encode.encode 0 value

messageToJson : String -> Time -> String
messageToJson mes time =
  let value = Json.Encode.object [
    ("room", Json.Encode.string roomName),
    ("from", Json.Encode.string clientId),
    ("type", Json.Encode.string "message"),
    ("data", Json.Encode.object [
      ("message", Json.Encode.string mes),
      ("time", Json.Encode.float time)
    ])
  ]
  in Json.Encode.encode 0 value

joinToJson : String
joinToJson =
  let value = Json.Encode.object [
    ("room", Json.Encode.string roomName),
    ("from", Json.Encode.string clientId),
    ("type", Json.Encode.string "join")
  ]
  in Json.Encode.encode 0 value

-- Signals

context : Signal Context
context = Signal.foldp update initialContext actionSignal

userOf : Context -> PeerId -> User
userOf c peerId = case Dict.get peerId c.rtc.users of
  Just user -> user
  Nothing -> { name="", email="" }

-- Action --

type Action
  = NoOp
  | RTCAction WebRTC.Action
  | ChatAction ChatView.Action
  | InitRoom API.InitialData
  | StartStreaming (String, List PeerId)
  | EndStreaming (String, List PeerId)
  | FullScreen String

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

decode : String -> Action
decode s = case WebRTC.decode s of
  Just a -> RTCAction a
  Nothing -> case ChatView.decode s of
    Just a -> ChatAction a
    Nothing -> NoOp


actionSignal : Signal Action
actionSignal = Signal.mergeMany [
  actions.signal
  , decode <~ WS.message
  , RTCAction <~ WebRTC.actions
  ]


-- Update --

update : Action -> Context -> Context
update action context =
    case log "action" action of
      RTCAction event ->
        { context |
          rtc <- WebRTC.update event context.rtc
        }
      InitRoom initial ->
        { context |
          roomName <- initial.room.id,
          rtc <- WebRTC.update (WebRTC.InitRoom initial.room.peers initial.room.users initial.user) context.rtc,
          chat <- ChatView.update (ChatView.MyName initial.user.name) context.chat
        }
      StartStreaming a -> { context |
          rtc <- WebRTC.update (WebRTC.StartStreaming a) context.rtc
        }
      EndStreaming a -> { context |
          rtc <- WebRTC.update (WebRTC.EndStreaming a) context.rtc
        }
      ChatAction action ->
        { context |
          chat <- ChatView.update action context.chat
        }
      _ -> context

-- View --

fullscreenButton : Address Action -> String -> Html
fullscreenButton address videoURL = div [
      class "btn pull-right",
      onClick address (FullScreen videoURL)
    ] [
      div [class "glyphicon glyphicon-fullscreen"] []
    ]

windowCloseButton : Context -> String -> Html
windowCloseButton c mediaType = div [
      class "btn pull-right",
      onClick c.address (EndStreaming (mediaType, (Set.toList c.rtc.peers)))
    ] [
      div [class "glyphicon glyphicon-remove"] []
    ]

windowHeader : String -> List Html -> Html
windowHeader title buttons =
  let buttonGroup = div [class "btn-group pull-right"] buttons
  in div [class "panel-heading clearfix"] [(text title), buttonGroup]


-- View
view : Context -> Html
view c =
  div [] [
    Header.header {user= {name=c.rtc.me.name}},
    div [class "container"] [
      statusView c,
      mainView c,
      ChatView.view (forwardTo c.address ChatAction) c.chat
    ]
  ]

window : Html -> Html -> Bool -> Html
window header body local =
  let face = if local then "panel-primary" else "panel-default"
  in div [class "col-sm-6 col-md-6"] [
        div [class ("panel " ++ face)] [header, body]
      ]

roomTitle c = h2 [ class "room-name" ] [text c.roomName]

madiaIcon : String -> Html
madiaIcon mediaType =
  let classes = case mediaType of
      "video" -> "fa fa-video-camera"
      "mic" -> "fa fa-microphone"
      "screen" -> "fa fa-desktop"
  in i [class classes] []

madiaButton : Context -> String -> Html
madiaButton c mediaType =
  let classes = case mediaType of
        "video" -> "fa fa-video-camera"
        "mic" -> "fa fa-microphone"
        "screen" -> "fa fa-desktop"
      streaming = case Dict.get mediaType c.rtc.localVideoUrls of
        Just _ -> True
        Nothing -> False
      face = if streaming then "btn-primary" else "btn-default"
      action = if streaming then EndStreaming (mediaType, peers) else StartStreaming (mediaType, peers)
      peers = Set.toList c.rtc.peers
  in button [
    Html.Attributes.type' "button",
    class ("btn " ++ face),
    onClick c.address action
  ] [madiaIcon mediaType]

mediaButtons : Address Action -> Context -> Html
mediaButtons address c = div [
    Html.Attributes.attribute "role" "group", class "btn-group"
  ] (List.map (madiaButton c) mediaTypes)

peerView : Address Action -> Context -> PeerId -> Html
peerView address c peer =
  let user = userOf c peer
  in li [] [
    div [] [
      i [class "fa fa-user"] [],
      text user.name
      -- text peer
    ]
  ]

peerViews : Address Action -> Context -> List PeerId -> Html
peerViews address c peers = ul [
    class "user-list list-unstyled hidden-xs"
  ] (List.map (\peer -> peerView address c peer) peers)

statusView : Context -> Html
statusView c = div [class "col-sm-3 col-md-3"] [
    div [class "status-panel row panel panel-default"] [
      div [class "panel-body"] [
        roomTitle c,
        mediaButtons c.address c,
        peerViews c.address c (Set.toList c.rtc.peers)
      ]
    ]
  ]

mainView : Context -> Html
mainView c = div [class "col-sm-9 col-md-9"] [div [class "row"] (mediaViews c)]

mediaViews : Context -> List Html
mediaViews c =
  let localList = List.map (\mediaType -> localMediaWindowView c.address c mediaType) ["video", "screen"]
      remoteList = List.map (\connection -> remoteMediaWindowView c.address c connection) (Set.toList c.rtc.connections)
      both = List.concat [localList, remoteList]
      filterd = List.filter (\maybe -> case maybe of
          Just a -> True
          Nothing -> False
        ) both
  in List.map (\(Just a) -> a) filterd

localMediaWindowView : Address Action -> Context -> String -> Maybe Html
localMediaWindowView address c mediaType =
  let title = "Local " ++ mediaType
      maybeVideoUrl = Dict.get mediaType c.rtc.localVideoUrls
  in Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl True) maybeVideoUrl

remoteMediaWindowView : Address Action -> Context -> Connection -> Maybe Html
remoteMediaWindowView address c connection =
  let (peerId, mediaType) = connection
      user = userOf c peerId
      title = String.concat [user.name]
      maybeVideoUrl = Dict.get connection c.rtc.videoUrls
  in Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl False) maybeVideoUrl

mediaWindowView : Context -> String -> String -> String -> Bool -> Html
mediaWindowView c mediaType title videoUrl local =
  let videoHtml = video [
            src videoUrl,
            Html.Attributes.attribute "autoplay" ""
          ] []
      buttons = if | local -> [windowCloseButton c mediaType, fullscreenButton c.address videoUrl]
                   | otherwise -> [fullscreenButton c.address videoUrl]
  in window (windowHeader title buttons) videoHtml local


-- Main
main : Signal Html
main = view <~ context


























--
