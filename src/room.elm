module Main where

import Json.Decode as Json exposing ((:=))
import Json.Encode
import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)

import Date exposing (Date)
import Time exposing (Time)
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
type alias RawWSMessage = (String, PeerId, String)
type alias WSMessage = (String, PeerId, Maybe WsMessageBody)
type WsMessageBody = WSChatMessage String Time

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

port initRoom : Signal API.InitialData
port initRoom =
  let f x = case x of
    (InitRoom initial, True) -> Just initial
    _ -> Nothing
  in Signal.filterMap f nullInitialData (Signal.map2 (\x y -> (x, y)) actionSignal WS.opened)


port updateRoom : Signal String

port runner : Signal (Task err ())
port runner = Signal.map fetchRoom updateRoom

port websocketRunner : Signal ()

port websocketRunner' : Signal (Task () ())
port websocketRunner' = Signal.map (\_ -> WS.connect "wss://localhost:9999/ws") websocketRunner

rawWsMessage : Signal RawWSMessage
rawWsMessage =
  let f s = case (log "raw" <| Json.decodeString wsMessageDecoder s) of
    Ok value -> value
    Err s -> ("","","")
  in Signal.map f WS.message


constructedWsMessage : Signal WSMessage
constructedWsMessage =
  let f (type_, peerId, data) =
    let decoder = wsMessageBodyDecoder type_
      in case decoder of
        Just d ->
          case (log "parse" (Json.decodeString d data)) of
            Ok value -> (type_, peerId, Just value)
            Err s -> (type_, peerId, Nothing)
        Nothing -> (type_, peerId, Nothing)
  in Signal.map f rawWsMessage


rtcMessage : Signal WebRTC.Action
rtcMessage = WebRTC.actions WS.message

port wssend : Signal String
port wssend' : Signal (Task () ())
port wssend' = WS.send <~ wssend


port sendChat : Signal String
port sendChat =
  let f action = case action of
    ChatViewAction (ChatView.Send mes) -> Just mes
    _ -> Nothing
  in Signal.filterMap f "" actionSignal


port runRTC : Signal (Task () ())
port runRTC =
  let f action = case action of
    RTCAction x -> WebRTC.doTask x
    _ -> Task.succeed ()
  in Signal.map f actionSignal


port requestFullScreen : Signal (Task () ())
port requestFullScreen =
  let f action = case action of
    FullScreen a -> Just (VideoControl.requestFullScreen a)
    _ -> Nothing
  in Signal.filterMap f (VideoControl.requestFullScreen "") actionSignal


-- Signals

connection : String -> String -> Connection
connection peer mediaType =(peer, mediaType)



updateActionSignal : Signal Action
updateActionSignal =
  let f action = case action of
    ChatViewAction (ChatView.Send _) -> False
    _ -> True
  in Signal.filter f NoOp actionSignal

context : Signal Context
context = Signal.foldp update initialContext updateActionSignal

userOf : Context -> PeerId -> User
userOf c peerId = case Dict.get peerId c.rtc.users of
  Just user -> user
  Nothing -> { name="", email="" }

wsMessageDecoder : Json.Decoder RawWSMessage
wsMessageDecoder = Json.object3 (\t f d -> (t, f, Json.Encode.encode 0 d))
  ("type" := Json.string)
  ("from" := Json.string)
  ("data" := Json.value)

wsMessageBodyDecoder : String -> Maybe (Json.Decoder WsMessageBody)
wsMessageBodyDecoder type_ = case type_ of
  "message" -> Just wsMessageChatMessageDecoder
  _ -> Nothing


wsMessageChatMessageDecoder : Json.Decoder WsMessageBody
wsMessageChatMessageDecoder = Json.object2 (\mes time -> WSChatMessage mes time)
  ("message" := Json.string)
  ("time" := Json.float)

-- Actions
type Action
  = NoOp
  | InitRoom API.InitialData
  | RTCAction WebRTC.Action
  | ChatViewAction ChatView.Action
  | WSAction WSMessage
  | StartStreaming (String, List PeerId)
  | EndStreaming (String, List PeerId)
  | FullScreen String

-- input
actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp


actionSignal : Signal Action
actionSignal = Signal.mergeMany [
  actions.signal
  , WSAction <~ constructedWsMessage
  , RTCAction <~ rtcMessage
  ]


update : Action -> Context -> Context
update action context =
    case log "action" action of
      NoOp -> context
      RTCAction event ->
        { context |
          rtc <- WebRTC.update event context.rtc
        }
      InitRoom initial ->
        let chat = context.chat
            newChat = { chat |
              myName <- initial.user.name
            }
        in { context |
          roomName <- initial.room.id,
          rtc <- WebRTC.update (WebRTC.InitRoom initial.room.peers initial.room.users initial.user) context.rtc,
          chat <- newChat
        }
      StartStreaming a -> { context |
          rtc <- WebRTC.update (WebRTC.StartStreaming a) context.rtc
        }
      EndStreaming a -> { context |
          rtc <- WebRTC.update (WebRTC.EndStreaming a) context.rtc
        }
      ChatViewAction action ->
        { context |
          chat <- ChatView.update action context.chat
        }
      WSAction (type_, peerId, Just (WSChatMessage s t)) ->
        let context1 = { context |
            chat <- ChatView.update (ChatView.AddMessage (peerId, s, Date.fromTime t)) context.chat
          }
        in { context1 |
            chat <- ChatView.update (ChatView.UpdateField "") context1.chat
          }
      _ -> context

-- Views

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
      ChatView.view (forwardTo c.address ChatViewAction) c.chat
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
      title = String.concat [user.name, "'s ", mediaType, " view."]
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
