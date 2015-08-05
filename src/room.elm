module Main where

import Json.Decode as Json exposing ((:=))
import Json.Encode
import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src, classList, action, type', method, value)
import Html.Events exposing (..)
import Http

import Date exposing (Date)
import Time exposing (Time, every, second)
import String
import Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Signal exposing (..)
import Time exposing (..)

import Lib.API as API exposing (PeerId, User)
import Lib.Header as Header
import Lib.WebSocket as WS
import Lib.WebRTC as WebRTC
import Lib.VideoControl as VideoControl
import Lib.ChatView as ChatView
import Lib.Modal as Modal
import Lib.UserSearch as UserSearch

import Debug exposing (log)


-- Models

type alias Context =
    { selfPeerId: PeerId
    , roomName: String
    , address: Signal.Address Action
    , ws : WS.Model
    , rtc : WebRTC.Model
    , chat : ChatView.Model
    , modal : Modal.Model
    , userSearch : UserSearch.Model
    }
type alias MediaType = String
type alias Connection = (PeerId, MediaType, Int)

mediaTypes = ["mic", "video", "screen"]
initialContext : Context
initialContext =
  { selfPeerId = ""
  , roomName = ""
  , address = actions.address
  , ws = WS.init
  , rtc = WebRTC.init
  , chat = ChatView.init
  , modal = Modal.init
  , userSearch = UserSearch.init
  }

-- Data access

type Error
  = FetchError Http.Error
  | WSError WS.Error
  | ChatViewError ChatView.Error
  | RTCError WebRTC.Error
  | VideoControlError

fetchRoom : String -> Task Error API.InitialData
fetchRoom roomId = (API.getInitialData roomId) `onError` (\err -> fail <| FetchError err)

connectWebSocket : String -> Task Error ()
connectWebSocket url = (WS.connect url) `onError` (\e -> fail <| WSError e)

initializeRTC : PeerId -> API.InitialData -> Task Error ()
initializeRTC selfPeerId initial =
  WebRTC.doTask (WebRTC.Initialize selfPeerId initial.iceServers)
  `onError` (\e -> fail <| RTCError e)

initialize : String -> String -> String -> Task Error ()
initialize selfPeerId roomName wsURL =
  Task.map2 (\initial _ -> initial) (fetchRoom roomName) (connectWebSocket wsURL)
  `andThen` (\initial -> Task.map (\_ -> initial) (initializeRTC selfPeerId initial))
  `andThen` (\initial -> (Signal.send actions.address (InitRoom initial)))

port runner : Signal (PeerId, String, String)

taskRunner : Signal (Task Error ())
taskRunner = Signal.map (\(selfPeerId, roomName, wsURL) -> (Signal.send actions.address (Init selfPeerId roomName))
  `andThen` (\_ -> initialize selfPeerId roomName wsURL)) runner

port errorLogRunner : Signal (Task String ())
port errorLogRunner = Signal.map (\task -> task `onError` (\e -> fail (log "error: " <| errorLog e))) taskRunner

errorLog : Error -> String
errorLog e = case e of
  FetchError e -> "fetch error"
  WSError e -> WS.logError e
  RTCError e -> WebRTC.logError e
  VideoControlError -> "VideoControlError"
  ChatViewError e -> "ChatViewError"


port runTasks : Signal (Task Error ())
port runTasks =
  let f (time, action) c = case {-log "runTasks"-} action of
      InitRoom initial -> (WS.send <| joinToJson c.selfPeerId c.roomName) `onError` (\e -> fail <| WSError e)
      WSAction event ->
        case event of
          WS.Message s ->
            let
              decodedMessage = decode s
            in
              case decodedMessage of
                RTCMessage (WebRTC.Request x) ->
                  (WS.send <| signalToJson c.selfPeerId c.roomName x) `onError` (\e -> fail <| WSError e)
                RTCMessage x ->
                  WebRTC.doTask x `onError` (\e -> fail <| RTCError e)
                ChatMessage _ _ _ ->
                  ChatView.afterUpdate ChatView.ScrollDown `onError` (\e -> fail <| ChatViewError e)
                UndefinedMessage ->
                  Task.succeed ()
          _ ->
            Task.succeed ()
      RTCAction (WebRTC.Request x) -> (WS.send <| signalToJson c.selfPeerId c.roomName x) `onError` (\e -> fail <| WSError e)
      RTCAction x -> WebRTC.doTask x `onError` (\e -> fail <| RTCError e)
      ChatAction (ChatView.Send x) -> (WS.send <| messageToJson c.selfPeerId c.roomName x time) `onError` (\e -> fail <| WSError e)
      ChatAction (ChatView.Open) -> (ChatView.afterUpdate ChatView.ScrollDown) `onError` (\e -> fail <| ChatViewError e)
      ChatAction x -> ChatView.afterUpdate x `onError` (\e -> fail <| ChatViewError e)
      FullScreen x -> VideoControl.requestFullScreen x `onError` (\e -> fail VideoControlError)
      StartStreaming x -> WebRTC.doTask (WebRTC.StartStreaming x) `onError` (\e -> fail <| RTCError e)
      EndStreaming x -> WebRTC.doTask (WebRTC.EndStreaming x) `onError` (\e -> fail <| RTCError e)
      _ -> Task.succeed ()
  in Signal.map2 f (Time.timestamp actionSignal) context


signalToJson : String -> String -> (String, String, Json.Encode.Value) -> String
signalToJson selfId roomName (type_, to, data_) =
  let
    value = Json.Encode.object
      [ ("room", Json.Encode.string roomName)
      , ("from", Json.Encode.string selfId)
      , ("to", Json.Encode.string to)
      , ("type", Json.Encode.string type_)
      , ("data", data_)
      ]
  in Json.Encode.encode 0 value

messageToJson : String -> String -> String -> Time -> String
messageToJson selfId roomName mes time =
  let
    value = Json.Encode.object
      [ ("room", Json.Encode.string roomName)
      , ("from", Json.Encode.string selfId)
      , ("type", Json.Encode.string "message")
      , ("data", Json.Encode.object
          [ ("message", Json.Encode.string mes)
          , ("time", Json.Encode.float time)
          ])
      ]
  in Json.Encode.encode 0 value

joinToJson : String -> String -> String
joinToJson selfId roomName =
  let
    value = Json.Encode.object
      [ ("room", Json.Encode.string roomName)
      , ("from", Json.Encode.string selfId)
      , ("type", Json.Encode.string "join")
      ]
  in Json.Encode.encode 0 value


decodeChatMessage : String -> Maybe (PeerId, String, Time)
decodeChatMessage s = case (Json.decodeString chatMessagedecoder s) of
  Ok mes -> Just mes
  _ -> Nothing

chatMessagedecoder : Json.Decoder (PeerId, String, Time)
chatMessagedecoder = Json.object3 (\t f (mes, time) -> (f, mes, time))
    ("type" := Json.string)
    ("from" := Json.string)
    ("data" := messageDecoder)

messageDecoder : Json.Decoder (String, Time)
messageDecoder = Json.object2 (,)
  ("message" := Json.string)
  ("time" := Json.float)

context : Signal Context
context = Signal.foldp update initialContext actionSignal

userOf : Context -> PeerId -> User
userOf c peerId = case Dict.get peerId c.rtc.users of
  Just user -> user
  Nothing -> { name="", displayName="", image="", authority="" }

-- Action --

type Action
  = NoOp
  | WSAction WS.Action
  | RTCAction WebRTC.Action
  | ChatAction ChatView.Action
  | ModalAction Modal.Action
  | UserSearchAction UserSearch.Action
  | Init PeerId String
  | InitRoom API.InitialData
  | StartStreaming (String, List PeerId)
  | EndStreaming (String, List PeerId)
  | FullScreen String

type DecodedMessage
  = RTCMessage WebRTC.Action
  | ChatMessage PeerId String Time
  | UndefinedMessage

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

decode : String -> DecodedMessage
decode s = case WebRTC.decode s of
  Just a -> RTCMessage a
  Nothing -> case decodeChatMessage s of
    Just (peerId, mes, time) -> ChatMessage peerId mes time
    Nothing -> UndefinedMessage


actionSignal : Signal Action
actionSignal = Signal.mergeMany
  [ actions.signal
  , WSAction <~ WS.actions
  , RTCAction <~ WebRTC.actions (fps 25)
  ]


-- Update --

update : Action -> Context -> Context
update action context =
    case {-log "action"-} action of
      WSAction event ->
        let newContext =
          { context |
            ws <- WS.update event context.ws
          }
        in
          case event of
            WS.Message s ->
              let
                decodedMessage = decode s
              in
                case decodedMessage of
                  RTCMessage rtcMes ->
                    { newContext |
                      rtc <- WebRTC.update rtcMes newContext.rtc
                    }
                  ChatMessage peerId s time ->
                    { newContext |
                      chat <- ChatView.update
                                (ChatView.Message (userOf newContext peerId).displayName (userOf newContext peerId).image s time)
                                newContext.chat
                    }
            _ -> newContext
      RTCAction event ->
        { context |
          rtc <- WebRTC.update event context.rtc
        }
      Init selfPeerId roomName ->
        { context |
          selfPeerId <- selfPeerId,
          roomName <- roomName
        }
      InitRoom initial ->
        { context |
          rtc <- WebRTC.update (WebRTC.InitRoom initial.room.peers initial.room.users initial.user) context.rtc,
          chat <- ChatView.update (ChatView.MyName initial.user.displayName) context.chat
        }
      StartStreaming a ->
        { context |
          rtc <- WebRTC.update (WebRTC.StartStreaming a) context.rtc
        }
      EndStreaming a ->
        { context |
          rtc <- WebRTC.update (WebRTC.EndStreaming a) context.rtc
        }
      ChatAction action ->
        { context |
          chat <- ChatView.update action context.chat
        }
      ModalAction action ->
        { context |
          modal <- Modal.update action context.modal
        }
      UserSearchAction action ->
        let
          (newModel, maybeTask) = UserSearch.update action context.userSearch
          -- TODO use maybeTask
        in
          { context |
            userSearch <- newModel
          }
      _ -> context

-- View --

fullscreenButton : Address Action -> String -> Html
fullscreenButton address videoURL =
  div
    [ class "btn pull-right"
    , onClick address (FullScreen videoURL)
    ] [
      div [class "glyphicon glyphicon-fullscreen"] []
    ]

windowCloseButton : Context -> String -> Html
windowCloseButton c mediaType =
  div
    [ class "btn pull-right"
    , onClick c.address (EndStreaming (mediaType, (Set.toList c.rtc.peers)))
    ] [
      div [class "glyphicon glyphicon-remove"] []
    ]

windowHeader : String -> List Html -> Html
windowHeader title buttons =
  let buttonGroup = div [class "header-buttons btn-group pull-right"] buttons
  in div [class "panel-heading clearfix"] [(text title), buttonGroup]


view : Context -> Html
view c =
  div []
    [ Header.header { user = c.rtc.me, connected = c.ws.connected }
    , div [class "container"]
      [ statusView c
      , mainView c
      , ChatView.view (forwardTo c.address ChatAction) c.chat
      , Modal.view "Invite" (div [] [userSearchView c.address c]) (forwardTo c.address ModalAction) c.modal
      ]
    ]

window : Html -> Html -> Bool -> Bool -> Html
window header body local hidden =
  let
    face = if local then "panel-primary" else "panel-default"
    classes = [("hidden", hidden), ("col-sm-6", True), ("col-md-6", True)]
  in
    div [classList classes]
    [ div [class ("panel " ++ face)] [header, body]
    ]

roomTitle : Context -> Html
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
  in button
    [ Html.Attributes.type' "button"
    , class ("btn " ++ face)
    , onClick c.address action
    ] [madiaIcon mediaType]

mediaButtons : Address Action -> Context -> Html
mediaButtons address c = div
  [ Html.Attributes.attribute "role" "group"
  , class "btn-group"
  ] (List.map (madiaButton c) mediaTypes)

peerView : Address Action -> Context -> PeerId -> Html
peerView address c peer =
  let user = userOf c peer
  in li []
    [ div []
      [ -- i [class "fa fa-user"] [],
        img [src user.image] []
      , text user.displayName
      ]
    ]

peerViews : Address Action -> Context -> List PeerId -> Html
peerViews address c peers = ul
  [ class "user-list list-unstyled hidden-xs"
  ] (List.map (\peer -> peerView address c peer) peers)

statusView : Context -> Html
statusView c =
  let
    myVolume =
      case List.head (List.filter (\(peerId, _) -> peerId == c.selfPeerId) c.rtc.volumes) of
        Just (_, volume) -> volume
        Nothing -> 0
    myVolumeLog = round <| (logBase 1.14 (toFloat myVolume))
  in
    div [class "col-sm-3 col-md-3"]
      [ div [class "status-panel row panel panel-default"]
        [ div [class "panel-body"]
            [ roomTitle c
            , button
              [ class "btn btn-default"
              , onClick c.address (ModalAction Modal.open)
              ]
              [ text "Invite"]
            , div [] [text <| String.repeat (max (myVolumeLog - 5) 1) "|" ]
            , mediaButtons c.address c
            , peerViews c.address c (Set.toList c.rtc.peers)
            ]
        ]
      ]

mainView : Context -> Html
mainView c = div [class "col-sm-9 col-md-9"] [div [class "row"] (mediaViews c)]

mediaViews : Context -> List Html
mediaViews c =
  let
    localList = List.map (\mediaType -> localMediaWindowView c.address c mediaType) ["video", "screen"]
    remoteList = List.map (\connection -> remoteMediaWindowView c.address c connection) (Set.toList c.rtc.connections)
    both = List.concat [localList, remoteList]
    filterd = List.filter (\maybe -> case maybe of
        Just a -> True
        Nothing -> False
      ) both
  in
    List.map (\(Just a) -> a) filterd

localMediaWindowView : Address Action -> Context -> String -> Maybe Html
localMediaWindowView address c mediaType =
  let
    title = "Local " ++ mediaType
    maybeVideoUrl = Dict.get mediaType c.rtc.localVideoUrls
  in
    Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl True) maybeVideoUrl

remoteMediaWindowView : Address Action -> Context -> Connection -> Maybe Html
remoteMediaWindowView address c connection =
  let
    (peerId, mediaType, upstream) = connection
    user = userOf c peerId
    title = String.concat [user.displayName]
    maybeVideoUrl = Dict.get connection c.rtc.videoUrls
  in
    Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl False) maybeVideoUrl

mediaWindowView : Context -> String -> String -> String -> Bool -> Html
mediaWindowView c mediaType title videoUrl local =
  let
    attrs =
      [ src videoUrl
        , Html.Attributes.attribute "autoplay" ""
      ] ++ (if local then [Html.Attributes.attribute "muted" ""] else [])
    videoHtml =
      video attrs []
    buttons =
      if | local -> [windowCloseButton c mediaType, fullscreenButton c.address videoUrl]
         | otherwise -> [fullscreenButton c.address videoUrl]
    hidden = if mediaType == "mic" then True else False
  in window (windowHeader title buttons) videoHtml local hidden




userSearchView : Address Action -> Context -> Html
userSearchView address model =
  let
    (userSearchInput, userSearchHidden) =
      UserSearch.view (Signal.forwardTo address UserSearchAction) model.userSearch
    input_ = div [class "form-group"]
      [ label [] [text "Invite"]
      -- , text "@"
      , userSearchInput
      ]
      -- submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      -- form_ = Html.form [action ("/invite"), method "POST"] [input_, submit_]
    submit_ = Html.form
      [ action ("/api/invite")
      , method "POST"
      ]
      [ userSearchHidden
      , input [ type' "submit", class "btn btn-primary", value "Create" ] []
      ]
    form_ = div [] [input_, submit_]
  in div [] [form_]


-- Main
main : Signal Html
main = view <~ context


























--
