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

type alias Model =
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

mediaTypes : List MediaType
mediaTypes = ["mic", "video", "screen"]

initialContext : Model
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
  let f (time, action) model = case {-log "runTasks"-} action of
      InitRoom initial -> (WS.send <| joinToJson model.selfPeerId model.roomName) `onError` (\e -> fail <| WSError e)
      WSAction event ->
        case event of
          WS.Message s ->
            let
              decodedMessage = decode s
            in
              case decodedMessage of
                RTCMessage (WebRTC.Request x) ->
                  (WS.send <| signalToJson model.selfPeerId model.roomName x) `onError` (\e -> fail <| WSError e)
                RTCMessage x ->
                  WebRTC.doTask x `onError` (\e -> fail <| RTCError e)
                ChatMessage _ _ _ ->
                  ChatView.afterUpdate ChatView.ScrollDown `onError` (\e -> fail <| ChatViewError e)
                UndefinedMessage ->
                  Task.succeed ()
          _ ->
            Task.succeed ()
      RTCAction (WebRTC.Request x) -> (WS.send <| signalToJson model.selfPeerId model.roomName x) `onError` (\e -> fail <| WSError e)
      RTCAction x -> WebRTC.doTask x `onError` (\e -> fail <| RTCError e)
      ChatAction (ChatView.Send x) -> (WS.send <| messageToJson model.selfPeerId model.roomName x time) `onError` (\e -> fail <| WSError e)
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


context : Signal Model
context = Signal.map fst state


state : Signal (Model, Maybe (Task () Action))
state =
  Signal.foldp
    (\action (model, _) -> update action model)
    (initialContext, Nothing) actionSignal


port runState : Signal (Task () ())
port runState =
  Signal.map (\(_, maybeTask) -> case maybeTask of
      Just task -> task `andThen` (\action -> Signal.send actions.address action)
      Nothing -> Task.succeed ()
      ) state



userOf : Model -> PeerId -> User
userOf model peerId = case Dict.get peerId model.rtc.users of
  Just user -> user
  Nothing -> { name = "", displayName = "", image = "", authority = "" }

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

update : Action -> Model -> (Model, Maybe (Task () Action))
update action model =
    case {-log "action"-} action of
      WSAction event ->
        let newContext =
          { model |
            ws <- WS.update event model.ws
          }
        in
          case event of
            WS.Message s ->
              let
                decodedMessage = decode s
              in
                case decodedMessage of
                  RTCMessage rtcMes ->
                    (,) { newContext |
                      rtc <- WebRTC.update rtcMes newContext.rtc
                    } Nothing
                  ChatMessage peerId s time ->
                    (,) { newContext |
                      chat <- ChatView.update
                                (ChatView.Message (userOf newContext peerId).displayName (userOf newContext peerId).image s time)
                                newContext.chat
                    } Nothing
            _ -> (,) newContext Nothing
      RTCAction event ->
        (,) { model |
          rtc <- WebRTC.update event model.rtc
        } Nothing
      Init selfPeerId roomName ->
        (,) { model |
          selfPeerId <- selfPeerId,
          roomName <- roomName
        } Nothing
      InitRoom initial ->
        (,) { model |
          rtc <- WebRTC.update (WebRTC.InitRoom initial.room.peers initial.room.users initial.user) model.rtc,
          chat <- ChatView.update (ChatView.MyName initial.user.displayName) model.chat
        } Nothing
      StartStreaming a ->
        (,) { model |
          rtc <- WebRTC.update (WebRTC.StartStreaming a) model.rtc
        } Nothing
      EndStreaming a ->
        (,) { model |
          rtc <- WebRTC.update (WebRTC.EndStreaming a) model.rtc
        } Nothing
      ChatAction action ->
        (,) { model |
          chat <- ChatView.update action model.chat
        } Nothing
      ModalAction action ->
        (,) { model |
          modal <- Modal.update action model.modal
        } Nothing
      UserSearchAction action ->
        let
          (newModel, maybeTask) = UserSearch.update action model.userSearch
          task = Maybe.map (Task.map UserSearchAction) maybeTask
        in
          (,) { model |
            userSearch <- newModel
          } task
      _ -> (,) model Nothing

-- View --

fullscreenButton : Address Action -> String -> Html
fullscreenButton address videoURL =
  div
    [ class "btn pull-right"
    , onClick address (FullScreen videoURL)
    ] [
      div [class "glyphicon glyphicon-fullscreen"] []
    ]

windowCloseButton : Address Action -> Model -> String -> Html
windowCloseButton address model mediaType =
  div
    [ class "btn pull-right"
    , onClick address (EndStreaming (mediaType, (Set.toList model.rtc.peers)))
    ] [
      div [class "glyphicon glyphicon-remove"] []
    ]

windowHeader : String -> List Html -> Html
windowHeader title buttons =
  let buttonGroup = div [class "header-buttons btn-group pull-right"] buttons
  in div [class "panel-heading clearfix"] [(text title), buttonGroup]


view : Address Action -> Model -> Html
view address model =
  div []
    [ Header.header { user = model.rtc.me, connected = model.ws.connected }
    , div [class "container"]
      [ statusView address model
      , mainView address model
      , ChatView.view (forwardTo address ChatAction) model.chat
      , Modal.view "Invite" (div [] [userSearchView address model]) (forwardTo address ModalAction) model.modal
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

roomTitle : Model -> Html
roomTitle model = h2 [ class "room-name" ] [text model.roomName]

madiaIcon : String -> Html
madiaIcon mediaType =
  let classes = case mediaType of
      "video" -> "fa fa-video-camera"
      "mic" -> "fa fa-microphone"
      "screen" -> "fa fa-desktop"
  in i [class classes] []

madiaButton : Address Action -> Model -> String -> Html
madiaButton address model mediaType =
  let classes = case mediaType of
        "video" -> "fa fa-video-camera"
        "mic" -> "fa fa-microphone"
        "screen" -> "fa fa-desktop"
      streaming = case Dict.get mediaType model.rtc.localVideoUrls of
        Just _ -> True
        Nothing -> False
      face = if streaming then "btn-primary" else "btn-default"
      action = if streaming then EndStreaming (mediaType, peers) else StartStreaming (mediaType, peers)
      peers = Set.toList model.rtc.peers
  in button
    [ Html.Attributes.type' "button"
    , class ("btn " ++ face)
    , onClick address action
    ] [madiaIcon mediaType]

mediaButtons : Address Action -> Model -> Html
mediaButtons address model = div
  [ Html.Attributes.attribute "role" "group"
  , class "btn-group"
  ] (List.map (madiaButton address model) mediaTypes)

peerView : Address Action -> Model -> PeerId -> Html
peerView address model peer =
  let user = userOf model peer
  in li []
    [ div []
      [ -- i [class "fa fa-user"] [],
        img [src user.image] []
      , text user.displayName
      ]
    ]

peerViews : Address Action -> Model -> List PeerId -> Html
peerViews address model peers = ul
  [ class "user-list list-unstyled hidden-xs"
  ] (List.map (\peer -> peerView address model peer) peers)

statusView : Address Action -> Model -> Html
statusView address model =
  let
    myVolume =
      case List.head (List.filter (\(peerId, _) -> peerId == model.selfPeerId) model.rtc.volumes) of
        Just (_, volume) -> volume
        Nothing -> 0
    myVolumeLog = round <| (logBase 1.14 (toFloat myVolume))
  in
    div [class "col-sm-3 col-md-3"]
      [ div [class "status-panel row panel panel-default"]
        [ div [class "panel-body"]
            [ roomTitle model
            , button
              [ class "btn btn-default"
              , onClick address (ModalAction Modal.open)
              ]
              [ text "Invite"]
            , div [] [text <| String.repeat (max (myVolumeLog - 5) 1) "|" ]
            , mediaButtons address model
            , peerViews address model (Set.toList model.rtc.peers)
            ]
        ]
      ]

mainView : Address Action -> Model -> Html
mainView address model =
  div [class "col-sm-9 col-md-9"] [div [class "row"] (mediaViews address model)]

mediaViews : Address Action -> Model -> List Html
mediaViews address model =
  let
    localList = List.map (\mediaType -> localMediaWindowView address model mediaType) ["video", "screen"]
    remoteList = List.map (\connection -> remoteMediaWindowView address model connection) (Set.toList model.rtc.connections)
    both = List.concat [localList, remoteList]
    filterd = List.filter (\maybe -> case maybe of
        Just a -> True
        Nothing -> False
      ) both
  in
    List.map (\(Just a) -> a) filterd

localMediaWindowView : Address Action -> Model -> String -> Maybe Html
localMediaWindowView address model mediaType =
  let
    title = "Local " ++ mediaType
    maybeVideoUrl = Dict.get mediaType model.rtc.localVideoUrls
  in
    Maybe.map (\videoUrl -> mediaWindowView address model mediaType title videoUrl True) maybeVideoUrl

remoteMediaWindowView : Address Action -> Model -> Connection -> Maybe Html
remoteMediaWindowView address model connection =
  let
    (peerId, mediaType, upstream) = connection
    user = userOf model peerId
    title = String.concat [user.displayName]
    maybeVideoUrl = Dict.get connection model.rtc.videoUrls
  in
    Maybe.map (\videoUrl -> mediaWindowView address model mediaType title videoUrl False) maybeVideoUrl

mediaWindowView : Address Action -> Model -> String -> String -> String -> Bool -> Html
mediaWindowView address model mediaType title videoUrl local =
  let
    attrs =
      [ src videoUrl
        , Html.Attributes.attribute "autoplay" ""
      ] ++ (if local then [Html.Attributes.attribute "muted" ""] else [])
    videoHtml =
      video attrs []
    buttons =
      if | local -> [windowCloseButton address model mediaType, fullscreenButton address videoUrl]
         | otherwise -> [fullscreenButton address videoUrl]
    hidden = if mediaType == "mic" then True else False
  in window (windowHeader title buttons) videoHtml local hidden

userSearchView : Address Action -> Model -> Html
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
main = (view actions.address) <~ context


























--
