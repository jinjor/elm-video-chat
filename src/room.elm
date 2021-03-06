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
    , me : User
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
  , me = { name = "", displayName = "", image = "", authority = "" }
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
  | OtherError

fetchRoom : String -> Task Error API.InitialData
fetchRoom roomId = (API.getInitialData roomId) `onError` (\err -> fail <| FetchError err)

connectWebSocket : String -> Task Error ()
connectWebSocket url = (WS.connect url) `onError` (\e -> fail <| WSError e)

initializeRTC : PeerId -> API.InitialData -> Task Error ()
initializeRTC selfPeerId initial =
  WebRTC.initialize selfPeerId initial.iceServers `onError` (\e -> fail <| RTCError e)

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
errorLog e =
  case e of
    FetchError e -> "fetch error"
    WSError e -> WS.logError e
    RTCError e -> WebRTC.logError e
    VideoControlError -> "VideoControlError"
    ChatViewError e -> "ChatViewError"


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
decodeChatMessage s =
  case (Json.decodeString chatMessagedecoder s) of
    Ok mes -> Just mes
    _ -> Nothing

chatMessagedecoder : Json.Decoder (PeerId, String, Time)
chatMessagedecoder =
  Json.object3 (\t f (mes, time) -> (f, mes, time))
    ("type" := Json.string)
    ("from" := Json.string)
    ("data" := messageDecoder)

messageDecoder : Json.Decoder (String, Time)
messageDecoder =
  Json.object2 (,)
    ("message" := Json.string)
    ("time" := Json.float)


context : Signal Model
context = Signal.map fst state


state : Signal (Model, Maybe (Task Error ()))
state =
  Signal.foldp
    (\(time, action) (model, _) -> update time action model)
    (initialContext, Nothing) (Time.timestamp actionSignal)


port runState : Signal (Task Error ())
port runState =
  Signal.map (\(_, maybeTask) -> case maybeTask of
      Just task -> task
      Nothing -> Task.succeed ()
      ) state



userOf : Model -> PeerId -> User
userOf model peerId =
  case Dict.get peerId model.rtc.users of
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
  | SubmitInvite
  | EndInvitation

type DecodedMessage
  = RTCMessage WebRTC.Action
  | ChatMessage PeerId String Time
  | UndefinedMessage

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

decode : String -> DecodedMessage
decode s =
  case WebRTC.decode s of
    Just a -> RTCMessage a
    Nothing ->
      case decodeChatMessage s of
        Just (peerId, mes, time) -> ChatMessage peerId mes time
        Nothing -> UndefinedMessage


actionSignal : Signal Action
actionSignal =
  Signal.mergeMany
    [ actions.signal
    , WSAction <~ WS.actions
    , RTCAction <~ WebRTC.actions (fps 25)
    ]


-- Update --

update : Time -> Action -> Model -> (Model, Maybe (Task Error ()))
update time action model =
  let
    send x = (WS.send <| signalToJson model.selfPeerId model.roomName x) `onError` (\e -> Task.succeed ())
    sendMessage mes = (WS.send <| messageToJson model.selfPeerId model.roomName mes time) `onError` (\e -> fail <| WSError e)
  in
    case {-log "action"-} action of
      WSAction event ->
        let
          newContext =
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
                    let
                      (newRTC, maybeTask) = WebRTC.update send rtcMes newContext.rtc
                    in
                      (,) { newContext |
                        rtc <- newRTC
                      } (Maybe.map (\task -> task `onError` (\e -> fail <| RTCError e)) maybeTask)
                  ChatMessage peerId s time ->
                    let
                      (newModel, maybeTask) =
                        ChatView.update
                          sendMessage
                          (ChatView.Message (userOf newContext peerId).displayName (userOf newContext peerId).image s time)
                          newContext.chat
                    in
                      (,) { newContext |
                        chat <- newModel
                      } <| Maybe.map (\task -> task `onError` (\e -> fail <| ChatViewError e)) maybeTask
                  UndefinedMessage ->
                    (,) newContext Nothing
            _ -> (,) newContext Nothing
      RTCAction action ->
        let
          (newRTC, maybeTask) = WebRTC.update send action model.rtc
        in
          (,) { model |
            rtc <- newRTC
          } (Maybe.map (\task -> task `onError` (\e -> fail <| RTCError e)) maybeTask)
      Init selfPeerId roomName ->
        (,) { model |
          selfPeerId <- selfPeerId,
          roomName <- roomName
        } Nothing
      InitRoom initial ->
        (,) { model |
          me <- initial.user,
          rtc <- WebRTC.initRoom initial.room.peers initial.room.users initial.user model.rtc,
          chat <- ChatView.updateMyName initial.user.displayName model.chat
        } <| Just <| (WS.send <| joinToJson model.selfPeerId model.roomName) `onError` (\e -> fail <| WSError e)
      StartStreaming a ->
        let
          (newRTC, maybeTask) = WebRTC.update send (WebRTC.StartStreaming a) model.rtc
        in
          (,) { model |
            rtc <- newRTC
          } (Maybe.map (\task -> task `onError` (\e -> fail <| RTCError e)) maybeTask)
      EndStreaming a ->
        let
          (newRTC, maybeTask) = WebRTC.update send (WebRTC.EndStreaming a) model.rtc
        in
          (,) { model |
            rtc <- newRTC
          } (Maybe.map (\task -> task `onError` (\e -> fail <| RTCError e)) maybeTask)
      ChatAction action ->
        let
          (newModel, maybeTask) = ChatView.update sendMessage action model.chat
        in
          (,) { model |
            chat <- newModel
          } <| Maybe.map (\task -> task `onError` (\e -> fail <| ChatViewError e)) maybeTask
      ModalAction action ->
        (,) { model |
          modal <- Modal.update action model.modal
        } Nothing
      UserSearchAction action ->
        let
          (newModel, maybeTask) = UserSearch.update action model.userSearch
          maybeTask' = Maybe.map (\task -> task `andThen` (\a -> Signal.send actions.address <| UserSearchAction a) `onError` (\e -> fail OtherError)) maybeTask
        in
          (,) { model |
            userSearch <- newModel
          } maybeTask'
      EndInvitation ->
        (,) { model |
          modal <- Modal.init,
          userSearch <- UserSearch.init
        } Nothing
      FullScreen x ->
        (,) model <| Just <| VideoControl.requestFullScreen x `onError` (\e -> fail VideoControlError)
      SubmitInvite ->
        let task =
          API.postInvitation model.roomName (UserSearch.field model.userSearch)
            `onError` (\e -> fail <| FetchError e)
            `andThen` (\_ -> Signal.send actions.address EndInvitation)
        in
          (,) model <| Just task
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
  let
    classes =
      case mediaType of
        "video" -> "fa fa-video-camera"
        "mic" -> "fa fa-microphone"
        "screen" -> "fa fa-desktop"
    streaming =
      case Dict.get mediaType model.rtc.localVideoUrls of
        Just _ -> True
        Nothing -> False
    face = if streaming then "btn-primary" else "btn-default"
    action = if streaming then EndStreaming (mediaType, peers) else StartStreaming (mediaType, peers)
    peers = Set.toList model.rtc.peers
  in
    button
      [ Html.Attributes.type' "button"
      , class ("btn " ++ face)
      , onClick address action
      ] [madiaIcon mediaType]

mediaButtons : Address Action -> Model -> Html
mediaButtons address model =
  div
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
peerViews address model peers =
  ul
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
    invitationButton =
      if model.me.authority == "twitter"
        then [button
          [ class "btn btn-default"
          , onClick address (ModalAction Modal.open)
          ]
          [ text "Invite"]]
        else []
  in
    div [class "col-sm-3 col-md-3"]
      [ div [class "status-panel row panel panel-default"]
        [ div [class "panel-body"]
            ([ roomTitle model ] ++
            invitationButton ++
            [ div [] [text <| String.repeat (max (myVolumeLog - 5) 1) "|" ]
            , mediaButtons address model
            , peerViews address model (Set.toList model.rtc.peers)
            ])
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
      -- , input [ type' "submit", class "btn btn-primary", value "Create" ] []
      , input
        [ class "btn btn-primary"
        , value "Invite"
        , onClick address SubmitInvite
        ] []
      ]
    form_ = div [] [input_, submit_]
  in div [] [form_]


-- Main
main : Signal Html
main = (view actions.address) <~ context


























--
