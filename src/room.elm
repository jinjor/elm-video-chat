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

import Lib.API exposing (..)
import Lib.Header as Header
import Lib.WebSocket as WS
import Lib.VideoControl as VideoControl
import Lib.ChatView exposing (..)

import Debug exposing (log)
-- Models

type alias Context = { me: User
                      , roomName:String
                      , address: Signal.Address Action
                      , peers: Set PeerId
                      , users: Dict PeerId User
                      , connections: Set Connection
                      , chatField: String
                      , chatMessages: List ChatMessage'
                      , videoUrls: Dict Connection String
                      , localVideoUrls: Dict String String
                      , localVideo: Bool
                      , localAudio: Bool
                      , localScreen: Bool
                      , chatOpened: Bool
                      , noReadCount: Int}

type alias ChatMessage' = (PeerId, String, Date)
type alias MediaType = String
type alias Connection = (PeerId, MediaType)
type alias RawWSMessage = (String, PeerId, String)
type alias WSMessage = (String, PeerId, Maybe WsMessageBody)
type WsMessageBody = WSJoin User | WSLeave | WSChatMessage String Time


nullInitialData : InitialData
nullInitialData = { room= {id="", peers=[], users= []}, user={name="", email=""}}

-- Data access

fetchRoom : String -> Task err ()
fetchRoom roomId = (getInitialData roomId)
    `andThen` (\initial -> (Signal.send actions.address (InitRoom (log "initial" initial)) `andThen` (\_ -> Signal.send initRoomMB.address initial)))
    `onError` (\err -> log "err" (succeed ()))

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



port processWS : Signal(Task x ())
port processWS =
  let f (type_, peerId, maybe) = case (log "WS" maybe) of
    Just (WSJoin user) -> (Signal.send beforeJoinMB.address peerId) `andThen` (\_ -> Signal.send actions.address (Join peerId user))
    Just (WSLeave) -> (Signal.send beforeLeaveMB.address peerId) `andThen` (\_ -> Signal.send actions.address (Leave peerId))
    Just (WSChatMessage s t) -> updateChat (peerId, s, Date.fromTime t)
    Nothing -> Signal.send actions.address NoOp
  in Signal.map f constructedWsMessage


updateChat : ChatMessage' -> Task x ()
updateChat mes = (Signal.send actions.address (AddChatMessage mes)) `andThen` (\_ -> Signal.send actions.address (UpdateField ""))


port wsmessage : Signal String
port wsmessage = WS.message

port wsopened : Signal Bool
port wsopened = WS.opened

port wssend : Signal String
port wssend' : Signal (Task () ())
port wssend' = WS.send <~ wssend


port beforeJoin : Signal String
port beforeJoin = beforeJoinMB.signal

port beforeLeave : Signal String
port beforeLeave = beforeLeaveMB.signal

port updateRoom : Signal String

port initRoom : Signal InitialData
port initRoom = initRoomMB.signal

port sendChat : Signal String
port sendChat =
  let f action = case action of
    SendChat mes -> Just mes
    _ -> Nothing
  in Signal.filterMap f "" actions.signal


port startStreaming : Signal (String, List PeerId)
port startStreaming = startStreamingMB.signal
port endStreaming : Signal (String, List PeerId)
port endStreaming = endStreamingMB.signal


port setVideoUrl : Signal (Connection, Maybe String)
videoUrlList : Signal (Dict Connection String)
videoUrlList =
  let f (conn, maybeUrl) dict = case maybeUrl of
    Just url -> Dict.insert conn url dict
    Nothing -> Dict.remove conn dict
  in foldp f Dict.empty setVideoUrl
port videoUrlList' : Signal (Task x ())
port videoUrlList' = Signal.map (\dict -> (Signal.send actions.address (UpdateVideoUrls dict))) videoUrlList


port setLocalVideoUrl : Signal (String, Maybe String)
localVideoUrlList : Signal (Dict String String)
localVideoUrlList =
  let f (mediaType, maybeUrl) dict = case maybeUrl of
    Just url -> Dict.insert mediaType url dict
    Nothing -> Dict.remove mediaType dict
  in foldp f Dict.empty setLocalVideoUrl
port localVideoUrlList' : Signal (Task x ())
port localVideoUrlList' = Signal.map (\dict -> (Signal.send actions.address (UpdateLocalVideoUrls dict))) localVideoUrlList

port addConnection : Signal Connection
port addConnection' : Signal (Task x ())
port addConnection' = Signal.map (\conn -> (Signal.send actions.address (AddConnection conn))) addConnection

port removeConnection : Signal Connection
port removeConnection' : Signal (Task x ())
port removeConnection' = Signal.map (\conn -> (Signal.send actions.address (RemoveConnection conn))) removeConnection


requestFullScreenMB : Signal.Mailbox String
requestFullScreenMB = Signal.mailbox ""

port requestFullScreen' : Signal (Task () ())
port requestFullScreen' = Signal.map VideoControl.requestFullScreen requestFullScreenMB.signal


-- Statics
mediaTypes = ["mic", "video", "screen"]

-- Signals

connection : String -> String -> Connection
connection peer mediaType =(peer, mediaType)



actionSignal : Signal Action
actionSignal =
  let f action = case action of
    SendChat _ -> False
    _ -> True
  in Signal.filter f NoOp actions.signal

context : Signal Context
context =
  let initial = { roomName = "　"
                  , me = {name="", email=""}
                  , address = actions.address
                  , peers = Set.empty
                  , users = Dict.empty
                  , connections = Set.empty
                  , chatMessages = []
                  , chatField = ""
                  , videoUrls = Dict.empty
                  , localVideoUrls = Dict.empty
                  , localVideo = False
                  , localAudio = False
                  , localScreen = False
                  , chatOpened = False
                  , noReadCount = 0}
  in Signal.foldp update initial actionSignal

userOf : Context -> PeerId -> User
userOf c peerId = case Dict.get peerId c.users of
  Just user -> user
  Nothing -> { name="", email="" }



wsMessageDecoder : Json.Decoder RawWSMessage
wsMessageDecoder = Json.object3 (\t f d -> (t, f, Json.Encode.encode 0 d))
  ("type" := Json.string)
  ("from" := Json.string)
  ("data" := Json.value)

wsMessageBodyDecoder : String -> Maybe (Json.Decoder WsMessageBody)
wsMessageBodyDecoder type_ = case type_ of
  "join" -> Just wsMessageJoinDecoder
  "leave" -> Just wsMessageLeaveDecoder
  "message" -> Just wsMessageChatMessageDecoder
  _ -> Nothing


wsMessageJoinDecoder : Json.Decoder WsMessageBody
wsMessageJoinDecoder =
  let userDecoder = Json.object3 (,,) ("id" := Json.string) ("name" := Json.string) ("email" := Json.string)
  in Json.object1 (\(id, name, email) -> WSJoin {name= name, email= email}) ("user" := userDecoder)

wsMessageLeaveDecoder : Json.Decoder WsMessageBody
wsMessageLeaveDecoder = Json.null WSLeave

wsMessageChatMessageDecoder : Json.Decoder WsMessageBody
wsMessageChatMessageDecoder = Json.object2 (\mes time -> WSChatMessage mes time)
  ("message" := Json.string)
  ("time" := Json.float)

-- Actions
type Action
  = NoOp
  | CloseWindow Connection
  | InitRoom InitialData
  | RemovePeer String
  | AddConnection Connection
  | RemoveConnection Connection
  | UpdateField String
  | AddChatMessage ChatMessage'
  | UpdateVideoUrls (Dict Connection String)
  | UpdateLocalVideoUrls (Dict String String)
  | Join PeerId User
  | Leave PeerId
  | SendChat String
  | ToggleChatView Bool

update : Action -> Context -> Context
update action context =
    case log "action" action of
      NoOp -> context
      CloseWindow target ->
        { context |
          connections <- Set.remove target context.connections
        }
      InitRoom initial ->
        { context |
          roomName <- initial.room.id,
          peers <- Set.fromList initial.room.peers,
          users <- Dict.fromList(initial.room.users),
          me <- initial.user
        }
      RemovePeer target ->
        { context |
          peers <- Set.remove target context.peers
        }
      AddConnection conn ->
        { context |
          connections <- Set.insert conn context.connections
        }
      RemoveConnection conn ->
        { context |
          connections <- Set.remove conn context.connections
        }
      UpdateField input ->
        { context |
          chatField <- input
        }
      AddChatMessage mes ->
        { context |
          chatMessages <- mes :: context.chatMessages,
          noReadCount <- if context.chatOpened then 0 else context.noReadCount + 1
        }
      UpdateVideoUrls videoUrls ->
        { context |
          videoUrls <- videoUrls
        }
      UpdateLocalVideoUrls videoUrls ->
        { context |
          localVideoUrls <- videoUrls
        }
      Join peerId user ->
        { context |
          peers <- Set.insert peerId context.peers,
          users <- Dict.insert peerId user context.users
        }
      Leave peerId ->
        { context |
          peers <- Set.remove peerId context.peers,
          users <- Dict.remove peerId context.users
        }
      ToggleChatView opened ->
        { context |
          chatOpened <- opened,
          noReadCount <- if opened then 0 else context.noReadCount
        }

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

initRoomMB : Signal.Mailbox InitialData
initRoomMB = Signal.mailbox nullInitialData


startStreamingMB : Signal.Mailbox (String, List PeerId)
startStreamingMB = Signal.mailbox ("", [])

endStreamingMB : Signal.Mailbox (String, List PeerId)
endStreamingMB = Signal.mailbox ("", [])

beforeJoinMB : Signal.Mailbox String
beforeJoinMB = Signal.mailbox ""

beforeLeaveMB : Signal.Mailbox String
beforeLeaveMB = Signal.mailbox ""



-- Views(no signals appears here)


fullscreenButton : String -> Html
fullscreenButton videoURL = div [
      class "btn pull-right",
      onClick requestFullScreenMB.address videoURL
    ] [
      div [class "glyphicon glyphicon-fullscreen"] []
    ]

windowCloseButton : Context -> String -> Html
windowCloseButton c mediaType = div [
      class "btn pull-right",
      onClick endStreamingMB.address (mediaType, (Set.toList c.peers))
    ] [
      div [class "glyphicon glyphicon-remove"] []
    ]

windowHeader : String -> List Html -> Html
windowHeader title buttons =
  let buttonGroup = div [class "btn-group pull-right"] buttons
  in div [class "panel-heading clearfix"] [(text title), buttonGroup]

----------------

view : Context -> Html
view c =
  let f e = case e of
        ChatOpen -> ToggleChatView True
        ChatClose -> ToggleChatView False
        ChatUpdateField field -> UpdateField field
        ChatSend field -> SendChat field
      address = forwardTo c.address f
      chatMessages = List.map (\(peer, mes, date) -> ((userOf c peer).name, mes, date, (userOf c peer).email == c.me.email)) c.chatMessages
  in div [] [
    Header.header {user= {name=c.me.name}},
    div [class "container"] [
      statusView c,
      mainView c,
      chatView chatMessages address c.chatField c.chatOpened c.noReadCount
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
      streaming = case Dict.get mediaType c.localVideoUrls of
        Just _ -> True
        Nothing -> False
      face = if streaming then "btn-primary" else "btn-default"
      address = if streaming then endStreamingMB.address else startStreamingMB.address
      peers = Set.toList c.peers
  in button [
    Html.Attributes.type' "button",
    class ("btn " ++ face),
    onClick address (mediaType, peers)
  ] [madiaIcon mediaType]

mediaButtons : Signal.Address Action -> Context -> Html
mediaButtons address c = div [
    Html.Attributes.attribute "role" "group", class "btn-group"
  ] (List.map (madiaButton c) mediaTypes)

peerView : Signal.Address Action -> Context -> PeerId -> Html
peerView address c peer =
  let user = userOf c peer
  in li [] [
    div [] [
      i [class "fa fa-user"] [],
      text user.name
      -- text peer
    ]
  ]

peerViews : Signal.Address Action -> Context -> List PeerId -> Html
peerViews address c peers = ul [
    class "user-list list-unstyled hidden-xs"
  ] (List.map (\peer -> peerView address c peer) peers)

statusView : Context -> Html
statusView c = div [class "col-sm-3 col-md-3"] [
    div [class "status-panel row panel panel-default"] [
      div [class "panel-body"] [
        roomTitle c,
        mediaButtons c.address c,
        peerViews c.address c (Set.toList c.peers)
      ]
    ]
  ]

mainView : Context -> Html
mainView c = div [class "col-sm-9 col-md-9"] [div [class "row"] (mediaViews c)]

mediaViews : Context -> List Html
mediaViews c =
  let localList = List.map (\mediaType -> localMediaWindowView c.address c mediaType) ["video", "screen"]
      remoteList = List.map (\connection -> remoteMediaWindowView c.address c connection) (Set.toList c.connections)
      both = List.concat [localList, remoteList]
      filterd = List.filter (\maybe -> case maybe of
          Just a -> True
          Nothing -> False
        ) both
  in List.map (\(Just a) -> a) filterd

localMediaWindowView : Signal.Address Action -> Context -> String -> Maybe Html
localMediaWindowView address c mediaType =
  let title = "Local " ++ mediaType
      maybeVideoUrl = Dict.get mediaType c.localVideoUrls
  in Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl True) maybeVideoUrl

remoteMediaWindowView : Signal.Address Action -> Context -> Connection -> Maybe Html
remoteMediaWindowView address c connection =
  let (peerId, mediaType) = connection
      user = userOf c peerId
      title = String.concat [user.name, "'s ", mediaType, " view."]
      maybeVideoUrl = Dict.get connection c.videoUrls
  in Maybe.map (\videoUrl -> mediaWindowView c mediaType title videoUrl False) maybeVideoUrl

mediaWindowView : Context -> String -> String -> String -> Bool -> Html
mediaWindowView c mediaType title videoUrl local =
  let videoHtml = video [
            src videoUrl,
            Html.Attributes.attribute "autoplay" ""
          ] []
      buttons = if | local -> [windowCloseButton c mediaType, fullscreenButton videoUrl]
                   | otherwise -> [fullscreenButton videoUrl]
  in window (windowHeader title buttons) videoHtml local


-- Main
main : Signal Html
main = view <~ context




























--
