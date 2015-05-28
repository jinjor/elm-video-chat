module Main where

import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

import Graphics.Element exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)
import Mouse
import String
import Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Signal exposing (..)

import Lib.Header as Header

import Debug exposing (log)

-- Models
type alias Room = { id:String, peers: List PeerId, users: Dict PeerId User}
type alias User = { name:String, email:String }
type alias Context = { roomName:String
                      , address: Signal.Address Action
                      , peers: Set PeerId
                      , users: Dict PeerId User
                      , connections: Set Connection
                      , chatField: String
                      , chatMessages: List ChatMessage
                      , videoUrls: Dict Connection String
                      , localVideoUrls: Dict String String
                      , localVideo: Bool
                      , localAudio: Bool
                      , localScreen: Bool }

type alias PeerId = String
type alias MediaType = String
type alias Connection = (PeerId, MediaType)
type alias ChatMessage = { userId: String, message: String }
-- Data access

getRoomInfo : String -> Task Http.Error Room
getRoomInfo roomId = Http.get roomDecoder (log "url" ("/api/room/" ++ roomId))

roomDecoder : Json.Decoder Room
roomDecoder =
  let peer = Json.string
      user = Json.object2 (\name email -> { name=name, email=email })
          ("name" := Json.string)
          ("email" := Json.string)
  in
    Json.object3 (\id peers users -> { id=id, peers=peers, users=users })
      ("id" := Json.string)
      ("peers" := Json.list peer)
      ("users" := Json.dict user)

fetchRoom : String -> Task Http.Error ()
fetchRoom roomId = (getRoomInfo roomId) `andThen` (\room -> (Signal.send actions.address (InitRoom room))) `onError` (\err -> log "err" (succeed ()))

port runner : Signal (Task Http.Error ())
port runner = Signal.map fetchRoom updateRoom

port updateRoom : Signal String


port receiveChat : Signal ChatMessage
port updateChat : Signal (Task x ())
port updateChat = Signal.map (\mes -> (Signal.send actions.address (AddChatMessage mes)) `andThen` (\_ -> Signal.send actions.address (UpdateField ""))) receiveChat
port sendChat : Signal String
port sendChat = chatSendMB.signal
port startStreaming : Signal String
port startStreaming = startStreamingMB.signal
port endStreaming : Signal String
port endStreaming = endStreamingMB.signal


port join : Signal (PeerId, User)
port join' : Signal (Task x ())
port join' = Signal.map (\(peerId, user) -> (Signal.send actions.address (Join peerId user))) join

port leave : Signal PeerId
port leave' : Signal (Task x ())
port leave' = Signal.map (\peerId -> (Signal.send actions.address (Leave peerId))) leave

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


-- Statics
mediaTypes = ["mic", "video", "screen"]

-- Signals

connection : String -> String -> Connection
connection peer mediaType =(peer, mediaType)

context : Signal Context
context =
  let initial = { roomName= "　"
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
                  , localScreen = False}
  in Signal.foldp update initial actions.signal

userOf : Context -> PeerId -> User
userOf c peerId = case Dict.get peerId c.users of
  Just user -> user
  Nothing -> { name="", email="" }

-- Actions

type Action
  = NoOp
  | CloseWindow Connection
  | InitRoom Room
  | RemovePeer String
  | AddConnection Connection
  | RemoveConnection Connection
  | UpdateField String
  | AddChatMessage ChatMessage
  | UpdateVideoUrls (Dict Connection String)
  | UpdateLocalVideoUrls (Dict String String)
  | Join PeerId User
  | Leave PeerId

update : Action -> Context -> Context
update action context =
    case log "action" action of
      NoOp -> context
      CloseWindow target ->
        { context |
          connections <- Set.remove target context.connections
        }
      InitRoom room ->
        { context |
          roomName <- room.id,
          peers <- Set.fromList room.peers,
          users <- room.users
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
          chatMessages <- mes :: context.chatMessages
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


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

chatSendMB : Signal.Mailbox String
chatSendMB = Signal.mailbox ""

startStreamingMB : Signal.Mailbox String
startStreamingMB = Signal.mailbox ""

endStreamingMB : Signal.Mailbox String
endStreamingMB = Signal.mailbox ""


-- Views(no signals appears here)

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


fullscreenButton : Html
fullscreenButton = div [class "btn pull-right"] [
    div [class "glyphicon glyphicon-fullscreen"] []
  ]

windowCloseButton : String -> Html
windowCloseButton mediaType = div [class "btn pull-right", onClick endStreamingMB.address mediaType] [
    div [class "glyphicon glyphicon-remove"] []
  ]

windowHeader : String -> List Html -> Html
windowHeader title buttons =
  let buttonGroup = div [class "btn-group pull-right"] buttons
  in div [class "panel-heading clearfix"] [(text title), buttonGroup]

----------------
view : Context -> Html
view c = div [] [
    Header.header {user= {name="ore"}},
    div [class "container"] [
      statusView c,
      mainView c
      -- chatView c
    ]
  ]

window : Html -> Html -> Bool -> Html
window header body local =
  let face = if | local -> "panel-primary"
                | otherwise -> "panel-default"
  in div [class "col-sm-6 col-md-4"] [
        div [class ("panel " ++ face)] [header, body]
      ]

roomTitle c = h2 [] [text c.roomName]

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
      face = if | streaming -> "btn-primary"
                | otherwise -> "btn-default"
      address = if | streaming -> endStreamingMB.address
                   | otherwise -> startStreamingMB.address
  in button [
    Html.Attributes.type' "button",
    class ("btn " ++ face),
    onClick address mediaType
  ] [madiaIcon mediaType]

madiaButtons : Signal.Address Action -> Context -> Html
madiaButtons address c = div [ Html.Attributes.attribute "role" "group", class "btn-group"] (List.map (madiaButton c) mediaTypes)

peerView : Signal.Address Action -> Context -> PeerId -> Html
peerView address c peer =
  let user = userOf c peer
  in li [] [
    div [] [
      i [class "fa fa-user"] [],
      text user.name
    ]
  ]

peerViews : Signal.Address Action -> Context -> List PeerId -> Html
peerViews address c peers = ul [class "list-unstyled hidden-xs"] (List.map (\peer -> peerView address c peer) peers)

-- upperView : Context -> Html
-- upperView c = div [class "col-md-12"] [statusView c, mainView c]

statusView : Context -> Html
statusView c = div [class "col-sm-3 col-md-3"] [
    div [class "row panel panel-default"] [
      div [class "panel-body"] [
        roomTitle c,
        madiaButtons c.address c,
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
      buttons = if | local -> [windowCloseButton mediaType, fullscreenButton] --TODO
                   | otherwise -> [fullscreenButton] --TODO
  in window (windowHeader title buttons) videoHtml local

messageView : ChatMessage -> Html
messageView message = li [] [text message.message]

chatTimeline : List ChatMessage -> Html
chatTimeline messages = ul [class "list-unstyled"] (List.map messageView (List.reverse messages))

chatInput : Context -> Html
chatInput c = input [
  Html.Attributes.value c.chatField,
    on "input" targetValue (Signal.message c.address << UpdateField),
    onEnter chatSendMB.address c.chatField
  ] []

chatView : Context -> Html
chatView c = div [class "col-md-12"] [chatTimeline c.chatMessages, chatInput c]

-- Main
main : Signal Html
main = view <~ context






























--
