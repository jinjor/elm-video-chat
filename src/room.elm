module Main where

import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (..)

import String
import Maybe
import Dict exposing (Dict)
import Signal exposing (..)

import Lib.Header as Header

import Debug exposing (log)

-- Models
type alias Room = { id:String, peers: List Peer}
type alias Peer = { id:String, name:String, mail:String }
type alias Context = { roomName:String
                      , address: Signal.Address Action
                      , peers: Dict String Peer
                      , connections: List Connection
                      , chatField: String
                      , chatMessages: List ChatMessage
                      , videoUrls: Dict Connection String}
type alias PeerId = String
type alias MediaType = String
type alias Connection = (PeerId, MediaType)
type alias ChatMessage = { userId: String, message: String }
-- Data access

getRoomInfo : String -> Task Http.Error Room
getRoomInfo roomId = Http.get roomDecoder (log "url" ("/api/room/" ++ roomId))

roomDecoder : Json.Decoder Room
roomDecoder =
  let peer =
        Json.object3 (\id name mail -> { id=id, name=name, mail=mail })
          ("id" := Json.string)
          ("name" := Json.string)
          ("mail" := Json.string)
  in
    Json.object2 (\id peers -> { id=id, peers=peers })
      ("id" := Json.string)
      ("peers" := Json.list peer)

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

port videoUrlList : Signal (List (Connection, String))
port updateVideoUrlList : Signal (Task x ())
port updateVideoUrlList = Signal.map (\list -> (Signal.send actions.address (UpdateVideoUrls (Dict.fromList list)))) videoUrlList

port addConnection : Signal Connection
port addConnection' : Signal (Task x ())
port addConnection' = Signal.map (\conn -> (Signal.send actions.address (AddConnection conn))) addConnection


-- Statics
mediaTypes = ["mic", "video", "screen"]

-- Signals

connection : String -> String -> Connection
connection peer mediaType =(peer, mediaType)

context : Signal Context
context =
  let initial = { roomName= "　"
                  , address = actions.address
                  , peers = Dict.empty
                  , connections = []
                  , chatMessages = []
                  , chatField = ""
                  , videoUrls = Dict.empty}
  in Signal.foldp update initial actions.signal

peerOf : Context -> String -> Peer
peerOf c peerId = case Dict.get peerId c.peers of
  Just peer -> peer
  Nothing -> { id="", name="", mail="" }

-- Actions

type Action
  = NoOp
  | CloseWindow Connection
  | InitRoom Room
  | RemovePeer String
  | AddConnection Connection
  | UpdateField String
  | AddChatMessage ChatMessage
  | UpdateVideoUrls (Dict Connection String)

update : Action -> Context -> Context
update action context =
    case log "action" action of
      NoOp -> context
      CloseWindow target ->
        { context |
          connections <- List.filter ((/=) target) context.connections
        }
      InitRoom room ->
        { context |
          roomName <- room.id,
          peers <- Dict.fromList (List.map (\p -> (p.id, p)) room.peers)
        }
      RemovePeer target ->
        { context |
          peers <- Dict.remove target context.peers
        }
      AddConnection conn ->
        { context |
          connections <- conn :: context.connections
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

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

chatSendMB : Signal.Mailbox String
chatSendMB = Signal.mailbox ""

startStreamingMB : Signal.Mailbox String
startStreamingMB = Signal.mailbox ""


-- Views(no signals appears here)

onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"




fullscreenButton : Signal.Address a -> a -> Html
fullscreenButton address event = div [class "btn pull-right", onClick address event] [
    div [class "glyphicon glyphicon-fullscreen"] []
  ]

windowCloseButton : Signal.Address a -> a -> Html
windowCloseButton address event = div [class "btn pull-right", onClick address event] [
    div [class "glyphicon glyphicon-remove"] []
  ]

windowHeader : String -> List Html -> Html
windowHeader title buttons = div [class "panel-heading row"] ((text title) :: buttons)

modal : String -> Signal.Address a -> a -> List Html -> Html
modal title address event contents =
  let buttons = [windowCloseButton address event, fullscreenButton address event] --TODO
  in window (windowHeader title buttons) contents

window : Html -> List Html -> Html
window header contents =
  let body = div [class "panel-body row"] contents
  in div [class "panel panel-default col-md-4"] [header, body]

----------------

view : Context -> Html
view c = div [] [
    Header.header {user= {name="ore"}},
    upperView c, chatView c
  ]

roomTitle c = h2 [] [text c.roomName]

madiaIcon : String -> Html
madiaIcon mediaType =
  let classes = case mediaType of
      "video" -> "fa fa-video-camera"
      "mic" -> "fa fa-microphone"
      "screen" -> "fa fa-desktop"
  in i [class classes] []

madiaButton : Signal.Address Action -> String -> Html
madiaButton address mediaType =
  let classes = case mediaType of
      "video" -> "fa fa-video-camera"
      "mic" -> "fa fa-microphone"
      "screen" -> "fa fa-desktop"
  in button [
    Html.Attributes.type' "button",
    class "btn btn-default",
    onClick startStreamingMB.address mediaType
  ] [madiaIcon mediaType]

madiaButtons : Signal.Address Action -> Html
madiaButtons address = div [ Html.Attributes.attribute "role" "group", class "btn-group"] (List.map (madiaButton address) mediaTypes)

peerView : Signal.Address Action -> Peer -> Html
peerView address peer = li [] [
    div [] [
      i [class "fa fa-user"] [],
      text peer.name
    ]
  ]

peerViews : Signal.Address Action -> List Peer -> Html
peerViews address peers = ul [class "list-unstyled"] (List.map (\peer -> peerView address peer) peers)

upperView : Context -> Html
upperView c = div [class "col-md-12"] [statusView c, mainView c]

statusView : Context -> Html
statusView c = div [class "col-md-3"] [
    roomTitle c,
    madiaButtons c.address,
    peerViews c.address (Dict.values c.peers)
  ]

mainView : Context -> Html
mainView c = div [class "col-md-9"] [div [class "row"] (mediaViews c)]

mediaViews : Context -> List Html
mediaViews c = List.map (\connection -> mediaWindowView c.address c connection) c.connections

mediaWindowView : Signal.Address Action -> Context -> Connection -> Html
mediaWindowView address c connection =
  let (peerId, mediaType) = connection
      peer = peerOf c peerId
      title = String.concat [peer.name, "'s ", mediaType, " view."]
      videoMaybe = Maybe.map (\url -> video [src url, Html.Attributes.attribute "autoplay" ""] []) (Dict.get connection c.videoUrls)
      videoHtml = case videoMaybe of
        Just html -> [html]
        Nothing -> []
  in modal title address (CloseWindow connection) videoHtml

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
