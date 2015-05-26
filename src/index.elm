import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Debug exposing (log)
import Lib.Header as Header
import Lib.URI exposing(encodeURI, decodeURI)

type alias Room = { id:String, peers: List Peer}
type alias Peer = { id:String, name:String, mail:String }
type alias Context = { roomName : String, rooms: List Room }

--- Model
initialContext : Context
initialContext = { roomName = "", rooms = [] }

context : Signal Context
context = Signal.foldp update initialContext actions.signal


getRooms : Task Http.Error (List Room)
getRooms = Http.get (Json.list roomDecoder) "/api/rooms"


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

port fetchRoom : Task Http.Error ()
port fetchRoom = getRooms
      `andThen` (\rooms -> (Signal.send actions.address (Init rooms)))
      `onError` (\err -> log "err" (succeed ()))

--- Action
type Action
  = NoOp
  | Init (List Room)
  | UpdateRoomName String

update : Action -> Context -> Context
update action context =
    case action of
      Init rooms -> { context |
        rooms <- rooms
      }
      UpdateRoomName roomName -> { context |
        roomName <- roomName
      }
actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Context -> Html
view address c = div [] [
    Header.header {user= {name="ore"}},
    -- h2 [] [text "Rooms"],
    ul [class "list-unstyled clearfix col-md-12"] (roomViews c),
    createRoomView address c
  ]

createRoomView : Address Action -> Context ->Html
createRoomView address c =
  let input_ = div [class "form-group"] [
        label [] [text "New Room"],
        input [ name "",
          class "form-control",
          value c.roomName,
          on "input" targetValue (Signal.message address << UpdateRoomName)
        ] []
      ]
      submit_ = input [ type' "submit", class "btn btn-default", value "Create" ] []
      form_ = Html.form [class "form-inline", action ("/room/" ++ encodeURI(c.roomName)), method "GET"] [input_, submit_]
  in div [] [form_]

roomViews c = List.map roomView c.rooms

roomView room =
  let header = div [class "panel-heading"] [
          a [href ("/room/" ++ room.id)] [
            div [class "panel-title"] [ text room.id ]
          ]
        ]
      body = div [class "panel-body"] [
        ul [class "list-unstyled"] (List.map peerView room.peers)
      ]
  in li [class "col-md-3 pull-left"] [
        div [class "panel panel-default"] [header, body]
      ]

peerView peer = li [] [text peer.name]


--- Main
main : Signal Html
main = Signal.map (\c -> view actions.address c) context
