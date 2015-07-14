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
import Dict exposing (Dict)

import Lib.API exposing (..)

type alias Context =
  { roomName : String
  , inviteName : String
  , rooms: List Room
  , me: User }

--- Model
initialContext : Context
initialContext =
  { roomName = ""
  , inviteName = ""
  , rooms = []
  , me = { name="", displayName="", image="" } }

context : Signal Context
context = Signal.foldp update initialContext actions.signal

port fetchRoom : Task Http.Error ()
port fetchRoom = getRooms
      `andThen` (\initData -> (Signal.send actions.address (Init initData)))
      `onError` (\err -> log "err" (succeed ()))

--- Action
type Action
  = NoOp
  | Init InitialRoomsData
  | UpdateRoomName String
  | UpdateInviteName String

update : Action -> Context -> Context
update action context =
    case action of
      Init initData -> { context |
        me <- initData.user,
        rooms <- initData.rooms
      }
      UpdateRoomName roomName -> { context |
        roomName <- roomName
      }
      UpdateInviteName inviteName -> { context |
        inviteName <- inviteName
      }
actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Context -> Html
view address c = div [] [
    Header.header { user = c.me, connected = True },
    div [ class "container" ] [
      ul [class "list-unstyled clearfix col-md-12"] (roomViews c)
    , createRoomView address c
    , hr [] []
    , inviteView address c
    ]
  ]

createRoomView : Address Action -> Context -> Html
createRoomView address c =
  let input_ = div [class "form-group"] [
        label [] [text "New Room"]
        , input [ name ""
          , class "form-control"
          , value c.roomName
          , on "input" targetValue (Signal.message address << UpdateRoomName)
        ] []
      ]
      submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      form_ = Html.form [class "form-inline", action ("/room/" ++ encodeURI(c.roomName)), method "GET"] [input_, submit_]
  in div [] [form_]


inviteView : Address Action -> Context -> Html
inviteView address c =
  let input_ = div [class "form-group"] [
        label [] [text "Invite"]
        , text "@"
        , input [ name "invited"
          , placeholder "Twitter ID"
          , class "form-control"
          , value c.inviteName
          , on "input" targetValue (Signal.message address << UpdateInviteName)
        ] []
      ]
      submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      form_ = Html.form [class "form-inline", action ("/invite"), method "POST"] [input_, submit_]
  in div [] [form_]


roomViews c = List.map roomView c.rooms

userOf : Dict PeerId User -> PeerId -> User
userOf d peerId = case Dict.get peerId d of
  Just user -> user
  Nothing -> { name = "", displayName = "", image = "" }

roomView : Room -> Html
roomView room =
  let usersDict = Dict.fromList room.users
      users = List.map (\peerId -> userOf usersDict peerId) room.peers
      header = div [class "panel-heading"] [
          a [href ("/room/" ++ room.id)] [
            div [class "room-name panel-title"] [ text room.id ]
          ]
        ]
      body = div [class "panel-body"] [
        ul [class "list-unstyled"] (List.map peerView users)
      ]
  in li [class "col-md-3 pull-left"] [
        div [class "panel panel-default"] [header, body]
      ]

peerView : User -> Html
peerView peer = li [] [text peer.name]


--- Main
main : Signal Html
main = Signal.map (\c -> view actions.address c) context
