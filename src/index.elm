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
import Lib.Typeahead as Typeahead


type alias Model =
  { roomName : String
  , inviteName : String
  , typeahead : Typeahead.Model User
  , rooms: List Room
  , me: User }

--- Model
initialContext : Model
initialContext =
  { roomName = ""
  , inviteName = ""
  , typeahead = Typeahead.init "" (\user -> user.name) userOptionToHtml fetchOptions
  , rooms = []
  , me = { name="", displayName="", image="" } }

-- context : Signal Context
-- context = Signal.foldp update initialContext actions.signal

state : Signal (Model, Task () Action)
state = Signal.foldp (\action (model, _) -> update action model)
                (initialContext, Task.succeed NoOp) actions.signal


port runState : Signal (Task () ())
port runState = Signal.map (\(_, task) -> task `andThen` (\action -> Signal.send actions.address action)) state




port fetchRoom : Task Http.Error ()
port fetchRoom = getRooms
      `andThen` (\initData -> (Signal.send actions.address (Init initData)))
      `onError` (\err -> log "err" (succeed ()))

-- TODO
fetchOptions : String -> Task () (List User)
fetchOptions s = Task.succeed [{
  name = "456"
  , displayName = "123"
  , image = ""
  }, {
  name = s ++ s
  , displayName = s ++ s
  , image = ""
  }]



--- Action
type Action
  = NoOp
  | Init InitialRoomsData
  | UpdateRoomName String
  | UpdateInviteName String
  | TypeaheadAction (Typeahead.Action User)

noTask : Task () Action
noTask = Task.succeed NoOp

update : Action -> Model -> (Model, Task () Action)
update action model =
    case action of
      NoOp -> (model, noTask)
      Init initData -> (,) { model |
        me <- initData.user,
        rooms <- initData.rooms
      } noTask
      UpdateRoomName roomName -> (,) { model |
        roomName <- roomName
      } noTask
      UpdateInviteName inviteName -> (,) { model |
        inviteName <- inviteName
      } noTask
      TypeaheadAction action ->
        let (newModel, task) = Typeahead.update action model.typeahead
        in (,) { model |
              typeahead <- newModel
            } (Task.map TypeaheadAction task)

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Model -> Html
view address model = div [] [
    Header.header { user = model.me, connected = True },
    div [ class "container" ] [
      ul [class "list-unstyled clearfix col-md-12"] (roomViews model)
    , createRoomView address model
    , hr [] []
    , inviteView address model
    , typeaheadView address model
    ]
  ]


typeaheadView : Address Action -> Model -> Html
typeaheadView address model =
  let input_ = div [class "form-group"] [
        label [] [text "Invite"]
        , text "@"
        , Typeahead.view (Signal.forwardTo address TypeaheadAction) model.typeahead
      ]
      submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      form_ = div [action ("/invite"), method "POST"] [input_, submit_]
  in div [] [form_]

inviteView : Address Action -> Model -> Html
inviteView address model =
  let input_ = div [class "form-group"] [
        label [] [text "Invite"]
        , text "@"
        , input [ name "invited"
          , placeholder "Twitter ID"
          , class "form-control"
          , value model.inviteName
          , on "input" targetValue (Signal.message address << UpdateInviteName)
        ] []
      ]
      submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      form_ = Html.form [class "form-inline", action ("/invite"), method "POST"] [input_, submit_]
  in div [] [form_]



createRoomView : Address Action -> Model -> Html
createRoomView address model =
  let input_ = div [class "form-group"] [
        label [] [text "New Room"]
        , input [ name ""
          , class "form-control"
          , value model.roomName
          , on "input" targetValue (Signal.message address << UpdateRoomName)
        ] []
      ]
      submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      form_ = Html.form [class "form-inline", action ("/room/" ++ encodeURI(model.roomName)), method "GET"] [input_, submit_]
  in div [] [form_]

roomViews : Model -> List Html
roomViews model = List.map roomView model.rooms

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

userOptionToHtml : User -> Html
userOptionToHtml user = text user.name


--- Main
main : Signal Html
main = Signal.map (view actions.address << fst) state
