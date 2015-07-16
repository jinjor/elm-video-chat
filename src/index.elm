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
  , typeahead : Typeahead.Model User
  , rooms: List Room
  , me: User
  }

--- Model
initialContext : Model
initialContext =
  { roomName = ""
  , typeahead = Typeahead.init "" (\user -> user.name) userOptionToHtml fetchOptions
  , rooms = []
  , me = { name="", displayName="", image="", authority = "" }
  }

-- context : Signal Context
-- context = Signal.foldp update initialContext actions.signal

state : Signal (Model, Maybe (Task () ()))
state = Signal.foldp (\action (model, _) -> update action model)
                (initialContext, Just fetchRoom) actions.signal


port runState : Signal (Task () ())
port runState = Signal.map (\(_, maybeTask) -> case maybeTask of
      Just task -> task
      Nothing -> Task.succeed ()
      ) state


fetchRoom : Task () ()
fetchRoom = getRooms
      `andThen` (\initData -> (Signal.send actions.address (Init initData)))
      `onError` (\err -> log "err" (succeed ()))

-- TODO
fetchOptions : String -> Task () (List User)
fetchOptions q = searchUser q `onError` (\err -> log "err" (succeed []))



--- Action
type Action
  = NoOp
  | Init InitialRoomsData
  | UpdateRoomName String
  | TypeaheadAction (Typeahead.Action User)


update : Action -> Model -> (Model, Maybe (Task () ()))
update action model =
    case action of
      NoOp -> (model, Nothing)
      Init initData -> (,) { model |
        me <- log "initData" initData.user,
        rooms <- initData.rooms
      } Nothing
      UpdateRoomName roomName -> (,) { model |
        roomName <- roomName
      } Nothing
      TypeaheadAction action ->
        let (newModel, maybeTask) = Typeahead.update action model.typeahead
        in (,) { model |
              typeahead <- newModel
            } (Maybe.map (\task -> task  `andThen` (\action -> Signal.send actions.address (TypeaheadAction action))) maybeTask)

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Model -> Html
view address model =
  let
    inviteView =
      [ hr [] []
      , typeaheadView address model
      ]
  in
    div []
      [ Header.header { user = model.me, connected = True }
      , div [ class "container" ]
        ([ ul [class "list-unstyled clearfix col-md-12"] (roomViews model)
        , createRoomView address model
        ] ++ (if model.me.authority == "twitter" then inviteView else []))
      ]


typeaheadView : Address Action -> Model -> Html
typeaheadView address model =
  let
    input_ = div [class "form-group"]
      [ label [] [text "Invite"]
      -- , text "@"
      , Typeahead.view (Signal.forwardTo address TypeaheadAction) model.typeahead
      ]
      -- submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
      -- form_ = Html.form [action ("/invite"), method "POST"] [input_, submit_]

    submit_ = Html.form
      [ action ("/invite")
      , method "POST"
      ]
      [ input [ type' "hidden", name "invited", value model.typeahead.field ] []
      , input [ type' "submit", class "btn btn-primary", value "Create" ] []
      ]
    form_ = div [] [input_, submit_]

  in div [] [form_]

createRoomView : Address Action -> Model -> Html
createRoomView address model =
  let
    input_ = div [class "form-group"]
      [ label [] [text "New Room"]
      , input
        [ name ""
        , class "form-control"
        , value model.roomName
        , on "input" targetValue (Signal.message address << UpdateRoomName)
        ] []
      ]
    submit_ = input [ type' "submit", class "btn btn-primary", value "Create" ] []
    form_ = Html.form
      [ class "form-inline"
      , action ("/room/" ++ encodeURI(model.roomName))
      , method "GET"
      ] [input_, submit_]
  in div [] [form_]

roomViews : Model -> List Html
roomViews model = List.map roomView model.rooms

userOf : Dict PeerId User -> PeerId -> User
userOf d peerId = case Dict.get peerId d of
  Just user -> user
  Nothing -> { name = "", displayName = "", image = "", authority = "" }

roomView : Room -> Html
roomView room =
  let usersDict = Dict.fromList room.users
      users = List.map (\peerId -> userOf usersDict peerId) room.peers
      header = div [class "panel-heading"]
        [ a [href ("/room/" ++ room.id)]
          [ div [class "room-name panel-title"] [ text room.id ]
          ]
        ]
      body = div [class "panel-body"]
        [ ul [class "list-unstyled"] (List.map peerView users)
        ]
  in li [class "col-md-3 pull-left"]
      [ div [class "panel panel-default"] [header, body]
      ]

peerView : User -> Html
peerView peer = li [] [text peer.name]

userOptionToHtml : User -> Html
userOptionToHtml user =
  div [class "twitter-user"]
    [ img [class "twitter-user-image", src user.image] []
    , span [class "twitter-user-name"] [text user.displayName]
    , span [class "twitter-user-account"] [text <| "@" ++ user.name]
    ]


--- Main
main : Signal Html
main = Signal.map (view actions.address << fst) state
