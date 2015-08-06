import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Debug exposing (log)
import Dict exposing (Dict)

import Lib.API exposing (..)

type alias Model = { error : Maybe String }

--- Model
init : Model
init = { error = Nothing }

model : Signal Model
model = Signal.foldp update init actions.signal

--- Action
type Action
  = NoOp

update : Action -> Model -> Model
update action model =
  case action of
    _ -> model


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Model -> Html
view address model =
  div [class "login-wrapper"]
    [ div [class "panel panel-default center-block", style [("max-width", "500px")]]
      [ div [class "panel-heading"]
        [ h2 [class "panel-title"] [text "please login"]
        ]
      , div [class "panel-body"]
        [ Html.form [class "form-horizontal col-md-12", action "/oauth/twitter", method "GET"]
            [ h3 [] [text "Login by Twitter"]
            , input
              [class "login-submit btn btn-primary", type' "submit", value "Go"] []
            ]
        , Html.form [class "form-horizontal col-md-12", action "/api/login", method "POST"]
            [ h3 [] [text "Login as a guest"]
            , input [class "form-control", name "name", value "", placeholder "Your Name"] []
            , input [class "login-submit btn btn-primary", type' "submit", value "submit"] []
            ]
        ]
      ]
    ]

--- Main
main : Signal Html
main = Signal.map (view actions.address) model
