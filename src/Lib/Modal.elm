module Lib.Modal (Model, init, Action(..), update, view, open) where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)
import Time exposing (Time)

type alias Model =
  { show : Bool
  }

init : Model
init =
  { show = False
  }

type Action
  = Open
  | Close

open : Action
open = Open

update : Action -> Model -> Model
update action model =
  case action of
    Open -> { model | show <- True }
    Close -> { model | show <- False }

view : String -> Html -> Address Action -> Model -> Html
view title body address model =
  let
    header = div [class "panel-heading"] []
    styles = if model.show then [("display", "block")] else []
  in
    div
      [ class "modal in", style styles ]
      [ div
          [ class "modal-dialog"]
          [ div
              [class "modal-content"]
              [ div
                  [class "modal-header"]
                  [ button
                      [ type' "button"
                      , class "close"
                      , onClick address Close
                      ]
                      [ span [] [text "x"]
                      , span [class "sr-only"] [text "Close"]
                      ]
                  , h4 [class "modal-title"] [text title]
                  ]
              , div
                  [ class "modal-body"]
                  [ body ]
              -- , div
              --     [ class "modal-footer"]
              --     []
              ]
          ]
      ]

--
