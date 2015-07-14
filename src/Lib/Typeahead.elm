module Lib.Typeahead (Model, init, Action(..), update, view, Error) where

import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)

-- Models

type Action a
  = NoOp
  | UpdateField String
  | Data (List a)
  | Cursor Int
  | Select Int
  | Blur

type alias Model a = {
  field : String,
  toField : a -> String,
  toHtml : a -> Html,
  fetch : String -> Task () (List a), -- `onError` outside
  options : List a,
  selected : Int
}

type Error = Error String

init : String -> (a -> String) -> (a -> Html) -> (String -> Task () (List a)) -> (Model a)
init initialField toField toHtml fetch = {
    field = initialField,
    toField = toField,
    toHtml = toHtml,
    fetch = fetch,
    options = [],
    selected = -1
  }


-- Update

update : Action a -> Model a -> (Model a, Task () (Action a))
update action model =
  let
    newModel = case action of
      NoOp -> model
      UpdateField field -> { model |
        field <- field
      }
      Data options -> { model |
        options <- options
      }
      Cursor index -> { model |
        selected <- index
      }
      Select index -> { model |
        selected <- -1,
        options <- [],
        field <- case List.head (List.drop model.selected model.options) of
          Just obj -> model.toField obj
          Nothing -> ""
      }
      Blur -> { model |
        selected <- -1,
        options <- []
      }
    task = case action of
      UpdateField field -> Task.map Data (model.fetch field)
      _ -> Task.succeed NoOp
  in (newModel, task)


-- View

view : Address (Action a) -> Model a -> Html
view address model = div [] [
    input [
      on "input" targetValue (Signal.message address << UpdateField),
      value model.field
    ] [],
    ul [] (List.indexedMap (\index obj -> optionsView address model index obj) model.options)
  ]

optionsView : Address (Action a) -> Model a -> Int -> a -> Html
optionsView address model index obj =
  li [
    class (if index == model.selected then "selected" else ""),
    onClick address (Select index)
  ] [
    model.toHtml obj
  ]





--
