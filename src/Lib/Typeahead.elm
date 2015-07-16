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
  | KeyDown Int
  | Select Int
  | Focus
  | Blur
  | Blur2

type alias Model a =
  { field : String
  , toField : a -> String
  , toHtml : a -> Html
  , fetch : String -> Task () (List a) -- `onError` outside
  , options : List a
  , selected : Int
  , optionVisible : Bool
  }

type Error = Error String

init : String -> (a -> String) -> (a -> Html) -> (String -> Task () (List a)) -> (Model a)
init initialField toField toHtml fetch =
  { field = initialField
  , toField = toField
  , toHtml = toHtml
  , fetch = fetch
  , options = []
  , selected = -1
  , optionVisible = False
  }

-- Update

update : Action a -> Model a -> (Model a, Maybe (Task () (Action a)))
update action model =
  let
    newModel = case log "typeahead action" action of
      NoOp -> model
      UpdateField field -> { model |
        field <- field
      }
      Data options -> { model |
        options <- options
      }
      KeyDown key ->
        if (key == 13 && model.selected >= 0) then selectOption model model.selected else
          { model |
            selected <- updateSelected key (List.length model.options) model.selected
          , optionVisible <- True
          }
      Select index -> selectOption model index
      Focus -> { model |
        optionVisible <- True
      }
      Blur -> { model |
        selected <- -1
      }
      -- Blur2 -> { model |
      --   optionVisible <- False
      -- }
    task = case action of
      UpdateField field -> if field == "" then Nothing else Just <| Task.map Data (model.fetch field)
      -- Blur -> Just <| Task.sleep 1 `andThen` (\_ -> Task.succeed Blur2)
      _ -> Nothing
  in (newModel, task)

selectOption : Model a -> Int -> Model a
selectOption model index =
  { model |
    selected <- -1
  , optionVisible <- False
  , options <- case nth index model.options of
    Just obj -> [obj]
    Nothing -> []
  , field <- case nth index model.options of
    Just obj -> model.toField obj
    Nothing -> ""
  }

updateSelected : Int -> Int -> Int -> Int
updateSelected key length selected =
  let
    next = if key == 38 then selected - 1 else (if key == 40 then selected + 1 else selected)
    next' = if next <= -2 then length - 1 else (if next >= length then -1 else next)
  in next'

nth : Int -> List a -> Maybe a
nth index list = List.head (List.drop index list)

-- View

view : Address (Action a) -> Model a -> Html
view address model =
  let
    visible = model.optionVisible && List.length model.options > 0
  in
    div [class "typeahead dropdown"]
      [ input
        [ class "form-control"
        , onKeyDown address KeyDown
        , onFocus address Focus
        , onBlur address Blur
        , on "input" targetValue (Signal.message address << UpdateField)
        , value <| displayField model
        ] []
      , ul
          [ class "dropdown-menu"
          , style [("display", if visible then "block" else "none"), ("width", "100%")]
          ] (List.indexedMap (\index obj -> optionsView address model index obj) model.options)
      ]

optionsView : Address (Action a) -> Model a -> Int -> a -> Html
optionsView address model index obj =
  li
    [ class ("typeahead-option" ++ (if index == model.selected then " typeahead-selected" else ""))
    , onClick address (Select index)
    ] [
      model.toHtml obj
    ]

displayField : Model a -> String
displayField model =
  if model.selected == -1 then model.field else case nth model.selected model.options of
    Just obj -> model.toField obj
    Nothing -> model.field



--
