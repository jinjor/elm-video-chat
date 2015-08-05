module Lib.UserSearch (Model, init, Action(..), update, view) where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Task exposing (..)

import Debug exposing (log)
import Time exposing (Time)

import Lib.API exposing (..)
import Lib.Typeahead as Typeahead

type alias Model =
  { typeahead : Typeahead.Model User
  }

init : Model
init =
  { typeahead = Typeahead.init "" (\user -> user.name) userOptionToHtml fetchOptions
  }

type Action
  = TypeaheadAction (Typeahead.Action User)


update : Action -> Model -> (Model, Maybe (Task () Action))
update action model =
  case action of
    TypeaheadAction action ->
      let
        (newModel, maybeTask) = Typeahead.update action model.typeahead
      in
        (,) { model |
            typeahead <- newModel
          } (Maybe.map (Task.map TypeaheadAction) maybeTask)

view : Address Action -> Model -> (Html, Html)
view address model =
  let
    input' = Typeahead.view (Signal.forwardTo address TypeaheadAction) model.typeahead
    hidden' = hidden address model
  in
    (input', hidden')


hidden : Address Action -> Model -> Html
hidden address model =
  input [ type' "hidden", name "invited", value model.typeahead.field ] []


userOptionToHtml : User -> Html
userOptionToHtml user =
  div [class "twitter-user"]
    [ img [class "twitter-user-image", src user.image] []
    , span [class "twitter-user-name"] [text user.displayName]
    , span [class "twitter-user-account"] [text <| "@" ++ user.name]
    ]


fetchOptions : String -> Task () (List User)
fetchOptions q = searchUser q `onError` (\err -> log "err" (succeed []))

--
