module Lib.ChatView where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)

-- Models

type alias Name = String
type alias ChatMessage = (Name, String)

onEnter : Signal.Address () -> Attribute
onEnter address =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address ())

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Views(no signals appears here)
messageView : ChatMessage -> Html
messageView (_, message) = li [] [text message]

chatTimeline : List ChatMessage -> Html
chatTimeline messages = ul [
    class "list-unstyled"
  ] (List.map messageView (List.reverse messages))

chatInput : Address String -> Address String -> String -> Html
chatInput inputAddress sendAddress chatField = input [
  Html.Attributes.value chatField,
    on "input" targetValue (Signal.message inputAddress),
    onEnter (forwardTo sendAddress (\_ -> chatField))
  ] []

chatView : List ChatMessage -> Address String -> Address String -> String -> Html
chatView chatMessages inputAddress sendAddress chatField = div [
    class "col-md-12"
  ] [chatTimeline chatMessages, chatInput inputAddress sendAddress chatField]























--
