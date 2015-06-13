module Lib.ChatView where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)
import Time exposing (Time)

-- Models

type alias Name = String
type alias ChatMessage = (Name, String, Time)

onEnter : Signal.Address () -> Attribute
onEnter address =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address ())

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Views(no signals appears here)
messageView : ChatMessage -> Bool -> Html
messageView (name, message, time) self =
  let avator = img [src "", class "avator"] []
      name_ = div [class "chat-name"] [text name]
      message_ = div [class "message"] []
      hour_ = toString <| Time.inHours time
      minute_ = toString <| Time.inMinutes time
      time_ = div [class "time"] [text <| hour_ ++ ":" ++ minute_]
      clazz = if self then "chat chat-self" else "chat"
  in li [class clazz] [avator, name_, message_, time_]

chatTimeline : List ChatMessage -> Html
chatTimeline messages =
  let isSelf (name, _, _) = name == ""
  in ul [
    class "list-unstyled"
  ] (List.map (\mes -> messageView mes (isSelf mes)) (List.reverse messages))

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
