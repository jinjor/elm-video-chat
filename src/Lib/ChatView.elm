module Lib.ChatView where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)
import Date exposing (Date)

import Lib.PanelHeader exposing (..)

-- Models

type alias Name = String
type alias ChatMessage = (Name, String, Date, Bool)

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
messageView (name, message, time, self) =
  let avator = img [src "", class "chat-avator"] []
      name_ = div [class "chat-name"] [text name]
      message_ = div [class "chat-message"] [ div [] [text message], time_]
      hour_ = toString <| Date.hour time
      minute_ = toString <| Date.minute time
      time_ = div [class "chat-time"] [text <| hour_ ++ ":" ++ minute_]
      clazz = if self then "chat chat-self" else "chat"
  in li [class clazz] [avator, name_, message_]

chatTimeline : List ChatMessage -> Html
chatTimeline messages =
  ul [
    class "list-unstyled"
  ] (List.map (\mes -> messageView mes) (List.reverse messages))

chatInput : Address String -> Address String -> String -> Html
chatInput inputAddress sendAddress chatField = input [
  Html.Attributes.value chatField,
    on "input" targetValue (Signal.message inputAddress),
    onEnter (forwardTo sendAddress (\_ -> chatField))
  ] []

chatView : List ChatMessage -> Address String -> Address String -> String -> Html
chatView chatMessages inputAddress sendAddress chatField =
  div [class "chat-view-container container"] [
    div [
      class "chat-view panel panel-default col-xs-12 col-sm-8 col-md-6"
    ] [
      div [class "panel-heading row"] [text "Chat"],
      div [class "panel-body"] [
        chatTimeline chatMessages
      ],
      div [class "row"] [
        chatInput inputAddress sendAddress chatField
      ]
    ]
  ]
























--
