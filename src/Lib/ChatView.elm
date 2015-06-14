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
type ChatViewEvent = ChatOpen | ChatClose | ChatUpdateField String | ChatSend String

onEnter : Address () -> Attribute
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

chatInput : Address ChatViewEvent -> String -> Html
chatInput address chatField = input [
    class "form-control",
    Html.Attributes.value chatField,
    on "input" targetValue (Signal.message address << ChatUpdateField),
    onEnter (forwardTo address (\_ -> ChatSend chatField))
  ] []

openedView : List ChatMessage -> Address ChatViewEvent -> String -> List Html
openedView chatMessages address chatField =
  [
    div [class "panel-heading row", onClick address ChatClose] [
      span [class "fa fa-comments"] [],
      text <| "Chat"
    ],
    div [class "panel-body"] [
      chatTimeline chatMessages
    ],
    div [class "row"] [
      chatInput address chatField
    ]
  ]

closedView : Int -> Address ChatViewEvent -> List Html
closedView noReadCount address =
  let noreadClass = if noReadCount > 0 then " noread" else ""
  in [
    div [
      class <| "panel-heading row" ++ noreadClass,
      onClick address ChatOpen
    ] [
      span [class "fa fa-comments"] [],
      text <| "Chat(" ++ (toString noReadCount) ++ ")"
    ]
  ]


chatView : List ChatMessage -> Address ChatViewEvent -> String -> Bool -> Int -> Html
chatView chatMessages address chatField opened noReadCount =
  let inner = if | opened    -> openedView chatMessages address chatField
                 | otherwise -> closedView noReadCount address
      openedClass = if opened then " opened" else ""
  in div [class "chat-view-container container"] [
    div [
      class <| "chat-view panel panel-default col-xs-12 col-sm-8 col-md-6" ++ openedClass
    ] inner
  ]
























--
