module Lib.ChatView (Model, init, Action(UpdateField, AddMessage, Send), update, view, ChatMessage) where

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
type alias ChatMessage = (Name, String, Date)
type Action = Open | Close | UpdateField String | Send String | AddMessage ChatMessage
type alias Model = {
  opened : Bool,
  messages : List ChatMessage,
  field : String,
  noReadCount : Int,
  myName : String
}

init : Model
init = {
    opened = False,
    messages = [],
    field = "",
    noReadCount = 0,
    myName = ""
  }


update : Action -> Model -> Model
update action model = case action of
  Open -> { model |
    opened <- True,
    noReadCount <- 0
  }
  Close -> { model |
    opened <- False
  }
  UpdateField field -> { model |
    field <- field
  }
  AddMessage mes -> { model |
    messages <- mes :: model.messages,
    noReadCount <- if model.opened then 0 else model.noReadCount + 1
  }
  _ -> model


onEnter : Signal.Address () -> Attribute
onEnter address =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address ())

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Views(no signals appears here)
messageView : Name -> ChatMessage -> Html
messageView myName (name, message, time) =
  let self = myName == name
      avator = img [src "", class "chat-avator"] []
      name_ = div [class "chat-name"] [text name]
      message_ = div [class "chat-message"] [ div [] [text message], time_]
      hour_ = toString <| Date.hour time
      minute_ = toString <| Date.minute time
      time_ = div [class "chat-time"] [text <| hour_ ++ ":" ++ minute_]
      clazz = if self then "chat chat-self" else "chat"
  in li [class clazz] [avator, name_, message_]

chatTimeline : Model -> Html
chatTimeline model =
  ul [
    class "list-unstyled"
  ] (List.map (\mes -> messageView model.myName mes) (List.reverse model.messages))

chatInput : Address Action -> String -> Html
chatInput address field = input [
    class "form-control",
    Html.Attributes.value field,
    on "input" targetValue (Signal.message address << UpdateField),
    onEnter (forwardTo address (\_ -> Send field))
  ] []

openedView : Address Action -> Model -> List Html
openedView address model =
  [
    div [class "panel-heading row", onClick address Close] [
      span [class "fa fa-comments"] [],
      text <| "Chat"
    ],
    div [class "panel-body"] [
      chatTimeline model
    ],
    div [class "row"] [
      chatInput address model.field
    ]
  ]

closedView : Address Action -> Int -> List Html
closedView address noReadCount =
  let noreadClass = if noReadCount > 0 then " noread" else ""
  in [
    div [
      class <| "panel-heading row" ++ noreadClass,
      onClick address Open
    ] [
      span [class "fa fa-comments"] [],
      text <| "Chat(" ++ (toString noReadCount) ++ ")"
    ]
  ]


view : Signal.Address Action -> Model -> Html
view address model =
  let inner = if | model.opened -> openedView address model
                 | otherwise -> closedView address model.noReadCount
      openedClass = if model.opened then " opened" else ""
  in div [class "chat-view-container container"] [
    div [
      class <| "chat-view panel panel-default col-xs-12 col-sm-8 col-md-6" ++ openedClass
    ] inner
  ]
























--
