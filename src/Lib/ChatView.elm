module Lib.ChatView (Model, init, Action(..), update, updateMyName, view, Error) where

import Task exposing (..)

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)
import Date exposing (Date)
import Time exposing (Time)
import String

import Native.ChatView

-- Models

type alias Name = String
type alias Image = String
type alias ChatMessage = (Name, String, Date)
type alias IncomingChatMessage = (Name, Image, String, Date)
type Action
  = NoOp
  | Open
  | Close
  | UpdateField String
  | Send String
  | Message Name Image String Time

type alias Model =
  { opened : Bool
  , messages : List IncomingChatMessage
  , field : String
  , noReadCount : Int
  , myName : String
  }

type Error = Error String

init : Model
init =
  { opened = False
  , messages = []
  , field = ""
  , noReadCount = 0
  , myName = ""
  }

update : (String -> Task x ()) -> Action -> Model -> (Model, Maybe (Task Error ()))
update send action model =
  case log "ChatView.action" action of
    Open ->
      (,) { model |
        opened <- True,
        noReadCount <- 0
      } <| Just <| (focus "chat-input") `andThen` (\_ -> scrollDown "message-area") `onError` (\s -> fail <| Error s)
    Close ->
      (,) { model |
        opened <- False
      } Nothing
    Message name image s time ->
      (,) { model |
        messages <- (name, image, s, Date.fromTime time) :: model.messages,
        field <- if model.myName == name then "" else model.field,
        noReadCount <- if model.opened then 0 else model.noReadCount + 1
      } <| Just <| scrollDown "message-area" `onError` (\s -> fail <| Error s)
    UpdateField field ->
      (,) { model |
        field <- field
      } Nothing
    Send x ->
      (,) model <| Just <| send x `onError` (\s -> fail <| Error "")
    _ -> (,) model Nothing

updateMyName : String -> Model -> Model
updateMyName myName model =
  { model |
    myName <- myName
  }

-----

focus : String -> Task String ()
focus = Native.ChatView.focus

scrollDown : String -> Task String ()
scrollDown = Native.ChatView.scrollDown


onEnter : Address () -> Attribute
onEnter address =
  on "keydown"
    (Json.customDecoder keyCode is13)
    (\_ -> Signal.message address ())

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"


-- Views
messageView : Name -> IncomingChatMessage -> Html
messageView myName (name, image, message, time) =
  let
    self = myName == name
    avator = img [src image, class "chat-avator"] []
    name_ = div [class "chat-name"] [text name]
    message_ = div [class "chat-message"] [ div [] [text message], time_]
    time_ = div [class "chat-time"] [text <| dateToString time]
    clazz = if self then "chat chat-self" else "chat"
  in
    li [class clazz] (if self then [message_] else [avator, name_, message_])

chatTimeline : Model -> Html
chatTimeline model =
  ul [
    class "list-unstyled"
  ] (List.map (\mes -> messageView model.myName mes) (List.reverse model.messages))

chatInput : Address Action -> String -> Html
chatInput address field =
  input
    [ id "chat-input"
    , class "form-control"
    , Html.Attributes.value field
    , on "input" targetValue (Signal.message address << UpdateField)
    , onEnter (forwardTo address (\_ -> if (field == "") then NoOp else (Send field)))
    ] []

openedView : Address Action -> Model -> List Html
openedView address model =
  [ div [class "panel-heading row", onClick address Close]
      [ span [class "fa fa-comments"] []
      , text <| "Chat"
      ]
  , div [id "message-area", class "panel-body"]
      [ chatTimeline model
      ]
  , div [class "row"]
      [ chatInput address model.field
      ]
  ]

closedView : Address Action -> Int -> List Html
closedView address noReadCount =
  let
    noreadClass = if noReadCount > 0 then " noread" else ""
  in
    [ div
        [ class <| "panel-heading row" ++ noreadClass
        , onClick address Open
        ]
        [ span [class "fa fa-comments"] []
        , text <| "Chat(" ++ (toString noReadCount) ++ ")"
        ]
    ]

view : Signal.Address Action -> Model -> Html
view address model =
  let
    inner = if | model.opened -> openedView address model
               | otherwise -> closedView address model.noReadCount
    openedClass = if model.opened then " opened" else ""
  in
    div [class "chat-view-container container"]
      [ div
        [ class <| "chat-view panel panel-default col-xs-12 col-sm-8 col-md-6" ++ openedClass
        ] inner
      ]


---

dateToString : Date -> String
dateToString time =
  let
    hour_ = toString <| Date.hour time
    minute_ = toString <| Date.minute time
  in
    (fillZero2 hour_) ++ ":" ++ (fillZero2 minute_)

fillZero2 : String -> String
fillZero2 s = if String.length s == 2 then s else "0" ++ s














--
