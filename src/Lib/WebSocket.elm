module Lib.WebSocket (Model, init, Action(Message), update, actions, connect, send, Error, logError) where

import Task exposing (..)
import Signal exposing (..)
import Native.WebSocket

type alias Model = {
    connected: Bool
  }

init : Model
init = {
    connected = False
  }

type Action
  = Opened Bool
  | Message String

update: Action -> Model -> Model
update action model =
  case action of
    Opened opened ->
      { model | connected <- opened }
    _ ->
      model


connect_ : String -> Task String ()
connect_ s = Native.WebSocket.connect s

connect : String -> Task Error ()
connect url = (connect_ url) `onError` (\s -> fail <| ConnectionError s)

actions : Signal Action
actions = Signal.mergeMany
  [ Opened <~ opened
  , Message <~ message
  ]

message : Signal String
message = Native.WebSocket.message

opened : Signal Bool
opened = Native.WebSocket.opened

send_ : String -> Task String ()
send_ = Native.WebSocket.send

send : String -> Task Error ()
send mes = send_ mes `onError` (\s -> fail <| SendError s)

logError : Error -> String
logError e = case e of
  ConnectionError mes -> mes
  SendError mes -> mes

type Error = ConnectionError String | SendError String
