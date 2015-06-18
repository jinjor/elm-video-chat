module Lib.WebSocket where

import Task exposing (..)
import Native.WebSocket

connect_ : String -> Task String ()
connect_ s = Native.WebSocket.connect s

connect : String -> Task Error ()
connect url = (connect_ url) `onError` (\s -> fail <| ConnectionError s)

message : Signal String
message = Native.WebSocket.message

opened : Signal Bool
opened = Native.WebSocket.opened

send_ : String -> Task String ()
send_ = Native.WebSocket.send

send : String -> Task Error ()
send mes = send_ mes `onError` (\s -> fail <| SendError s)

type Error = ConnectionError String | SendError String
