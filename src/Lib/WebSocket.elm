module Lib.WebSocket where

import Task exposing (..)
import Native.WebSocket

connect : String -> Task () ()
connect = Native.WebSocket.connect

message : Signal String
message = Native.WebSocket.message

opened : Signal Bool
opened = Native.WebSocket.opened

send : String -> Task () ()
send = Native.WebSocket.send
