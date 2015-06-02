module Lib.VideoControl where

import Task exposing (..)
import Native.VideoControl
import Signal exposing (..)

requestFullScreen : String -> Task () ()
requestFullScreen = Native.VideoControl.requestFullScreen
