module Lib.PanelHeader where

import Json.Decode as Json exposing ((:=))
import Json.Encode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)
import Time exposing (Time)




-- Models


window : Html -> Html -> Bool -> Html
window header body local =
  let face = if local then "panel-primary" else "panel-default"
  in div [class "col-sm-6 col-md-6"]
      [ div [class ("panel " ++ face)] [header, body]
      ]


-- Views(no signals appears here)











--
