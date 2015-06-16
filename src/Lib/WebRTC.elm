module Lib.WebRTC where

import Json.Decode as Json exposing ((:=))
import Json.Encode
import Task exposing (..)

import String
import Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Signal exposing (..)

import Debug exposing (log)

-- Models
type alias MediaType = String
type alias Connection = (PeerId, MediaType)
type alias PeerId = String

type Action =
    OfferSDP String String
  | AnswerSDP String String
  | OfferCandidate String String
  | AnswerCandidate String String
  | EndStream String String
  | Undefined

encode : Action -> String
encode action = Json.Encode.encode 0 (encoder action)

encoder action =
  let (type_, from, data_) = case action of
    OfferSDP f d -> ("offerSDP", f, d)
    AnswerSDP f d -> ("answerSDP", f, d)
    OfferCandidate f d -> ("offerCandidate", f, d)
    AnswerCandidate f d -> ("answerCandidate", f, d)
    EndStream f d -> ("endStream", f, d)
  in Json.Encode.object
    [ ("type", Json.Encode.string type_)
    , ("from", Json.Encode.string from)
    , ("data", Json.Encode.string data_)
    ]

actions : Signal String -> Signal Action
actions rawJsonSignal = Signal.map decode rawJsonSignal

decode : String -> Action
decode s = case Json.decodeString decoder s of
  Ok decoded -> decoded
  Err s -> Undefined

convertToAction : String -> PeerId -> String -> Action
convertToAction type_ from data_ =
  if | type_ == "offerSDP" -> OfferSDP from data_
     | type_ == "answerSDP" -> AnswerSDP from data_
     | type_ == "offerCandidate" -> OfferCandidate from data_
     | type_ == "answerCandidate" -> AnswerCandidate from data_
     | type_ == "endStream" -> EndStream from data_
     | otherwise -> Undefined

decoder : Json.Decoder Action
decoder = Json.object3 (\t f d -> convertToAction t f (Json.Encode.encode 0 d))
    ("type" := Json.string)
    ("from" := Json.string)
    ("data" := Json.value)
--

requests : Signal String
requests = Native.WebRTC.requests

onLocalVideoURL : Signal (String, Maybe String)
onLocalVideoURL = Native.WebRTC.onLocalVideoURL

onRemoteVideoURL : Signal (Connection, Maybe String)
onRemoteVideoURL = Native.WebRTC.onRemoteVideoURL

onAddConnetion : Signal Connection
onAddConnetion = Native.WebRTC.onAddConnetion

onRemoveConnetion : Signal Connection
onRemoveConnetion = Native.WebRTC.onRemoveConnetion


--

answerSDP : String -> String -> Task () ()
answerSDP = Native.WebRTC.answerSDP

acceptAnswer : String -> String -> Task () ()
acceptAnswer = Native.WebRTC.acceptAnswer

addCandidate : String -> String -> Task () ()
addCandidate = Native.WebRTC.addCandidate

closeRemoteStream : String -> String -> Task () ()
closeRemoteStream = Native.WebRTC.closeRemoteStream

-- public

startStreaming : String -> String -> Task () ()
startStreaming = Native.WebRTC.startStreaming

endStreaming : String -> Task () ()
endStreaming = Native.WebRTC.endStreaming

beforeJoin : String -> Task () ()
beforeJoin = Native.WebRTC.beforeJoin

beforeLeave : String -> Task () ()
beforeLeave = Native.WebRTC.beforeLeave
--

replay : Signal Action -> Signal (Task () ())
replay actions =
  let f action = case action of
    OfferSDP from data_ -> answerSDP from data_
    AnswerSDP from data_ -> acceptAnswer from data_
    OfferCandidate from data_ -> addCandidate from data_
    AnswerCandidate from data_ -> addCandidate from data_
    EndStream from data_ -> closeRemoteStream from data_
  in Signal.map f actions
