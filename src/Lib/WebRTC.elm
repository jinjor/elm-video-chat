module Lib.WebRTC where

import Http
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
type alias InitialData = { room: Room, user: User }
type alias InitialRoomsData = { rooms: List Room, user: User }
type alias Room = { id:String, peers: List PeerId, users: List (PeerId, User)}
type alias User = { name:String, email:String }
type alias PeerId = String

Action =
    OfferSDP String String
  | AnswerSDP String String
  | OfferCandidate String String
  | AnswerCandidate String String
  | Undefined

send : Address String -> Action -> Task () ()
send address action -> Signal.send (forwardTo address encode) action

encode : Action -> String
encode action = Json.encode 0 (encoder action)

encoder action =
  let (type_, from, data_) = case action of
    OfferSDP f d -> ("offerSDP", f, d)
    AnswerSDP f d -> ("answerSDP", f, d)
    OfferCandidate f d -> ("offerCandidate", f, d)
    AnswerCandidate f d -> ("answerCandidate", f, d)
  in object
    [ ("type", string type_)
    , ("from", string from)
    , ("data", string data_)
    ]

actions : Signal String -> Signal (Maybe Action)
actions rawJsonSignal = Signal.map decode rawJsonSignal

decode : String -> Maybe Action
decode s = case Json.decodeString decoder s of
  Ok decoded -> Just decoded
  Err s -> Nothing

convertToAction : String -> PeerId -> String -> Action
convertToAction type_ from data_ =
  if | type_ == "offerSDP" -> OfferSDP from data_
     | type_ == "answerSDP" -> AnswerSDP from data_
     | type_ == "offerCandidate" -> OfferCandidate from data_
     | type_ == "answerCandidate" -> AnswerCandidate from data_
     | otherwise -> Undefined

decoder : Json.Decoder Action
decoder = Json.object3 (\t f d -> convertToAction t f (Json.Encode.encode 0 d))
    ("type" := Json.string)
    ("from" := Json.string)
    ("data" := Json.value)
--
