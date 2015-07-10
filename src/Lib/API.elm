module Lib.API where

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
type alias InitialData = { room: Room, user: User, iceServers: List Json.Encode.Value }
type alias InitialRoomsData = { rooms: List Room, user: User }
type alias Room = { id:String, peers: List PeerId, users: List (PeerId, User)}
type alias User = { name:String, displayName:String, image:String }
type alias PeerId = String

-- Data access
nocacheGet : Json.Decoder value -> String -> Task Http.Error value
nocacheGet decoder url =
  let request = {
      verb = "GET"
      , headers = [("Cache-Control", "no-cache"), ("If-Modified-Since", "Thu, 01 Jun 1970 00:00:00 GMT")]
      , url = url
      , body = Http.empty
    }
  in Http.fromJson decoder (Http.send Http.defaultSettings request)

getInitialData : String -> Task Http.Error InitialData
getInitialData roomId = nocacheGet initialDataDecoder (log "url" ("/api/room/" ++ roomId))

getRooms : Task Http.Error InitialRoomsData
getRooms = nocacheGet initialRoomsDataDecoder "/api/rooms"


initialDataDecoder : Json.Decoder InitialData
initialDataDecoder = Json.object3 (\user room iceServers -> { user=user, room=room, iceServers=iceServers })
      ("user" := userDecoder)
      ("room" := roomDecoder)
      ("iceServers" := Json.list Json.value)

initialRoomsDataDecoder : Json.Decoder InitialRoomsData
initialRoomsDataDecoder =
  let rooms = Json.list roomDecoder
  in Json.object2 (\user rooms -> { user=user, rooms=rooms })
    ("user" := userDecoder)
    ("rooms" := rooms)


userDecoder : Json.Decoder User
userDecoder = Json.object3 (\name displayName image -> { name=name, displayName=displayName, image=image })
        ("name" := Json.string)
        ("displayName" := Json.string)
        ("image" := Json.string)

roomDecoder : Json.Decoder Room
roomDecoder =
  let peer = Json.string
  in Json.object3 (\id peers users -> { id=id, peers=peers, users=users })
      ("id" := Json.string)
      ("peers" := Json.list peer)
      ("users" := Json.keyValuePairs userDecoder)








--
