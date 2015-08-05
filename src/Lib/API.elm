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
type alias Room =
  { id : String
  , private : Bool
  , members : List User
  , peers : List PeerId
  , users : List (PeerId, User)
  }
type alias User =
    { name : String
    , displayName : String
    , image : String
    , authority : String
    }
type alias PeerId = String

-- Data access
nocacheGet : Json.Decoder value -> String -> Task Http.Error value
nocacheGet decoder url =
  let
    request =
      { verb = "GET"
      , headers = [("Cache-Control", "no-cache"), ("If-Modified-Since", "Thu, 01 Jun 1970 00:00:00 GMT")]
      , url = url
      , body = Http.empty
      }
  in Http.fromJson decoder (Http.send Http.defaultSettings request)

postJson : Json.Decoder value -> String -> String -> Task Http.Error value
postJson decoder url body =
  let
    request =
      { verb = "POST"
      , headers =
        [ ("Cache-Control", "no-cache")
        , ("If-Modified-Since", "Thu, 01 Jun 1970 00:00:00 GMT")
        , ("Content-Type", "application/json")
        ]
      , url = url
      , body = Http.string body
      }
  in Http.fromJson decoder (Http.send Http.defaultSettings request)

getInitialData : String -> Task Http.Error InitialData
getInitialData roomId = nocacheGet initialDataDecoder (log "url" ("/api/room/" ++ roomId))

getRooms : Task Http.Error InitialRoomsData
getRooms = nocacheGet initialRoomsDataDecoder "/api/rooms"

searchUser : String -> Task Http.Error (List User)
searchUser q = nocacheGet (Json.list userDecoder) ("/api/search-user/" ++ q)


postInvitation : String -> String -> Task Http.Error ()
postInvitation roomName userName =
  let
    url = "/api/invite/" ++ roomName
    -- body = (Http.string <| "invited=" ++ userName ++ "&")
    body = (Http.string <| "{\"invited\":\"" ++ userName ++ "\"}")
    -- body = (Http.string <|  """{ "sortBy": "coolness", "take": 10 }""")
    -- body = Http.multipart [ Http.stringData "invited" userName]
  in
    postJson (Json.null ()) url ("{\"invited\":\"" ++ userName ++ "\"}")

initialDataDecoder : Json.Decoder InitialData
initialDataDecoder =
  Json.object3 (\user room iceServers -> { user=user, room=room, iceServers=iceServers })
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
userDecoder =
  Json.object4 (\name displayName image authority -> { name=name, displayName=displayName, image=image, authority=authority })
    ("name" := Json.string)
    ("displayName" := Json.string)
    ("image" := Json.string)
    ("authority" := Json.string)

roomDecoder : Json.Decoder Room
roomDecoder =
  let
    peer = Json.string
    toRecord id private members peers users =
      { id = id, private = private, members = members, peers = peers, users = users }
  in
    Json.object5 toRecord
      ("id" := Json.string)
      ("private" := Json.bool)
      ("members" := Json.list userDecoder)
      ("peers" := Json.list peer)
      ("users" := Json.keyValuePairs userDecoder)








--
