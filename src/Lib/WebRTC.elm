module Lib.WebRTC (Model, init, Action(..), update, actions, doTask, decode, Error) where

import Json.Decode as Json exposing ((:=))
import Json.Encode
import Task exposing (..)

import String
import Maybe
import Set exposing (Set)
import Dict exposing (Dict)
import Signal exposing (..)

import Debug exposing (log)
import Native.WebRTC

import Lib.API as API exposing (User)

-- Models
type alias MediaType = String
type alias Connection = (PeerId, MediaType)
type alias PeerId = String

type Action =
    LocalVideoUrl (MediaType, Maybe String)
  | RemoteVideoUrl (Connection, Maybe String)
  | AddConnection Connection
  | RemoveConnection Connection
  | CloseWindow Connection
  | InitRoom (List PeerId) (List (PeerId, User)) User
  | RemovePeer String
  | StartStreaming (String, List PeerId)
  | EndStreaming (String, List PeerId)
  | Request (String, String, Json.Encode.Value)
  | OfferSDP String Json.Encode.Value
  | AnswerSDP String Json.Encode.Value
  | OfferCandidate String Json.Encode.Value
  | AnswerCandidate String Json.Encode.Value
  | EndStream String Json.Encode.Value
  | Join PeerId User
  | Leave PeerId
  | Undefined

type alias Model = {
      peers: Set PeerId
    , users: Dict PeerId User
    , me: User
    , connections: Set Connection
    , videoUrls: Dict Connection String
    , localVideoUrls: Dict String String
    , localVideo: Bool
    , localAudio: Bool
    , localScreen: Bool
  }

type Error = Error String


init : Model
init = {
      peers = Set.empty
    , users = Dict.empty
    , me = {name="", email=""}
    , connections = Set.empty
    , videoUrls = Dict.empty
    , localVideoUrls = Dict.empty
    , localVideo = False
    , localAudio = False
    , localScreen = False
  }


update : Action -> Model -> Model
update action model = case log "WebRTC.update" action of
  CloseWindow target ->
    { model |
      connections <- Set.remove target model.connections
    }
  InitRoom peers users me ->
    { model |
      peers <- Set.fromList peers,
      users <- Dict.fromList(users),
      me <- me
    }
  RemovePeer target ->
    { model |
      peers <- Set.remove target model.peers
    }
  AddConnection conn ->
    { model |
      connections <- Set.insert conn model.connections
    }
  RemoveConnection conn ->
    { model |
      connections <- Set.remove conn model.connections
    }
  LocalVideoUrl (mediaType, maybeUrl) ->
    { model |
      localVideoUrls <- case maybeUrl of
        Just url -> Dict.insert mediaType url model.localVideoUrls
        Nothing -> Dict.remove mediaType model.localVideoUrls
    }
  RemoteVideoUrl (conn, maybeUrl) ->
    { model |
      videoUrls <- case maybeUrl of
        Just url -> Dict.insert conn url model.videoUrls
        Nothing -> Dict.remove conn model.videoUrls
    }
  Join peerId user ->
    { model |
      peers <- Set.insert peerId model.peers,
      users <- Dict.insert peerId user model.users
    }
  Leave peerId ->
    { model |
      peers <- Set.remove peerId model.peers,
      users <- Dict.remove peerId model.users
    }
  _ -> model

encode : Action -> String
encode action = Json.Encode.encode 0 (encoder action)

encoder : Action -> Json.Encode.Value
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
    , ("data", data_)
    ]

actions : Signal Action
actions = Signal.mergeMany [
    LocalVideoUrl <~ onLocalVideoURL
  , RemoteVideoUrl <~ onRemoteVideoURL
  , AddConnection <~ onAddConnection
  , RemoveConnection <~ onRemoveConnection
  , Request <~ requests
  ]

decode : String -> Maybe Action
decode s = case Json.decodeString decoder (log "WebRTC.decode" s) of
  Ok decoded -> case decoded of
    Undefined -> Nothing
    _ -> Just decoded
  Err s -> Nothing

convertToAction : String -> PeerId -> Json.Encode.Value -> Action
convertToAction type_ from data_ =
  if | type_ == "offerSDP" -> OfferSDP from data_
     | type_ == "answerSDP" -> AnswerSDP from data_
     | type_ == "offerCandidate" -> OfferCandidate from data_
     | type_ == "answerCandidate" -> AnswerCandidate from data_
     | type_ == "endStream" -> EndStream from data_
     | type_ == "join" -> case Json.decodeValue joinDecoder data_ of
                            Ok x -> Join from <| log "join" x
                            _ -> Undefined
     | type_ == "leave" -> Leave from
     | otherwise -> Undefined

decoder : Json.Decoder Action
decoder = Json.object3 (\t f d -> convertToAction t f d)
    ("type" := Json.string)
    ("from" := Json.string)
    ("data" := Json.value)


joinDecoder : Json.Decoder User
joinDecoder =
  let userDecoder = Json.object3 (,,) ("id" := Json.string) ("name" := Json.string) ("email" := Json.string)
  in Json.object1 (\(id, name, email) -> {name= name, email= email}) ("user" := userDecoder)

--

requests : Signal (String, String, Json.Encode.Value)
requests = Native.WebRTC.requests


onLocalVideoURL : Signal (MediaType, Maybe String)
onLocalVideoURL =
  let f (mediaType, url) = if url == "" then (mediaType, Nothing) else (mediaType, Just url)
  in Signal.map f Native.WebRTC.onLocalVideoURL

onRemoteVideoURL : Signal (Connection, Maybe String)
onRemoteVideoURL =
  let f (conn, url) = if url == "" then (conn, Nothing) else (conn, Just url)
  in Signal.map f Native.WebRTC.onRemoteVideoURL

onAddConnection : Signal Connection
onAddConnection = Native.WebRTC.onAddConnection

onRemoveConnection : Signal Connection
onRemoveConnection = Native.WebRTC.onRemoveConnection

--

answerSDP : String -> Json.Encode.Value -> Task String ()
answerSDP = Native.WebRTC.answerSDP

acceptAnswer : String -> Json.Encode.Value -> Task String ()
acceptAnswer = Native.WebRTC.acceptAnswer

addCandidate : String -> Json.Encode.Value -> Task String ()
addCandidate = Native.WebRTC.addCandidate

closeRemoteStream : String -> Json.Encode.Value -> Task String ()
closeRemoteStream = Native.WebRTC.closeRemoteStream



--

startStreaming : MediaType -> List PeerId -> Task String ()
startStreaming = Native.WebRTC.startStreaming

endStreaming : MediaType -> Task String ()
endStreaming = Native.WebRTC.endStreaming

beforeJoin : String -> Task String ()
beforeJoin = Native.WebRTC.beforeJoin

beforeLeave : String -> Task String ()
beforeLeave = Native.WebRTC.beforeLeave
--

doTask : Action -> Task Error ()
doTask action = case log "WebRTC.doTask" action of
    OfferSDP from data_ -> answerSDP from data_ `onError` (\e -> fail <| Error e)
    AnswerSDP from data_ -> acceptAnswer from data_ `onError` (\e -> fail <| Error e)
    OfferCandidate from data_ -> addCandidate from data_ `onError` (\e -> fail <| Error e)
    AnswerCandidate from data_ -> addCandidate from data_ `onError` (\e -> fail <| Error e)
    EndStream from data_ -> closeRemoteStream from data_ `onError` (\e -> fail <| Error e)
    StartStreaming (mediaType, peers) -> startStreaming mediaType peers `onError` (\e -> fail <| Error e)
    EndStreaming (mediaType, peers) -> endStreaming mediaType `onError` (\e -> fail <| Error e)
    Join peerId user -> beforeJoin peerId `onError` (\e -> fail <| Error e)
    Leave peerId -> beforeLeave peerId `onError` (\e -> fail <| Error e)
    _ -> Task.succeed ()
