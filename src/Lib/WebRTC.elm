module Lib.WebRTC (Model, init, Action(..), initialize, initRoom, update, actions, decode, Error, logError) where

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
type alias Upstream = Int
type alias MediaType = String
type alias Connection = (PeerId, MediaType, Upstream)
type alias PeerId = String

type Action
  = NoOp
  | LocalVideoUrl (MediaType, Maybe String)
  | RemoteVideoUrl (Connection, Maybe String)
  | AddConnection Connection
  | RemoveConnection Connection
  | CloseWindow Connection
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
  | Volumes (List (PeerId, Int))
  | Undefined

type alias Model =
    { peers : Set PeerId
    , users : Dict PeerId User
    , me : User
    , connections : Set Connection
    , videoUrls : Dict Connection String
    , localVideoUrls : Dict String String
    , localVideo : Bool
    , localAudio : Bool
    , localScreen : Bool
    , volumes : List (PeerId, Int)
    }

type Error = Error String


init : Model
init =
    { peers = Set.empty
    , users = Dict.empty
    , me = {name="", displayName="", image="", authority=""}
    , connections = Set.empty
    , videoUrls = Dict.empty
    , localVideoUrls = Dict.empty
    , localVideo = False
    , localAudio = False
    , localScreen = False
    , volumes = []
    }

initRoom : (List PeerId) -> (List (PeerId, User)) -> User -> Model -> Model
initRoom peers users me model =
  { model |
    peers <- Set.fromList peers,
    users <- Dict.fromList(users),
    me <- me
  }

initialize : PeerId -> (List Json.Encode.Value) -> Task Error ()
initialize selfId iceServers =
  initialize' selfId iceServers `onError` (\e -> fail <| Error e)

update : ((String, String, Json.Encode.Value) -> Task () ()) -> Action -> Model -> (Model, Maybe (Task Error ()))
update send action model = case action of
  OfferSDP from data_ ->
    (,) model <| Just <| answerSDP from data_ `onError` (\e -> fail <| Error e)
  AnswerSDP from data_ ->
    (,) model <| Just <| acceptAnswer from data_ `onError` (\e -> fail <| Error e)
  OfferCandidate from data_ ->
    (,) model <| Just <| addCandidate from data_ False `onError` (\e -> fail <| Error e)
  AnswerCandidate from data_ ->
    (,) model <| Just <| addCandidate from data_ True `onError` (\e -> fail <| Error e)
  EndStream from data_ ->
    (,) model <| Just <| closeRemoteStream from data_ `onError` (\e -> fail <| Error e)
  StartStreaming (mediaType, peers) ->
    (,) model <| Just <| startStreaming mediaType peers `onError` (\e -> fail <| Error e)
  EndStreaming (mediaType, peers) ->
    (,) model <| Just <| endStreaming mediaType `onError` (\e -> fail <| Error e)
  Request x ->
    (,) model <| Just <| send x `onError` (\e -> fail <| Error "")
  CloseWindow target ->
    (,) { model |
      connections <- Set.remove target model.connections
    } Nothing
  RemovePeer target ->
    (,) { model |
      peers <- Set.remove target model.peers
    } Nothing
  AddConnection conn ->
    (,) { model |
      connections <- Set.insert conn model.connections
    } Nothing
  RemoveConnection conn ->
    (,) { model |
      connections <- Set.remove conn model.connections
    } Nothing
  LocalVideoUrl (mediaType, maybeUrl) ->
    (,) { model |
      localVideoUrls <- case maybeUrl of
        Just url -> Dict.insert mediaType url model.localVideoUrls
        Nothing -> Dict.remove mediaType model.localVideoUrls
    } Nothing
  RemoteVideoUrl (conn, maybeUrl) ->
    (,) { model |
      videoUrls <- case maybeUrl of
        Just url -> Dict.insert conn url model.videoUrls
        Nothing -> Dict.remove conn model.videoUrls
    } Nothing
  Join peerId user ->
    (,) { model |
      peers <- Set.insert peerId model.peers,
      users <- Dict.insert peerId user model.users
    } <| Just <| beforeJoin peerId `onError` (\e -> fail <| Error e)
  Leave peerId ->
    (,) { model |
      peers <- Set.remove peerId model.peers,
      users <- Dict.remove peerId model.users
    } <| Just <| beforeLeave peerId `onError` (\e -> fail <| Error e)
  Volumes volumes ->
    (,) { model |
      volumes <- volumes
    } Nothing
  _ -> (,) model Nothing

logError : Error -> String
logError e = case e of
  Error mes -> mes


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

actions : Signal x -> Signal Action
actions volumeSampling = Signal.mergeMany
  [ LocalVideoUrl <~ onLocalVideoURL
  , RemoteVideoUrl <~ onRemoteVideoURL
  , AddConnection <~ onAddConnection
  , RemoveConnection <~ onRemoveConnection
  , Request <~ requests
  , Volumes <~ toVolumes volumeSampling
  ]

decode : String -> Maybe Action
decode s = case Json.decodeString decoder s of
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
  let
    toRecord (id, name, displayName, image, authority) =
      {name = name, displayName = displayName, image = image, authority = authority}
    userDecoder =
      Json.object5 (,,,,)
        ("id" := Json.string)
        ("name" := Json.string)
        ("displayName" := Json.string)
        ("image" := Json.string)
        ("authority" := Json.string)
  in
    Json.object1 toRecord ("user" := userDecoder)

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

initialize' : PeerId -> List Json.Encode.Value -> Task String ()
initialize' = Native.WebRTC.initialize

answerSDP : String -> Json.Encode.Value -> Task String ()
answerSDP = Native.WebRTC.answerSDP

acceptAnswer : String -> Json.Encode.Value -> Task String ()
acceptAnswer = Native.WebRTC.acceptAnswer

addCandidate : String -> Json.Encode.Value -> Bool -> Task String ()
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

currentVolumes : x -> (List (PeerId, Int))
currentVolumes = Native.WebRTC.currentVolumes

toVolumes : Signal x -> Signal (List (PeerId, Int))
toVolumes signal = Signal.map currentVolumes signal
