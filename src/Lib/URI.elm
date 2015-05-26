module Lib.URI where

import Native.URI

encodeURI : String -> String
encodeURI = Native.URI.encodeURI

decodeURI : String -> String
decodeURI = Native.URI.decodeURI
