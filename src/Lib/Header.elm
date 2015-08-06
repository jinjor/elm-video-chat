module Lib.Header where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)

-- Models
type alias Model a =
  { user: User a
  , connected: Bool
  }
type alias User a = { a | displayName: String }


-- Views(no signals appears here)
header : Model a -> Html
header c =
  let
    offline = div [class "navbar-header"]
      [ div [class "navbar-text header-offline"]
          [ if c.connected then text "" else text "(Offline)"
          ]
      ]
  in
    div [class "navbar navbar-default"]
      [ div [class "container"]
        [ div [class "navbar-header"]
            [ a [class "navbar-brand", href "/"]
                [text "Video Chat"
                ]
            ]
        , offline
        , div [class "collapse navbar-collapse"]
            [ welcomeView
            , div [class "navbar-text navbar-right"]
              [ text ("Hello, " ++ c.user.displayName)
              ]
            ]
        ]
      ]

welcomeView : Html
welcomeView =
  div
    [class "navbar-text navbar-right"]
    [ logoutButton
    ]

logoutButton : Html
logoutButton = a [class "navbar-link", href "/logout"] [text "Logout"]
