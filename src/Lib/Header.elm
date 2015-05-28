module Lib.Header where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)

import Debug exposing (log)

-- Models
type alias Context a = { user: User a }
type alias User a = { a | name: String }


-- Views(no signals appears here)
header : Context a -> Html
header c = div [class "navbar navbar-default"] [
            div [class "container"] [
              div [class "navbar-header"] [
                a [class "navbar-brand", href "/"] [
                  text "Video Chat"
                ]
              ],
              div [class "collapse navbar-collapse"] [
                welcomeView c.user
              ]
            ]
          ]


welcomeView user = div [class "navbar-text navbar-right"] [
                      text ("Hello, " ++ user.name),
                      logoutButton
                    ]

logoutButton = a [class "navbar-link", href "/logout"] [text "Logout"]
