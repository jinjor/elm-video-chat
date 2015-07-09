import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Debug exposing (log)
import Dict exposing (Dict)

import Lib.API exposing (..)

type alias Model = { error : Maybe String }

--- Model
init : Model
init = { error= Nothing }

model : Signal Model
model = Signal.foldp update init actions.signal

--- Action
type Action
  = NoOp

update : Action -> Model -> Model
update action model =
    case action of
      _ -> model


actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

--- View
view : Address Action -> Model -> Html
view address model =
  div [class "login-wrapper"][
    div [class "panel panel-default center-block", style [("max-width", "500px")]][
      a [href "/oauth/twitter"] [text "Login by twitter"]
      -- div [class "panel-heading"][
      --   h2 [class "panel-title"] [text "please login"]
      -- ]
      -- , div [class "panel-body"][
      --   Html.form [class "form-horizontal col-md-12", action "/api/login", method "POST"][
      --     div [class "form-group row"][
      --       label [class "col-sm-2 control-label"] [text "Mail"]
      --       , div [class "col-sm-10"] [
      --         input [class "form-control", name "email", value ""][]
      --       ]
      --     ]
      --     , div [class "form-group row"] [
      --       label [class "col-sm-2 control-label"][text "Password"]
      --       , div [class "col-sm-10"][
      --         input [class "form-control", type' "password", name "password", value ""][]
      --       ]
      --     ]
      --     , div [class "text-center"][
      --       input [class "login-submit btn btn-primary", type' "submit", value "submit"][]
      --     ]
      --   ]
      -- ]
    ]
  ]

  -- <div class="login-wrapper">
  --   <div class="panel panel-default center-block" style="max-width: 500px;">
  --     <div class="panel-heading">
  --       <h2 class="panel-title">please login</h2>
  --     </div>
  --     <div class="panel-body">
  --       <form class="form-horizontal col-md-12" action="/api/login" method="POST">
  --         <div class="form-group row">
  --           <label class="col-sm-2 control-label">Mail</label>
  --           <div class="col-sm-10">
  --             <input class="form-control" name="email" value=""></input>
  --           </div>
  --         </div>
  --         <div class="form-group row">
  --           <label class="col-sm-2 control-label">Password</label>
  --           <div class="col-sm-10">
  --             <input class="form-control" type="password" name="password" value=""></input>
  --           </div>
  --         </div>
  --         <div class="text-center">
  --           <input class="login-submit btn btn-primary" type="submit" value="submit"></input>
  --         </div>
  --       </form>
  --     </div>
  --   </div>
  -- </div>

--- Main
main : Signal Html
main = Signal.map (view actions.address) model
