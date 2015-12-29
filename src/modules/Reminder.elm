module Reminder ( Model, Context, init -- Model
                , viewBody             -- View
                ) where

---- Imports ----
import Date exposing ( Date )
import Html exposing ( Html )

---- Model ----
type alias Model = { body : String
                   , date : Date
                   }

type alias Context = {}

init : Model
init = { body = "this is a body"
       , date = (Date.fromTime 1450706759.0 )}


---- View ----
viewBody : Model -> Html
viewBody model =
  let
    div = Html.div []
    text = Html.text
  in
    div [ text model.body ]
