module ItemState ( Model, init          -- Model
                 , Action( NoAction     -- Update
                         , ToggleDone
                         , TogglePinned
                         )
                 , update
                 , viewButtons          -- View
                 ) where


---- Imports ----
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Css

---- Model ----
type alias Model =
  { is_pinned : Bool
  , is_done : Bool
  }


init : Model
init = { is_pinned = False
       , is_done = False
       }


---- Update ----
type Action
  = TogglePinned
  | ToggleDone
  | NoAction


update : Action -> Model -> Model
update action model =
  case action of
    TogglePinned -> { model | is_pinned <- not model.is_pinned }
    ToggleDone   -> { model | is_done <- not model.is_done }
    NoAction     -> model


---- View ----
viewButtons : Signal.Address Action -> Model -> Html
viewButtons address model =
  let
    div = Html.div

    pin_text = if model.is_pinned then "Unpin" else "Pin"
    done_text = if model.is_done then "Undo" else "Mark as Done"

    togglePinnedButton = Css.button [ onClick address TogglePinned ] [ text pin_text ]
    toggleDoneButton =  Css.button [ onClick address ToggleDone ] [ text done_text ]
  in
    Css.row_ [ div [ class "form-group" ]
                   [ div [ class "col-sm-6" ] [ togglePinnedButton ]
                   , div [ class "col-sm-6" ] [ toggleDoneButton ]
                   ]
             ]
