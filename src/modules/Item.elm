module Item (Model, init, Action, update, view, state) where
{-| Item describes the state of a single item in the item feed

-}

-- Imports
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- Model
type alias Model = { is_pinned : Bool
                   , is_done : Bool }

init : Model
init = { is_pinned = False
       , is_done = False
       }


-- Update
type Action
  = TogglePinned
  | ToggleDone
  | NoAction

update : Action -> Model -> Model
update action model =
  case action of
    TogglePinned -> { model | is_pinned <- not model.is_pinned }
    ToggleDone -> { model | is_done <- not model.is_done }
    NoAction -> model


-- View
view : Model -> Html
view model =
  let
    address = mailbox.address
    pin_text = if model.is_pinned then "Unpin" else "Pin"
    done_text = if model.is_done then "Undo" else "Mark as Done"
  in
    div []
      [ button [ onClick address TogglePinned ] [ text pin_text ]
      , button [ onClick address ToggleDone ] [ text done_text ]
      ]

-- State
mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoAction

state : Signal Model
state = Signal.foldp update init mailbox.signal
