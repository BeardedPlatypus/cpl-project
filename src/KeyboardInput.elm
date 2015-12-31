module KeyboardInput ( Model, init
                     , update, giveActions
                     , keyboardSignal
                     ) where

-- Import
import Keyboard

import AppActions as App
import KeyboardActions exposing ( .. )
import ItemFeed
import Item
import Email
import ItemState

-- Model
type alias Model = { alt_pressed : Bool
                   , next_pressed : Bool
                   , prev_pressed : Bool
                   , sort_pressed : Bool
                   , done_pressed : Bool
                   , pinned_pressed : Bool
                   , truncate_pressed : Bool
                   }

init : Model
init = { alt_pressed = False
       , next_pressed = False
       , prev_pressed = False
       , sort_pressed = False
       , done_pressed = False
       , pinned_pressed = False
       , truncate_pressed = False
       }

-- Update
update : Action -> Model -> Model
update action model =
  case action of
    NoAction       -> model
    ToggleAlt      -> { model | alt_pressed <- not model.alt_pressed }
    ToggleNext     -> { model | next_pressed <- not model.next_pressed }
    TogglePrev     -> { model | prev_pressed <- not model.prev_pressed }
    ToggleSort     -> { model | sort_pressed <- not model.sort_pressed }
    ToggleDone     -> { model | done_pressed <- not model.done_pressed }
    TogglePinned   -> { model | pinned_pressed <- not model.pinned_pressed }
    ToggleTruncate -> { model | truncate_pressed <- not model.truncate_pressed }


giveActions : Action -> Model -> List ItemFeed.Action
giveActions action model =
  let
    nextAction = ItemFeed.NextItem
    prevAction = ItemFeed.PreviousItem
    setSort = ItemFeed.SetSortComparison

    focusAction = ItemFeed.UpdateFocus
    doneAction = focusAction ( Item.UpdateItemState ItemState.ToggleDone )
    pinnedAction = focusAction ( Item.UpdateItemState ItemState.TogglePinned )
    truncateAction = focusAction (Item.UpdateEmailContext Email.ToggleDisplayBody )
  in
    case action of
      NoAction       -> []
      ToggleNext     -> if ( model.next_pressed && model.alt_pressed ) then
                          [ nextAction ]
                        else
                          []
      TogglePrev     -> if ( model.prev_pressed && model.alt_pressed ) then
                          [ prevAction ]
                        else
                          []
      ToggleDone     -> if ( model.done_pressed && model.alt_pressed ) then
                          [ doneAction ]
                        else
                          []
      TogglePinned   -> if ( model.pinned_pressed && model.alt_pressed ) then
                          [ pinnedAction ]
                        else
                          []
      ToggleTruncate -> if ( model.truncate_pressed && model.alt_pressed ) then
                          [ truncateAction ]
                        else
                          []
      ToggleSort     -> if ( model.sort_pressed && model.alt_pressed ) then
                          [ setSort Item.sortOrderOldTop ]
                        else
                          [ setSort Item.sortOrderDefault ]
      ToggleAlt      ->
        let
          possible_items =
            if model.alt_pressed then
              [ (if model.next_pressed then Just nextAction else Nothing )
              , (if model.prev_pressed then Just prevAction else Nothing )
              , (if model.sort_pressed then Just ( setSort Item.sortOrderOldTop ) else Nothing)
              , (if model.done_pressed then Just doneAction else Nothing )
              , (if model.pinned_pressed then Just pinnedAction else Nothing )
              , (if model.truncate_pressed then Just truncateAction else Nothing )
              ]
            else
              -- both alt and sort pressed were active, but alt swapped out thus back to default
              [ if model.sort_pressed then Just ( setSort Item.sortOrderDefault ) else Nothing]

          fold_func : Maybe ItemFeed.Action -> List ItemFeed.Action -> List ItemFeed.Action
          fold_func element acc =
            case element of
              Just action -> action :: acc
              Nothing -> acc
        in
          List.foldl fold_func [] possible_items


-- Signals
mapToAppAction : Action -> Signal a -> Signal App.Action
mapToAppAction action signal =
  Signal.map ( \_ -> App.UpdateKeyboardInput action ) signal

altSignal = mapToAppAction ToggleAlt ( Keyboard.isDown 18 )           -- 18 -> alt
nextSignal = mapToAppAction ToggleNext ( Keyboard.isDown 74 )         -- 74 -> j
prevSignal = mapToAppAction TogglePrev ( Keyboard.isDown 75 )         -- 75 -> k
sortSignal = mapToAppAction ToggleSort ( Keyboard.isDown 83 )         -- 83 -> s
doneSignal = mapToAppAction ToggleDone ( Keyboard.isDown 88 )         -- 88 -> x
pinnedSignal = mapToAppAction TogglePinned ( Keyboard.isDown 80 )     -- 80 -> p
truncateSignal = mapToAppAction ToggleTruncate ( Keyboard.isDown 79 ) -- 79 -> o

keyboardSignal : Signal App.Action
keyboardSignal = Signal.mergeMany [ altSignal
                                  , nextSignal
                                  , prevSignal
                                  , sortSignal
                                  , doneSignal
                                  , pinnedSignal
                                  , truncateSignal
                                  ]
