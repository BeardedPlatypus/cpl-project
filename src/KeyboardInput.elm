module KeyboardInput ( Model, init
                     , update, giveActions
                     , keyboardSignal
                     ) where

-- Import
import Keyboard

import AppActions exposing ( .. )
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
                   , todo_section_pressed : Bool
                   , reminder_input_pressed : Bool
                   }

init : Model
init = { alt_pressed = False
       , next_pressed = False
       , prev_pressed = False
       , sort_pressed = False
       , done_pressed = False
       , pinned_pressed = False
       , truncate_pressed = False
       , todo_section_pressed = False
       , reminder_input_pressed = False
       }


-- Update
update : KeyboardStateAction -> Model -> Model
update action model =
  case action of
    ToggleAlt      -> { model | alt_pressed <- not model.alt_pressed }
    ToggleNext     -> { model | next_pressed <- not model.next_pressed }
    TogglePrev     -> { model | prev_pressed <- not model.prev_pressed }
    ToggleSort     -> { model | sort_pressed <- not model.sort_pressed }
    ToggleDone     -> { model | done_pressed <- not model.done_pressed }
    TogglePinned   -> { model | pinned_pressed <- not model.pinned_pressed }
    ToggleTruncate -> { model | truncate_pressed <- not model.truncate_pressed }
    ToggleDoneSection -> { model | todo_section_pressed <- not model.todo_section_pressed }
    ToggleReminderInput -> { model | reminder_input_pressed <- not model.reminder_input_pressed }

giveActions : KeyboardStateAction -> Model -> List Action
giveActions action model =
  let
    nextAction = UpdateItemFeed ItemFeed.NextItem
    prevAction = UpdateItemFeed ItemFeed.PreviousItem
    toggleDoneAction = UpdateItemFeed ItemFeed.ToggleDoneVisibility
    setSort = (\ sort_method ->  UpdateItemFeed ( ItemFeed.SetSortComparison sort_method ) )

    focusAction = (\ sort_action -> UpdateItemFeed ( ItemFeed.UpdateFocus sort_action ) )
    doneAction = focusAction ( Item.UpdateItemState ItemState.ToggleDone )
    pinnedAction = focusAction ( Item.UpdateItemState ItemState.TogglePinned )
    truncateAction = focusAction (Item.UpdateEmailContext Email.ToggleDisplayBody )
    reminderAction = ToggleReminderInputView
  in
    case action of
      ToggleNext        -> if ( model.next_pressed && model.alt_pressed ) then
                             [ nextAction ]
                           else
                             []
      TogglePrev        -> if ( model.prev_pressed && model.alt_pressed ) then
                             [ prevAction ]
                           else
                             []
      ToggleDone        -> if ( model.done_pressed && model.alt_pressed ) then
                             [ doneAction ]
                           else
                             []
      TogglePinned      -> if ( model.pinned_pressed && model.alt_pressed ) then
                             [ pinnedAction ]
                           else
                             []
      ToggleTruncate    -> if ( model.truncate_pressed && model.alt_pressed ) then
                             [ truncateAction ]
                           else
                             []
      ToggleDoneSection -> if ( model.todo_section_pressed && model.alt_pressed ) then
                             [ toggleDoneAction ]
                           else
                             []
      ToggleSort        -> if ( model.sort_pressed && model.alt_pressed ) then
                             [ setSort Item.sortOrderOldTop ]
                           else
                             [ setSort Item.sortOrderDefault ]
      ToggleReminderInput -> if ( model.reminder_input_pressed && model.alt_pressed ) then
                               [ reminderAction ]
                             else
                               []
      ToggleAlt         ->
        let
          possible_items =
            if model.alt_pressed then
              [ ( if model.next_pressed then Just nextAction else Nothing )
              , ( if model.prev_pressed then Just prevAction else Nothing )
              , ( if model.sort_pressed then Just ( setSort Item.sortOrderOldTop ) else Nothing)
              , ( if model.done_pressed then Just doneAction else Nothing )
              , ( if model.pinned_pressed then Just pinnedAction else Nothing )
              , ( if model.truncate_pressed then Just truncateAction else Nothing )
              , ( if model.todo_section_pressed then Just toggleDoneAction else Nothing )
              , ( if model.reminder_input_pressed then Just reminderAction else Nothing )
              ]
            else
              -- both alt and sort pressed were active, but alt swapped out thus back to default
              [ if model.sort_pressed then Just ( setSort Item.sortOrderDefault ) else Nothing]

          fold_func : Maybe Action -> List Action -> List Action
          fold_func element acc =
            case element of
              Just action -> action :: acc
              Nothing -> acc
        in
          List.foldl fold_func [] possible_items


-- Signals
mapToAction : KeyboardStateAction -> Signal a -> Signal Action
mapToAction action signal =
  Signal.map ( \_ -> UpdateKeyboardInput action ) signal

altSignal = mapToAction ToggleAlt ( Keyboard.isDown 18 )           -- 18 -> alt
nextSignal = mapToAction ToggleNext ( Keyboard.isDown 74 )         -- 74 -> j
prevSignal = mapToAction TogglePrev ( Keyboard.isDown 75 )         -- 75 -> k
sortSignal = mapToAction ToggleSort ( Keyboard.isDown 83 )         -- 83 -> s
doneSignal = mapToAction ToggleDone ( Keyboard.isDown 88 )         -- 88 -> x
pinnedSignal = mapToAction TogglePinned ( Keyboard.isDown 80 )     -- 80 -> p
truncateSignal = mapToAction ToggleTruncate ( Keyboard.isDown 79 ) -- 79 -> o
toggleTodoSectionSignal = mapToAction ToggleDoneSection ( Keyboard.isDown 188 ) -- 222 -> ,
toggleReminderSignal = mapToAction ToggleReminderInput ( Keyboard.isDown 190 )   -- 188 -> .

keyboardSignal : Signal Action
keyboardSignal = Signal.mergeMany [ altSignal
                                  , nextSignal
                                  , prevSignal
                                  , sortSignal
                                  , doneSignal
                                  , pinnedSignal
                                  , truncateSignal
                                  , toggleTodoSectionSignal
                                  , toggleReminderSignal
                                  ]
