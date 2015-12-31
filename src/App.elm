module App ( Model                         -- Model
           , update
           , view                          -- View
           ) where


---- Import ----
import AppActions exposing (..)

import ItemFeed
import ReminderInput

import KeyboardInput

import Html exposing ( Html )
import Css

import Html.Events
import Json.Decode as Json
import Keyboard

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------
type alias Model = { -- Data
                     item_feed : ItemFeed.Model
                   , reminder_input : ReminderInput.Model
                   , keyboard_input : KeyboardInput.Model
                   -- View specific
                   , view_reminder_input : Bool
                   }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------
update : Action -> Model -> Model
update action model =
  case action of
    NoAction -> model
    UpdateItemFeed itemfeed_action ->
      { model | item_feed <- ItemFeed.update itemfeed_action model.item_feed }
    UpdateReminderInput reminder_action ->
      case reminder_action of
        ReminderInput.AddReminder reminder_model ->
          { model | item_feed <- ItemFeed.addReminder reminder_model model.item_feed
                  , reminder_input <- ReminderInput.update reminder_action
                                                           model.reminder_input }

        _ ->
          { model | reminder_input <- ReminderInput.update reminder_action
                                                           model.reminder_input }
    UpdateKeyboardInput keyboard_action ->
      let
        keyboard_model = KeyboardInput.update keyboard_action model.keyboard_input
        action_list = KeyboardInput.giveActions keyboard_action keyboard_model
        updated_model = { model | keyboard_input <- keyboard_model }
      in
        List.foldl update updated_model action_list
    ToggleReminderInputView ->
      { model | view_reminder_input <- not model.view_reminder_input }


--------------------------------------------------------------------------------
---- View ----
--------------------------------------------------------------------------------
-- Main view
view : Signal.Address Action -> Model -> Html
view address model =
  let
    text = Html.text
    small = Html.small []
    item_feed = viewItemFeed address model.item_feed
    section_elements = if model.view_reminder_input then
                         [ item_feed
                         , viewReminderInput address model.reminder_input
                         ]
                       else
                         [ item_feed ]
  in
    Css.body [ Css.header [ text "elm-TODO"
                          , small [ text "cpl project" ]
                          ]
             , Css.appContainer section_elements
             , Css.footer [ ]
             ]


-- Element views
viewItemFeed : Signal.Address Action -> ItemFeed.Model -> Html
viewItemFeed address model =
  ItemFeed.view ( Signal.forwardTo address UpdateItemFeed  ) model


viewReminderInput : Signal.Address Action -> ReminderInput.Model -> Html
viewReminderInput address model =
  ReminderInput.view ( Signal.forwardTo address UpdateReminderInput ) model

