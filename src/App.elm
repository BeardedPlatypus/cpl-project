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

---- Model ----
type alias Model = { item_feed : ItemFeed.Model
                   , reminder_input : ReminderInput.Model
                   , keyboard_input : KeyboardInput.Model
                   }


---- Update ----
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
        item_feed_actions = KeyboardInput.giveActions keyboard_action keyboard_model
        item_feed_updated = List.foldl ItemFeed.update model.item_feed item_feed_actions
      in
        { model | keyboard_input <- keyboard_model
                , item_feed <- item_feed_updated }


---- View ----
-- Main view
view : Signal.Address Action -> Model -> Html
view address model =
  let
    text = Html.text
  in
    Css.body [ Css.header [ Css.h1 [ text "elm-TODO" ] ]
             , Css.appContainer [ viewItemFeed address model.item_feed
                                , viewReminderInput address model.reminder_input
                                ]
             , Css.footer [ ]
             ]


-- Element views
viewItemFeed : Signal.Address Action -> ItemFeed.Model -> Html
viewItemFeed address model =
  ItemFeed.view ( Signal.forwardTo address UpdateItemFeed  ) model


viewReminderInput : Signal.Address Action -> ReminderInput.Model -> Html
viewReminderInput address model =
  ReminderInput.view ( Signal.forwardTo address UpdateReminderInput ) model

