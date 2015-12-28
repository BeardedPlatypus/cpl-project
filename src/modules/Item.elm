module Item ( Item, initEmail, initReminder  -- Model
            , Action, getNoAction, update
            , view                           -- View
            , getState                       -- Util
            , sortOrderDefault, sortOrderOldTop
            ) where


---- Imports ----
import Email
import Reminder
import ItemState

import Date exposing ( Date )
import Html exposing ( Html )


---- Model ----
type Item = EmailItem ItemState.Model Email.Context Email.Model
          | ReminderItem ItemState.Model Reminder.Context Reminder.Model


initEmail : Email.Model -> Item
initEmail model =
  EmailItem ItemState.init Email.initContext model


initReminder : Reminder.Model -> Item
initReminder model =
  ReminderItem ItemState.init {} model


---- Update ----
type Action = NoAction
            | UpdateItemState ItemState.Action
            | UpdateEmailContext Email.Action


getNoAction : Action
getNoAction = NoAction


update : Action -> Item -> Item
update action item =
  case action of
    NoAction -> item
    UpdateItemState itemStateAction ->
      case item of
        EmailItem itemState context model ->
          let
            newItemState = ItemState.update itemStateAction itemState
          in
            EmailItem newItemState context model
        ReminderItem itemState context model ->
          let
            newItemState = ItemState.update itemStateAction itemState
          in
            ReminderItem newItemState context model
    UpdateEmailContext emailAction ->
      case item of
        EmailItem itemState context model ->
          let
            newContext = Email.updateContext emailAction context
          in
            EmailItem itemState newContext model
        ReminderItem itemState context model ->
          ReminderItem itemState context model


---- View ----
view : Signal.Address Action -> Item -> Html
view address item =
  let
    div = Html.div []
  in
    case item of
      -- Display a single Email Item
      EmailItem itemState emailContext emailModel ->
        let
          itemStateButtons = viewItemStateButtons address itemState
          toggleBodyButton = viewEmailToggleButton address emailModel emailContext

          buttons = case toggleBodyButton of
                      Just button -> button :: itemStateButtons
                      Nothing -> itemStateButtons
        in
          div [ Email.viewHeader emailModel            -- Header email
              , Email.viewBody emailModel emailContext -- Body email
              , div buttons                            -- Toggle buttons
              , viewDate emailModel                    -- Date
              ]
    -- Display a single Reminder Item
      ReminderItem itemState reminderContext reminderModel ->
        let
          itemStateButtons = viewItemStateButtons address itemState
        in
          div [ Reminder.viewBody reminderModel    -- Reminder body
              , div itemStateButtons               -- Toggle buttons
              , viewDate reminderModel             -- Date
              ]


viewItemStateButtons : Signal.Address Action -> ItemState.Model -> List Html
viewItemStateButtons address model =
  ItemState.viewButtons ( Signal.forwardTo address UpdateItemState ) model


viewEmailToggleButton : Signal.Address Action -> Email.Model -> Email.Context -> Maybe Html
viewEmailToggleButton address model context =
  Email.viewDisplayAll ( Signal.forwardTo address UpdateEmailContext ) model context

type alias HasDate a = { a | date : Date }

viewDate : HasDate a -> Html
viewDate { date } =
  let
    div = Html.div []
    text = Html.text
  in
    div [ text "date: ", text (( toString ( Date.year date ))  ++ " - " ++
                               ( toString ( Date.month date )) ++ " - " ++
                               ( toString ( Date.day date )))
        ]


---- Util ----
getState : Item -> ItemState.Model
getState item =
  case item of
    EmailItem state context model -> state
    ReminderItem state context model -> state


getDate : Item -> Date
getDate item =
  case item of
    EmailItem state context model -> model.date
    ReminderItem state context model -> model.date


sortOrderDefault : Item -> Item -> Order
sortOrderDefault item_a item_b =
  let
    item_a_state = getState item_a
    item_b_state = getState item_b

    date_a = getDate item_a
    date_b = getDate item_b
  in
    if item_a_state.pinned && not item_b_state.pinned then
      LT
    else if item_b_state.pinned && not item_a_state.pinned then
      GT
    else
      case compare date_a date_b of
        LT -> GT
        EQ -> EQ
        GT -> LT


sortOrderOldTop : Item -> Item -> Order
sortOrderOldTop item_a item_b =
  let
    date_a = getDate item_a
    date_b = getDate item_b
  in
    compare date_a date_b


