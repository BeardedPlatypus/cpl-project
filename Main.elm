module Main where

-- import ItemState
import Reminder
import Email

import Item
import ItemFeed
import ReminderInput

import App
import AppActions
import KeyboardInput

import Html exposing ( Html )

import Date exposing ( Date )
import Signal

-- Name: Martinus Wilhelmus Tegelaers
-- Student ID: r0382389


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary: 


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary:

{- Test Itemstate 
mailbox : Signal.Mailbox ItemState.Action
mailbox = Signal.mailbox ItemState.getNoAction

state : Signal ItemState.Model
state = Signal.foldp ItemState.update ItemState.init mailbox.signal

view : ItemState.Model -> Html
view model =
  let
    div = Html.div []
  in
    div (ItemState.viewButtons mailbox.address model)


-- Start of program
main : Signal Html.Html
main = Signal.map view state
--Signal.constant ( Item.view (Item.initReminder Reminder.init) )
-}

{- Test Email
mailbox : Signal.Mailbox Email.Action
mailbox = Signal.mailbox Email.getNoAction

state : Signal Email.Context
state = Signal.foldp Email.updateContext Email.initContext mailbox.signal
-}

emailModel : Email.Model
emailModel = { from = "bossman@corporate.me"
             , to = "manager@corporate.me"
             , title = "Corporate Ipsum"
             , body = """Collaboratively administrate empowered markets via plug-and-play
                         networks. Dynamically procrastinate B2C users after installed base
                         benefits. Dramatically visualize customer directed convergence without
                         revolutionary ROI.

                         Efficiently unleash cross-media information without cross-media
                         value. Quickly maximize timely deliverables for real-time
                         schemas. Dramatically maintain clicks-and-mortar solutions
                         without functional solutions.

                         Completely synergize resource taxing relationships via premier
                         niche markets. Professionally cultivate one-to-one customer
                         service with robust ideas. Dynamically innovate
                         resource-leveling customer service for state of the art customer
                         service."""
             , date = withDefault (Date.fromTime 0) (Date.fromString "2015-01-30")
             }

emailModel1 : Email.Model
emailModel1 = { from = "hello@test.me"
              , to = "goodbye@test.me"
              , title = "Shorter than 200"
              , body = """This is the body of an email with less than 200 characters."""
              , date = withDefault (Date.fromTime 0) (Date.fromString "2015-09-30")
              }

reminderModel1 : Reminder.Model
reminderModel1 = { body = "Take out the thrash"
                 , date = withDefault (Date.fromTime 0 ) (Date.fromString "2016-09-30")
                 }


reminderModel2 : Reminder.Model
reminderModel2 = { body = "Groceries"
                 , date = withDefault (Date.fromTime 0 ) (Date.fromString "2015-09-25")
                 }


itemList : List (ItemFeed.ID, Item.Item)
itemList = [ ( 0, Item.initEmail emailModel1) 
           , ( 1, Item.initEmail emailModel )
           , ( 2, Item.initReminder reminderModel1 )
           , ( 3, Item.initReminder reminderModel2 )
           ]


itemFeedModel : ItemFeed.Model
itemFeedModel = { items = itemList
                , nextId = 4
                , focus = 2
                , sortComparison = Item.sortOrderDefault
                }
{-
view : Email.Context -> Html
view context =
  let
    div = Html.div []
    button = Email.viewDisplayAll mailbox.address emailModel context
  in
    case button of
      Just b -> b
      Nothing -> div []


--Start of program
main : Signal Html.Html
main = Signal.map view state
-}

{- Test Reminder item
mailbox : Signal.Mailbox Item.Action
mailbox = Signal.mailbox Item.getNoAction

state : Signal Item.Item
state = Signal.foldp Item.update (Item.initReminder Reminder.init) mailbox.signal


view : Item.Item -> Html
view item =
  Item.view mailbox.address item


main : Signal Html.Html
main = Signal.map view state
-}

{-
mailbox : Signal.Mailbox Item.Action
mailbox = Signal.mailbox Item.getNoAction

state : Signal Item.Item
state = Signal.foldp Item.update (Item.initEmail emailModel) mailbox.signal

view : Item.Item -> Html
view item =
  Item.view mailbox.address item

main : Signal Html.Html
main = Signal.map view state
-}

{-
mailbox : Signal.Mailbox ItemFeed.Action
mailbox = Signal.mailbox ItemFeed.getNoAction

state : Signal ItemFeed.Model
state = Signal.foldp ItemFeed.update itemFeedModel mailbox.signal

view : ItemFeed.Model -> Html
view model =
  ItemFeed.view mailbox.address model

main : Signal Html.Html
main = Signal.map view state
-}

withDefault : a -> Result x a -> a
withDefault default result =
  Maybe.withDefault default (Result.toMaybe result)

{-
mailbox : Signal.Mailbox ReminderInput.Action
mailbox = Signal.mailbox ReminderInput.getNoAction

state : Signal ReminderInput.Model
state = Signal.foldp ReminderInput.update ReminderInput.defaultModel mailbox.signal


view : ReminderInput.Model -> Html
view model =
  ReminderInput.view mailbox.address model

main : Signal Html.Html
main = Signal.map view state
-}

itemFeedAppModel = { item_feed = itemFeedModel
                   , reminder_input = ReminderInput.defaultModel
                   , keyboard_input = KeyboardInput.init
                   }


mailbox : Signal.Mailbox AppActions.Action
mailbox = Signal.mailbox AppActions.NoAction

state : Signal App.Model
state = Signal.foldp App.update itemFeedAppModel (Signal.merge mailbox.signal KeyboardInput.keyboardSignal )

view : App.Model -> Html
view model = App.view mailbox.address model


main : Signal Html.Html
main = Signal.map view state
