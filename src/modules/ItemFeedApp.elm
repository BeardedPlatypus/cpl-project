module ItemFeedApp ( Model                         -- Model
                   , Action( NoAction              -- Update
                           , UpdateItemFeed
                           , UpdateReminderInput
                           )
                   , update
                   , view                          -- View
                   ) where


---- Import ----
import ItemFeed
import ReminderInput

import Html exposing ( Html )


---- Model ----
type alias Model = { item_feed : ItemFeed.Model
                   , reminder_input : ReminderInput.Model
                   }


---- Update ----
type Action
  = NoAction
  | UpdateItemFeed ItemFeed.Action
  | UpdateReminderInput ReminderInput.Action


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


---- View ----
-- Main view
view : Signal.Address Action -> Model -> Html
view address model =
  let
    div = Html.div []
  in
    div [ viewItemFeed address model.item_feed
        , viewReminderInput address model.reminder_input
        ]


-- Element views
viewItemFeed : Signal.Address Action -> ItemFeed.Model -> Html
viewItemFeed address model =
  ItemFeed.view ( Signal.forwardTo address UpdateItemFeed  ) model


viewReminderInput : Signal.Address Action -> ReminderInput.Model -> Html
viewReminderInput address model =
  ReminderInput.view ( Signal.forwardTo address UpdateReminderInput ) model
