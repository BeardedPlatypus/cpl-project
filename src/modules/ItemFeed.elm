module ItemFeed ( Model, ID             -- Model
                , Action( NoAction      -- Update
                        , NextItem
                        , PreviousItem
                        , UpdateItem
                        )
                , update
                , addReminder
                , view                  -- View
                ) where


---- Imports ----
import Item
import Reminder

import Date exposing ( Date )
import Html exposing ( Html )


---- Model ----
type alias ID = Int
type alias Model = { items : List (ID, Item.Item)
                   , nextId : ID
                   , focus : ID
                   , sortComparison : ( Item.Item -> Item.Item -> Order )
                   }

---- Update ----
type Action
  = NoAction
  | NextItem
  | PreviousItem
  | UpdateItem ID Item.Action


update : Action -> Model -> Model
update action model =
  case action of
    NoAction -> model
    NextItem -> model
    PreviousItem -> model
    UpdateItem id itemAction ->
      let
        updateItem ( itemID, item ) =
          if id == itemID then
            ( itemID, Item.update itemAction item )
          else
            ( itemID, item )
      in
        { model | items <- List.map updateItem model.items }


addReminder : Reminder.Model -> Model-> Model
addReminder reminder_model model =
  { model | items <- ( model.nextId,
                       Item.initReminder reminder_model ) :: model.items
          , nextId <- model.nextId + 1
  }


---- View ----
view : Signal.Address Action -> Model -> Html
view address model =
  let
    div = Html.div []
    todo_section = viewSection address model False
    done_section = viewSection address model True
  in
    div (recursiveBuildView ( todo_section :: done_section :: [] ))


-- view a single to do section
-- whether ToDo or Done is displayed can be chosen with is_done : Bool
viewSection: Signal.Address Action -> Model -> Bool -> Maybe Html
viewSection address model is_done =
  let
    div = Html.div []
    text = Html.text

    -- Header tex
    header_text = if is_done then "Done" else "To do"
    section_items = getOrderedSectionItems model is_done
  in
    if not ( List.isEmpty section_items ) then
      Just ( div
               -- Header
               [ div [ text header_text ]
               -- Items
               , div ( List.map ( viewItem address ) section_items )
               ]
           )
    else
      Nothing


recursiveBuildView : List ( Maybe Html ) -> List Html
recursiveBuildView list = recursiveBuildViewAcc ( List.reverse list ) []

recursiveBuildViewAcc : List ( Maybe Html ) -> List Html -> List Html
recursiveBuildViewAcc list acc =
  case list of
    -- Traverse list to obtain all real elements
    head :: tail ->
      case head of
        Just element -> recursiveBuildViewAcc tail (element :: acc)
        Nothing      -> recursiveBuildViewAcc tail acc
    -- Return accumulator
    [] -> acc


viewItem : Signal.Address Action -> ( ID, Item.Item ) -> Html
viewItem address ( id, item ) =
  Item.view (Signal.forwardTo address (UpdateItem id)) item


---- Util ----
sortItemId : (Item.Item -> Item.Item -> Order) -> (ID, Item.Item) -> (ID, Item.Item) -> Order
sortItemId sortOrder ( id_a, item_a ) ( id_b, item_b ) = sortOrder item_a item_b

getOrderedSectionItems : Model -> Bool -> List (ID, Item.Item)
getOrderedSectionItems model is_done =
  let
    filter_section = \( id, item ) -> ( is_done == ( Item.getState item ).is_done )
    section_items = List.filter filter_section model.items
  in
    if not ( List.isEmpty section_items ) then
      List.sortWith ( sortItemId model.sortComparison ) section_items
    else
      []


