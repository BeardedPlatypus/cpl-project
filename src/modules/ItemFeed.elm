module ItemFeed ( Model, ID             -- Model
                , Action( NoAction      -- Update
                        , NextItem
                        , PreviousItem
                        , SetSortComparison
                        , UpdateItem
                        , UpdateFocus
                        )
                , update
                , addReminder
                , view                  -- View
                ) where


--------------------------------------------------------------------------------
---- Imports ----
--------------------------------------------------------------------------------
import Item
import Reminder

import Date exposing ( Date )
import Html exposing ( Html )
import Css


--------------------------------------------------------------------------------
---- Model ----
--------------------------------------------------------------------------------
type alias ID = Int
type alias Position = Int
type alias Model = { items : List (ID, Item.Item)
                   , nextId : ID
                   , focus : Position
                   , sortComparison : ( Item.Item -> Item.Item -> Order )
                   }

--------------------------------------------------------------------------------
---- Update ----
--------------------------------------------------------------------------------
type Action
  = NoAction
  | NextItem
  | PreviousItem
  | SetSortComparison ( Item.Item -> Item.Item -> Order )
  | UpdateItem ID Item.Action
  | UpdateFocus Item.Action


update : Action -> Model -> Model
update action model =
  case action of
    NoAction -> model
    NextItem -> changeFocus model Increment
    PreviousItem -> changeFocus model Decrement
    SetSortComparison sortOrder -> { model | sortComparison <- sortOrder }
    UpdateItem id itemAction ->
      let
        updateItem ( itemID, item ) =
          if id == itemID then
            ( itemID, Item.update itemAction item )
          else
            ( itemID, item )
      in
        { model | items <- List.map updateItem model.items }
    UpdateFocus itemAction -> update ( UpdateItem ( getIdFromFocus model ) itemAction ) model


--------------------------------------------------------------------------------
addReminder : Reminder.Model -> Model-> Model
addReminder reminder_model model =
  { model | items <- ( model.nextId,
                       Item.initReminder reminder_model ) :: model.items
          , nextId <- model.nextId + 1
  }


--------------------------------------------------------------------------------
type ChangeFocus = Increment
                 | Decrement

changeFocus : Model -> ChangeFocus -> Model
changeFocus model t =
  case t of
    Increment ->
      if ( model.focus + 1 ) < List.length model.items then
        { model | focus <- (model.focus + 1 ) }
      else
        { model | focus <- 0 }
    Decrement ->
      if ( model.focus - 1 ) >= 0 then
        { model | focus <- (model.focus - 1) }
      else
        { model | focus <- ( ( List.length model.items ) - 1 )}


getIdFromFocus : Model -> ID
getIdFromFocus model =
  let
    items_sorted = ( ( getOrderedSectionItems model False ) ++
                     ( getOrderedSectionItems model True) )
    possible_id = List.head ( List.drop model.focus items_sorted )
  in
    case possible_id of
      Just (id, _) -> id
      Nothing -> -1

--------------------------------------------------------------------------------
---- View ----
--------------------------------------------------------------------------------
view : Signal.Address Action -> Model -> Html
view address model =
  let
    div = Html.div []

    todo_items_raw = getOrderedSectionItems model False
    done_items_raw = getOrderedSectionItems model True

    n_todo = List.length todo_items_raw
    ( _, todo_items, done_items ) = List.foldl ( foldFunc model n_todo ) (0, [], []) (todo_items_raw ++ done_items_raw)

    todo_section = viewSection address "To Do" ( List.reverse todo_items )
    done_section = viewSection address "Done" ( List.reverse done_items )
  in
    Css.appSection (recursiveBuildView ( todo_section :: done_section :: [] ))

type alias FoldResult = ( Int, List ( ID, Item.Item, Bool ), List ( ID, Item.Item, Bool ) )
foldFunc : Model -> Int -> ( ID, Item.Item ) -> FoldResult -> FoldResult
foldFunc model n_todo (id, item) (count, todo, done) =
  if count < n_todo then
    ( count + 1, ( id, item, count == model.focus ) :: todo, done )
  else
    ( count + 1, todo, ( id, item, count == model.focus ) :: done )


--------------------------------------------------------------------------------
-- view a single to do section
-- whether ToDo or Done is displayed can be chosen with is_done : Bool
viewSection: Signal.Address Action -> String -> List (ID, Item.Item, Bool ) -> Maybe Html
viewSection address header_text items =
  let
    div = Html.div []
    text = Html.text

  in
    if not ( List.isEmpty items ) then
      Just (Css.sectionApp
                 header_text
               ( List.map ( viewItem address ) items )
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


viewItem : Signal.Address Action -> ( ID, Item.Item, Bool ) -> Html
viewItem address ( id, item, has_focus ) =
  Item.view (Signal.forwardTo address (UpdateItem id)) has_focus item


--------------------------------------------------------------------------------
---- Util ----
--------------------------------------------------------------------------------
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


