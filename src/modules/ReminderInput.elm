module ReminderInput ( Model, defaultModel  -- Model
                     , Action( NoAction     -- Update
                             , AddReminder
                             , UpdateBody
                             , UpdateDate
                             )
                     , update
                     , view                 -- View
                     ) where


import Reminder

import Html exposing ( Html
                     , Attribute
                     )
import Html.Attributes exposing ( type'
                                , value
                                , name
                                )
import Html.Events exposing ( .. )
import Json.Decode as Json

import Date exposing ( Date )

---- Model ----
type alias Model = { input_body : String
                   , input_date : String
                   }

defaultModel : Model
defaultModel =
  let
    defaultDate = "2015-01-30"
  in
    { input_body = ""
    , input_date = defaultDate
    }


---- Update ----
type Action = NoAction
            | UpdateBody String
            | UpdateDate String
            | AddReminder Reminder.Model


update : Action -> Model -> Model
update action model =
  case action of
    NoAction -> model
    UpdateBody body -> { model | input_body <- body }
    UpdateDate date -> { model | input_date <- date }
    AddReminder _ -> defaultModel


---- View ----
view : Signal.Address Action -> Model -> Html
view address model =
  let
    div = Html.div []
    input = Html.input
    button = Html.button
    text = Html.text

    reminder_model = { body = model.input_body
                     , date = withDefault ( Date.fromTime 0 ) ( Date.fromString model.input_date )
                     }
  in
    div [ div [ text "Reminder" ]
        , div [ input [ type' "text"
                       , name "reminder-body"
                       , value model.input_body
                       , on "input" targetValue ( Signal.message address << UpdateBody )
                       , onEnter address ( AddReminder reminder_model )
                       ] []
               , input [ type' "date"
                       , name "reminder-date"
                       , value model.input_date
                       , on "input" targetValue ( Signal.message address << UpdateDate )
                       ] []
               , button [ onClick address ( AddReminder reminder_model )
                        ] [ Html.text "Add" ]
               ]
        ]

withDefault : a -> Result x a -> a
withDefault default result =
  Maybe.withDefault default (Result.toMaybe result)


onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
  on "keydown"
     (Json.customDecoder keyCode is13)
     (\_ -> Signal.message address value)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"



