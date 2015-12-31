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
                                , class
                                )
import Html.Events exposing ( .. )
import Json.Decode as Json

import Date exposing ( Date )

import Css

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
    div = Html.div 

    reminder_model = { body = model.input_body
                     , date = withDefault ( Date.fromTime 0 ) ( Date.fromString model.input_date )
                     }

    input_body = Css.textInput [ name "reminder-body"
                               , value model.input_body
                               , on "input" targetValue ( Signal.message address << UpdateBody )
                               , onEnter address ( AddReminder reminder_model )
                               ] []
    input_date = Css.dateInput [ name "reminder-date"
                               , value model.input_date
                               , on "input" targetValue ( Signal.message address << UpdateDate )
                               ] []
    add_button = Css.button [ onClick address ( AddReminder reminder_model )
                            ] [ Html.text "Add" ]
  in
    Css.sectionApp
         "Reminder"
         [ Css.row_
              [ div [ class "form-group" ] 
                    [ div [ class "col-sm-9" ] [ input_body ]
                    , div [ class "col-sm-2" ] [ input_date ]
                    , div [ class "col-sm-1" ] [ add_button ]
                    ]
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



