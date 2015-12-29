module ReminderInput ( Model, defaultModel           -- Model
                     , Action, getNoAction, update   -- Update
                     , view                          -- View
                     ) where


import Html exposing ( Html
                     , Attribute
                     )
import Html.Attributes exposing ( type'
                                , value
                                , name
                                )
import Html.Events exposing ( .. )
import Json.Decode as Json


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
            | AddReminder


getNoAction : Action
getNoAction = NoAction


update : Action -> Model -> Model
update action model =
  case action of
    NoAction -> model
    UpdateBody body -> { model | input_body <- body }
    UpdateDate date -> { model | input_date <- date }
    AddReminder -> defaultModel


---- View ----
view : Signal.Address Action -> Model -> Html
view address model =
  let
    div = Html.div []
    input = Html.input
    button = Html.button
  in
    div [ input [ type' "text"
                , name "reminder-body"
                , value model.input_body
                , on "input" targetValue ( Signal.message address << UpdateBody )
                , onEnter address AddReminder
                ] []
        , input [ type' "date"
                , name "reminder-date"
                , value model.input_date
                , on "input" targetValue ( Signal.message address << UpdateDate )
                ] []
        , button [ onClick address AddReminder ] [ Html.text "Add" ]
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



