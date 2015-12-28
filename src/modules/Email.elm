module Email ( Model, Context, initContext           -- Model
             , Action, updateContext, getNoAction   -- Update
             , viewHeader, viewBody, viewDisplayAll  -- View
             ) where

-- Imports
import Date exposing ( Date )
import String

import Html exposing ( Html )
import Html.Events exposing ( onClick )


-- Constants
maxCharacters = 200


-- Model
type alias Model = { from : String
                   , to : String
                   , title : String
                   , body : String
                   , date : Date
                   }


type alias Context = Bool

initContext : Context
initContext = False

-- Update
type Action = NoAction
            | ToggleDisplayBody

getNoAction : Action
getNoAction = NoAction

updateContext : Action -> Context -> Context
updateContext action context =
  case action of
    NoAction -> context
    ToggleDisplayBody -> not context --{ context | display_all <- not context.display_all }


-- View
viewHeader : Model -> Html
viewHeader model =
  let
    div = Html.div []
    text = Html.text
  in
    div [ text model.title, text " | "
        , text model.from, text " says: "
        ]


viewBody : Model -> Context -> Html
viewBody model context =
  let
    div = Html.div []
    text = Html.text
  in
    if ((String.length model.body) <= maxCharacters || context) then
      div [ text model.body ]
    else
      div [ text (( String.left maxCharacters model.body ) ++ "..." )]


viewDisplayAll : Signal.Address Action -> Model -> Context -> Maybe Html
viewDisplayAll address model context =
  if ( String.length model.body ) > maxCharacters then
    let
      button_text = if context then "Less" else "More"

      text = Html.text
      button = Html.button
    in
      Just ( button [ onClick address ToggleDisplayBody ] [ text button_text ] )
  else
    Nothing


-- State
mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoAction


state : Signal Context
state = Signal.foldp updateContext initContext mailbox.signal
