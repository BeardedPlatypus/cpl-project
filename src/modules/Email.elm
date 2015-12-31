module Email ( Model, Context, initContext            -- Model
             , Action( NoAction                       -- Update
                     , ToggleDisplayBody
                     )
             , updateContext
             , viewHeader, viewBody, viewDisplayAll   -- View
             ) where

-- Imports
import Date exposing ( Date )
import String

import Html exposing ( Html )
import Html.Events exposing ( onClick )
import Html.Attributes exposing ( class )

import Css


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


updateContext : Action -> Context -> Context
updateContext action context =
  case action of
    NoAction -> context
    ToggleDisplayBody -> not context --{ context | display_all <- not context.display_all }


-- View
viewHeader : Model -> Html
viewHeader model =
  let
    div = Html.div
    text = Html.text
  in
    Css.row_ [ div [ class "col-sm-6" ] [ Css.h5 [ text model.title ] ]
             , div [ class "col-sm-6" ] [ Css.h4 [ Html.small [] [ text model.from
                                                                 , text " says: "
                                                                 ]
                                                 ]
                                        ]
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
      Just ( Css.button [ onClick address ToggleDisplayBody ] [ text button_text ] )
  else
    Nothing


-- State
mailbox : Signal.Mailbox Action
mailbox = Signal.mailbox NoAction


state : Signal Context
state = Signal.foldp updateContext initContext mailbox.signal
