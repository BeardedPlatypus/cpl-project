module Css where

import Html exposing ( Html
                     , Attribute
                     , node
                     )
import Html.Attributes exposing ( style
                                , type'
                                , class
                                )
---- Colour Definitions ----
red    = "#703030"
blue   = "#2F343B"
grey   = "#7E827A"
yellow = "#E3CDA4"
orange = "#C77966"


---- Html definitions ----
--------------------------------------------------------------------------------
-- Containers
header : List Html -> Html
header elements =
  Html.header [ ]
        [ Html.div [ class "apphead" ]
                   [ container_ [ Html.div [ class "apphead-title" ] elements ] ]
        ]

body : List Html -> Html
body elements =
  Html.body [ ] [ stylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.5.0/css/font-awesome.min.css"
                , stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"
                , script "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"
                , stylesheetFont "https://fonts.googleapis.com/css?family=Source+Sans+Pro:400,300,600,700"
                , stylesheet "css/drunken-parrot.css"
                , stylesheet "css/itemfeed.css"
                , container_ elements
                ]

footer : List Html -> Html
footer elements =
  Html.footer [] elements

appContainer : List Html -> Html
appContainer elements =
  Html.div [ ] elements

appSection : List Html -> Html
appSection elements =
  Html.section [ ] elements

itemFeedSection : List Html -> Html
itemFeedSection elements =
  Html.section [ ] elements

panel : String -> List Html -> Html
panel panel_header elements =
  let
    div = Html.div
  in
    div
      [ class "panel panel-default" ]
      [ div [ class "panel-heading" ] [ Html.text panel_header ]
      , div [ class "panel-body" ] elements
      ]

sectionApp : String -> List Html -> Html
sectionApp header_text elements =
  let
    div = Html.div
    br = Html.br [] []
  in
    div
      [ class "componant-section" ]
      [ h2 [ Html.text header_text ]
      , div [] elements
      , br
      ]


panelEmail : Bool -> List Html -> List Html -> List Html -> Html
panelEmail is_focus panel_header panel_content panel_footer =
  let
    panel_class = if is_focus then "panel panel-danger"
                  else "panel panel-default"
    div = Html.div
  in
    div [ class panel_class ]
        [ div [ class "panel-heading" ]
              panel_header
        , div [ class "panel-body" ]
              panel_content
        , div [ class "panel-footer" ]
              panel_footer
        ]

panelReminder : Bool -> List Html -> List Html -> Html
panelReminder is_focus panel_content panel_footer =
  let
    panel_class = if is_focus then "panel panel-danger"
                  else "panel panel-default"
    div = Html.div
  in
    div [ class panel_class ]
        [ div [ class "panel-heading" ]
              []
        , div [ class "panel-body" ]
              panel_content
        , div [ class "panel-footer" ]
              panel_footer
        ]

--------------------------------------------------------------------------------
-- Headers
h1 : List Html -> Html
h1 elements =
  Html.div [ ] [ Html.h1 [ ] elements
               , Html.hr [ ] [ ]
               ]

h2 : List Html -> Html
h2 elements =
  Html.h2 [ class "app-section-title" ] elements

h4 : List Html -> Html
h4 elements = Html.h4 [ ] elements

h5 : List Html -> Html
h5 elements = Html.h5 [ ] elements

--------------------------------------------------------------------------------
-- Input
textInput : List Attribute -> List Html -> Html
textInput input_attributes elements =
  let
    input_styling = [ type' "text"
                    , class "form-control"
                    ]
  in
    Html.input ( input_attributes ++ input_styling ) elements

dateInput : List Attribute -> List Html -> Html
dateInput input_attributes elements =
  let
    input_styling = [ type' "date"
                    , class "form-control"
                    ]
  in
    Html.input ( input_attributes ++ input_styling ) elements

button : List Attribute -> List Html -> Html
button input_attributes elements =
  let
    input_styling = [ class "btn btn-block btn-primary" ]
  in
    Html.button ( input_attributes ++ input_styling ) elements


---- Css definitions ----
-- column layout

stylesheet : String -> Html
stylesheet href =
  node "link" [ Html.Attributes.rel "stylesheet"
              , Html.Attributes.href href
              ] []

stylesheetFont : String -> Html
stylesheetFont href =
  node "link" [ Html.Attributes.rel "stylesheet"
              , Html.Attributes.href href
              , Html.Attributes.type' "text/css"
              ] []

script : String -> Html
script href =
  node "script" [ Html.Attributes.src href ] []

container_ : List Html -> Html
container_ = Html.div [ class "container" ]

row_ : List Html -> Html
row_ = Html.div [ class "row" ]


{-
columnCenter : List ( String, String )
columnCenter = [ ( "position", "relative" )
               , ( "min-height", "1px" )
               , ( "padding-left", "15px" )
               , ( "padding-right", "15px" )
               ]

columnOffset : List ( String, String )
columnOffset = [ ("margin-left", "16.66666%")]

-- text alignment
textCenter : List ( String, String )
textCenter = [ ( "text-align", "center" ) ]

-- backgrounds
backgroundBody : List ( String, String )
backgroundBody = [ ( "background-color", grey ) ]

backgroundItem : List ( String, String )
backgroundItem = [ ( "background-color", yellow ) ]

bodyCss : List ( String, String )
bodyCss = [ ( "font-family", "\"SourceSansPro\", Helvetica, Arial, sans-serif" )
          , ( "font-size", "16px" )
          , ( "line-height", "1.5" )
          , ( "background-color", grey )
          ]
-}
