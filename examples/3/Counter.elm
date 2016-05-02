module Counter (..) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL


type alias Model =
  Int


init : Int -> Model
init =
  identity



-- UPDATE


type Action
  = Increment
  | Decrement


update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model + 1

    Decrement ->
      model - 1



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ divStyle ]
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


divStyle : Html.Attribute
divStyle =
  style
    [ ( "margin", "20px" )
    , ( "padding", "20px" )
    , ( "border", "1px solid black" )
    , ( "float", "left" )
    , ( "clear", "left" )
    ]


countStyle : Attribute
countStyle =
  style
    [ ( "font-size", "20px" )
    , ( "font-family", "monospace" )
    , ( "display", "inline-block" )
    , ( "width", "50px" )
    , ( "text-align", "center" )
    ]
