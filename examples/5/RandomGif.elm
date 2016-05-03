module RandomGif (..) where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json
import Task


-- MODEL


type alias Model =
  { topic : String
  , gifUrl : String
  }


waiting : String
waiting =
  "assets/waiting.gif"


init : String -> ( Model, Effects Action )
init topic =
  ( Model topic waiting
  , getRandomGif topic
  )



-- UPDATE


type Action
  = RequestMore
  | PureAction PureAction


type PureAction
  = NewGif (Maybe String)


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    RequestMore ->
      ( { model | gifUrl = waiting }, getRandomGif model.topic )

    PureAction pureAction ->
      ( simpleUpdate pureAction model, Effects.none )


simpleUpdate : PureAction -> Model -> Model
simpleUpdate action model =
  case action of
    NewGif Nothing ->
      model

    NewGif (Just url) ->
      Model model.topic url



-- VIEW


(=>) : a -> b -> ( a, b )
(=>) =
  (,)


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ "width" => "200px" ] ]
    [ h2 [ headerStyle ] [ text model.topic ]
    , div [ imgStyle model.gifUrl ] []
    , button [ onClick address RequestMore ] [ text "More Please!" ]
    ]


headerStyle : Attribute
headerStyle =
  style
    [ "width" => "200px"
    , "text-align" => "center"
    ]


imgStyle : String -> Attribute
imgStyle url =
  style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "contain"
    , "background-repeat" => "no-repeat"
    , "background-image" => ("url('" ++ url ++ "')")
    ]



-- EFFECTS


getRandomGif : String -> Effects Action
getRandomGif topic =
  Http.get decodeUrl (randomUrl topic)
    |> Task.toMaybe
    |> Task.map (PureAction << NewGif)
    |> Effects.task


randomUrl : String -> String
randomUrl topic =
  Http.url
    "http://api.giphy.com/v1/gifs/random"
    [ "api_key" => "dc6zaTOxFJmzC"
    , "tag" => topic
    ]


decodeUrl : Json.Decoder String
decodeUrl =
  Json.at [ "data", "image_url" ] Json.string
