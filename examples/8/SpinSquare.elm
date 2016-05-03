module SpinSquare (Model, Action, init, update, view) where

import Easing exposing (ease, easeOutBounce, float)
import Effects exposing (Effects)
import Html exposing (Html)
import Svg exposing (svg, rect, g, text, text')
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type alias Model =
  { angle : Float
  , animationState : AnimationState
  }


type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time, count : Int }


init : ( Model, Effects Action )
init =
  ( { angle = 0, animationState = Nothing }
  , Effects.none
  )


rotateStep : Float
rotateStep =
  90


duration : Time
duration =
  second



-- UPDATE


type Action
  = Spin
  | Tick Time


addCounter : Model -> Model
addCounter model =
  case model.animationState of
    Nothing ->
      model

    Just a ->
      { model | animationState = Just { a | count = a.count + 1 } }


update : Action -> Model -> ( Model, Effects Action )
update msg model =
  case msg of
    Spin ->
      case model.animationState of
        Nothing ->
          ( model, Effects.tick Tick )

        Just a ->
          ( addCounter model, Effects.none )

    Tick clockTime ->
      let
        count =
          Maybe.withDefault 0 (Maybe.map .count model.animationState)

        newElapsedTime =
          case model.animationState of
            Nothing ->
              0

            Just { elapsedTime, prevClockTime } ->
              elapsedTime + (clockTime - prevClockTime)

        fullAngle =
          model.angle + rotateStep
      in
        if newElapsedTime > duration then
          if count > 0 then
            ( { angle = fullAngle
              , animationState = Just { elapsedTime = 0, prevClockTime = clockTime, count = count - 1 }
              }
            , Effects.tick Tick
            )
          else
            ( { angle = fullAngle
              , animationState = Nothing
              }
            , Effects.none
            )
        else
          ( { angle = model.angle
            , animationState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime, count = count }
            }
          , Effects.tick Tick
          )



-- VIEW


toOffset : AnimationState -> Float
toOffset animationState =
  case animationState of
    Nothing ->
      0

    Just { elapsedTime } ->
      ease easeOutBounce float 0 rotateStep duration elapsedTime


view : Signal.Address Action -> Model -> Html
view address model =
  let
    angle =
      model.angle + toOffset model.animationState
  in
    svg
      [ width "200", height "200", viewBox "0 0 200 200" ]
      [ g
          [ transform ("translate(100, 100) rotate(" ++ toString angle ++ ")")
          , onClick (Signal.message address Spin)
          ]
          [ rect
              [ x "-50"
              , y "-50"
              , width "100"
              , height "100"
              , rx "15"
              , ry "15"
              , style "fill: #60B5CC;"
              ]
              []
          , text' [ fill "white", textAnchor "middle" ] [ text "Click me!" ]
          ]
      ]
