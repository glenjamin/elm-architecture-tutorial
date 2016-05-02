module CounterList (..) where

import Dict exposing (Dict)
import Counter
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)


-- MODEL


type alias Model =
  { counters : Dict ID Counter.Model
  , nextID : ID
  }


type alias ID =
  Int


init : Model
init =
  { counters = Dict.empty
  , nextID = 0
  }



-- UPDATE


type Action
  = Insert
  | Remove
  | Modify ID Counter.Action


update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let
        newCounters =
          Dict.insert model.nextID (Counter.init 0) model.counters
      in
        { model
          | counters = newCounters
          , nextID = model.nextID + 1
        }

    Remove ->
      let
        key =
          Dict.keys model.counters |> List.foldl min 99999999
      in
        { model | counters = Dict.remove key model.counters }

    Modify id counterAction ->
      let
        modification =
          Maybe.map <| Counter.update counterAction
      in
        { model | counters = Dict.update id modification model.counters }



-- VIEW


view : Signal.Address Action -> Model -> Html
view address model =
  let
    counters =
      Dict.map (viewCounter address) model.counters
        |> Dict.toList
        |> List.sortBy fst
        |> List.map snd
        |> List.reverse

    remove =
      button [ onClick address Remove ] [ text "Remove" ]

    insert =
      button [ onClick address Insert ] [ text "Add" ]
  in
    div [] ([ remove, insert ] ++ counters)


viewCounter : Signal.Address Action -> ID -> Counter.Model -> Html
viewCounter address id model =
  div
    [ style [ ( "overflow", "hidden" ) ] ]
    [ (text <| toString id)
    , Counter.view (Signal.forwardTo address (Modify id)) model
    ]
