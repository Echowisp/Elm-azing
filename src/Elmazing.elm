module Elmazing exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Maze exposing (..)
import Grid exposing (Coordinate, Direction)

-- MODEL

type Difficulty = Easy | Medium | Hard
type Player = Human | AI | None

type alias Model = {
    maze : Maze
    player : Coordinate
    ai : Coordinate
    difficulty : Difficulty
    winner : Player
}

initModel =
    {
        maze = ??? //TODO
        player = (0,0)
        ai = (0,0)
        difficulty = Easy
        winner = None
    }


-- UPDATE

type Msg = Noop | Reset | Increment

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop -> (model, Cmd.none)
    Reset -> (initModel, Cmd.none)
    Increment -> ({ count = 1 + model.count }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    -- https://css-tricks.com/quick-css-trick-how-to-center-an-object-exactly-in-the-center/
    let styles =
          -- add more spaces if you get a "-- PARSE ERROR --"
          [ ("position", "fixed")
          , ("top", "50%")
          , ("left", "50%")
          , ("transform", "translate(-50%, -50%)")
          ]
    in
    let display = Html.text ("Count: " ++ Debug.toString model.count) in
    Html.div (List.map (\(k, v) -> Attr.style k v) styles) [display]


-- SUBSCRIPTIONS

-- https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onMouseDown (Decode.succeed Increment)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "Escape" then Reset else Noop) keyDecoder)
    ]


-- MAIN

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }