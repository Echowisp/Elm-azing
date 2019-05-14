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
type GameState = AIVictory | PlayerVictory | Started | Stopped

type alias Model =
    {
        playerMaze : Maze
        aiMaze : Maze
        player : Coordinate
        ai : Coordinate
        difficulty : Difficulty
        winner : Player
        gameState : GameState
        winningCoord : Coordinate
    }

initModel =
    {
        playerMaze = walledMaze 0 0
        aiMaze = walledMaze 0 0
        player = (0,0)
        ai = (0,0)
        difficulty = Easy
        winner = None
        gameState = Stopped
        winningCoord = (10, 10)
    }


-- UPDATE

type Msg = MoveN | MoveS | MoveE | MoveW | DiffEasy | DiffMed | DiffHard | Gen |
           NoOp

updatePlayerCoord : Model -> Direction -> Model
updatePlayerCoord mdl dir =
    -- Player should only move if came is started
    if mdl.gameState == Started then
        let
            newCoord = (Maze.movePlayer mdl.player dir mdl.playerMaze)
        in
        {
            mdl |
            player = newCoord,
            -- Check if player has won
            gameState =
                if newCoord == mdl.winningCoord then
                    PlayerVictory
                else
                    Started
        }
    else
        mdl

updateDifficulty : Model -> Difficulty -> Model
updateDifficulty mdl newDiff =
    --Only allow difficulty updates if game is stopped
    if mdl.gameState != Started then
        { mdl | difficulty = newDiff }
    else
        mdl

generateMazes : Model -> Model
generateMazes mdl =
    Debug.todo "???"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MoveN -> (updatePlayerCoord model N, Cmd.none)
        MoveS -> (updatePlayerCoord model S, Cmd.none)
        MoveE -> (updatePlayerCoord model E, Cmd.none)
        MoveW -> (updatePlayerCoord model W, Cmd.none)
        DiffEasy -> (updateDifficulty model Easy, Cmd.none)
        DiffMed -> (updateDifficulty model Medium, Cmd.none)
        DiffHard -> (updateDifficulty model Hard, Cmd.none)
        Gen -> (generateMazes model, Cmd.none)
        NoOp -> (model, Cmd.none) --I don't know if this is useful


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