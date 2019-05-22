module Elmazing exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Maze exposing (..)
import Grid exposing (..)
import Render exposing (..)
import MazeTypes exposing (..)
import RandomDFS exposing (..)

-- MODEL

initModel =
    {
        playerMaze = buildMaze 30 30,
        aiMaze = walledMaze 30 30,
        player = (0,0),
        ai = (0,0),
        difficulty = Easy,
        winner = None,
        gameState = Stopped,
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
    if mdl.gameState /= Started then
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
view model = render model


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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