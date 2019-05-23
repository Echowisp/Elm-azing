module Elmazing exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Maze exposing (..)
import Grid exposing (..)
import Render exposing (..)
import MazeTypes exposing (..)
import RandomDFS exposing (..)
import Bootstrap.Button as Button exposing (..)
import Bootstrap.Grid as BGrid exposing (..)
import Bootstrap.Grid.Col as Col exposing (..)
import Bootstrap.Grid.Row as Row exposing (..)
import Bootstrap.Text as Text exposing (..)
-- MODEL

initModel =
    {
        playerMaze = walledMaze mazeSize mazeSize,
        aiMaze = walledMaze mazeSize mazeSize,
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
    if mdl.gameState /= Started then
        { mdl | difficulty = newDiff }
    else
        mdl

generateMazes : Model -> Model
generateMazes mdl =
    case mdl.difficulty of
        Easy -> {mdl | playerMaze = Tuple.first <| RandomDFS.buildMaze mazeSize mazeSize}
        Medium -> {mdl | playerMaze = walledMaze mazeSize mazeSize}
        Hard -> {mdl | playerMaze = walledMaze mazeSize mazeSize}

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
    BGrid.containerFluid []
    [ BGrid.simpleRow
        [ BGrid.col
            [ Col.xs4, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffEasy ] [ text "Easy" ] ]
        , BGrid.col
            [ Col.xs4, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffMed ] [ text "Medium" ] ]
        , BGrid.col
            [ Col.xs4, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffHard ] [ text "Hard" ] ]
        ]
    , BGrid.simpleRow
        [ BGrid.col
            [ Col.xs12, Col.textAlign Text.alignXsCenter ]
            [ render model ]
        ]
    ]
    -- div []
    --     [
    --         Button.button [ Button.success, ] [ text "Primary" ],
    --         -- button [ onClick DiffEasy ] [ text "Easy" ],
    --         -- button [ onClick DiffMed ] [ text "Medium" ],
    --         -- button [ onClick DiffHard ] [ text "Hard" ],
    --         -- button [ onClick Gen ] [ text "Start" ],
    --         render model
    --     ]

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