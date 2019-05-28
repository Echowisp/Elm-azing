module Elmazing exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Maze exposing (..)
import Grid exposing (..)
import Render exposing (..)
import MazeTypes exposing (..)
import RandomDFS exposing (..)
import RecursiveDivision exposing (..)
import MazeAI exposing (..)
import Time exposing (..)
import Random exposing (..)
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
        aiState = {dir = N, seed = seed0},
        difficulty = Easy,
        winner = None,
        gameState = Stopped,
        winningCoord = (29, 29),
        seed = seed0
    }


-- UPDATE

type Msg = MoveN | MoveS | MoveE | MoveW | DiffEasy | DiffMed | DiffHard | Gen |
           NoOp | AIMove | Tick Posix

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
    let
        sd = mdl.seed
        aiSd = mdl.aiState.seed
    in
    if mdl.gameState == Started then
        mdl
    else
        {
            initModel | playerMaze = RecursiveDivision.buildMaze mazeSize mazeSize sd,
                        aiMaze = RecursiveDivision.buildMaze mazeSize mazeSize aiSd,
                        gameState = Started,
                        difficulty = mdl.difficulty
        }

makeAIMove : Model -> Model
makeAIMove mdl =
    if mdl.gameState /= Started then
        mdl
    else
        case mdl.difficulty of
            Easy -> bruteForceAI mdl
            Medium -> Debug.todo "???"
            Hard -> Debug.todo "???"

updateSeed : Model -> Int -> Model
updateSeed mdl nseed =
    let
        ai = mdl.aiState
    in
    {
        mdl |
            seed = initialSeed nseed,
            aiState = { ai | seed = initialSeed -nseed }
    }

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
        AIMove -> (makeAIMove model, Cmd.none)
        Tick x -> (updateSeed model (posixToMillis x), Cmd.none)
        NoOp -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    BGrid.containerFluid []
    [ BGrid.simpleRow
        [ BGrid.col
            [ Col.xs3, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffEasy ] [ text "Easy" ] ]
        , BGrid.col
            [ Col.xs3, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffMed ] [ text "Medium" ] ]
        , BGrid.col
            [ Col.xs3, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.primary, Button.onClick DiffHard ] [ text "Hard" ] ]
        , BGrid.col
            [ Col.xs3, Col.textAlign Text.alignXsCenter ]
            [ Button.button [ Button.success, Button.onClick Gen] [ text "Go!"] ]
        ]
    , BGrid.simpleRow
        [ BGrid.col
            [ Col.xs12, Col.textAlign Text.alignXsCenter ]
            [ render model ]
        ]
    ]

-- SUBSCRIPTIONS

-- https://github.com/elm/browser/blob/1.0.0/notes/keyboard.md
keyDecoder : Decode.Decoder String
keyDecoder =
  Decode.field "key" Decode.string

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch <|
    [ Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "w" then MoveN else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "s" then MoveS else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "d" then MoveE else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "a" then MoveW else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "ArrowUp" then MoveN else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "ArrowDown" then MoveS else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "ArrowRight" then MoveE else NoOp) keyDecoder)
    , Browser.Events.onKeyDown
        (Decode.map (\key -> if key == "ArrowLeft" then MoveW else NoOp) keyDecoder)
    , Time.every 1000 (\_ -> AIMove)
    , Time.every 1000 (\x -> Tick x)
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