module MazeTypes exposing (..)

import Array exposing (Array)

type alias Coordinate = (Int, Int)

type Direction = N | E | W | S

type Difficulty = Easy | Medium | Hard
type Player = Human | AI | None
type GameState = AIVictory | PlayerVictory | Started | Stopped

type alias Grid a = Array (Array a)
type alias Maze = Grid Node

type alias Model =
    {
        playerMaze : Maze,
        aiMaze : Maze,
        player : Coordinate,
        ai : Coordinate,
        difficulty : Difficulty,
        winner : Player,
        gameState : GameState,
        winningCoord : Coordinate
    }

type alias Node =
    { n : Bool
    , e : Bool
    , w : Bool
    , s : Bool
    }

mazeSize = 30 -- Maze dimensions will be mazeSize x mazeSize

oppositeDir : Direction -> Direction
oppositeDir dir =
    case dir of
        N -> S
        E -> W
        W -> E
        S -> N