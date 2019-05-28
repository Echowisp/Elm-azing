module MazeTypes exposing (..)

import Dict exposing (Dict)
import Random exposing (..)
import Array exposing (Array)

type alias Coordinate = (Int, Int)
type alias History = List Coordinate

type Direction = N | E | W | S

type Difficulty = Easy | Medium | Hard
type Player = Human | AI | None
type GameState = AIVictory | PlayerVictory | Started | Stopped

type alias Grid a = Array (Array a)
type alias Maze = Grid Node

type alias JunctionMap = Dict Coordinate (List Direction)


type alias Model =
    {
        playerMaze : Maze,
        aiMaze : Maze,
        player : Coordinate,
        ai : Coordinate,
        aiState : AIState,
        difficulty : Difficulty,
        winner : Player,
        gameState : GameState,
        winningCoord : Coordinate,
        seed : Seed
    }

type alias Node =
    { n : Bool
    , e : Bool
    , w : Bool
    , s : Bool
    }

type alias AIState =
    {
        dir : Direction,
        seed : Random.Seed,
        hist : History,
        junctions : JunctionMap 
    }

mazeSize = 30 -- Maze dimensions will be mazeSize x mazeSize

cardinals : List Direction
cardinals = [N, E, W, S]

oppositeDir : Direction -> Direction
oppositeDir dir =
    case dir of
        N -> S
        E -> W
        W -> E
        S -> N