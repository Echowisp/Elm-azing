module MazeTypes exposing (..)

import Dict exposing (Dict)
import Random exposing (..)
import Array exposing (Array)

type alias Coordinate = (Int, Int)
type alias DEdge = (Coordinate, Coordinate)
type alias History = List Coordinate

type Direction = N | E | W | S

type Difficulty = Easy | Medium | Hard
type Player = Human | AI | None
type GameState = AIVictory | PlayerVictory | Started | Stopped

type alias Grid a = Array (Array a)
type alias Maze = Grid Node

type alias JunctionMap = Dict DEdge Int


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
        junctions : JunctionMap,
        forward : Bool
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


orthogonalDirs : Direction -> List Direction
orthogonalDirs dir =
    case dir of 
        N -> [E, W]
        E -> [N, S]
        W -> [N, S]
        S -> [S, N]


-- Called with the assumption that the two coordinates
-- are adjacent
--
-- For coordinates a and b returns a tuple of the form of the directions
-- (a -> b, b -> a)
relativePosition : Coordinate -> Coordinate -> (Direction, Direction)
relativePosition (r1, c1) (r2, c2) =
    if r1 - r2 /= 0 then
        if r1 - r2 < 0 then
            (S, N)
        else
            (N, S)
    else
        if c1 - c2 < 0 then
            (E, W)
        else
            (W, E)