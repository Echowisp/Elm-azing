module WallFollowAI exposing (..)

import Maze exposing (..)
import MazeTypes exposing (..)

wallFollowAI : Model -> Model
wallFollowAI mdl =
    let
        coord = mdl.ai
        maze = mdl.aiMaze
        aiDir = mdl.aiState.dir
        state = mdl.aiState
        nextDir = findDirection coord maze aiDir
        newCoord = movePlayer coord nextDir maze
    in
        {
            mdl | ai = newCoord,
                aiState = { state | dir = nextDir }
        }

nextDirection : List Direction -> Coordinate -> Maze -> Direction
nextDirection dirs coord maze =
    case dirs of
        first :: rst ->
            if validMove coord first maze then
                first
            else
                nextDirection rst coord maze
        [] ->
            Debug.todo "error in nextdirection"

findDirection : Coordinate -> Maze -> Direction -> Direction
findDirection coord maze dir =
    case dir of
        N -> nextDirection [E, N, W, S] coord maze
        E -> nextDirection [S, E, N, W] coord maze
        S -> nextDirection [W, S, E, N] coord maze
        W -> nextDirection [N, W, S, E] coord maze