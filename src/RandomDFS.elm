module RandomDFS exposing (..)

import Stack exposing (Stack)
import Random
import Set exposing (Set)
import MazeTypes exposing (..)
import Maze exposing (..)


buildMaze : Int -> Int -> Maze
buildMaze len wid =
    let
        initialMaze = walledMaze len wid
        visited = Set.empty |> Set.insert (0, 0)
        stack = Stack.initialise |> Stack.push (0, 0)
    in
        randDFS Maze.seed0 initialMaze visited stack Nothing


unvisitedNeighbors : Maze -> Set Coordinate -> Coordinate -> List Coordinate
unvisitedNeighbors maze visited coord =
    getNeighbors maze coord
    |> List.filter (\c -> not (Set.member c visited))


randDFS : Random.Seed -> Maze -> Set Coordinate -> Stack Coordinate -> Maybe Coordinate -> Maze
randDFS seed maze visited toVisit cur =
    case cur of
        Nothing    ->
            case Stack.pop toVisit of
                (Nothing, _)         -> maze -- finished
                (newCur, unvisited)  -> randDFS seed maze visited unvisited newCur
        Just coord ->
            case unvisitedNeighbors maze visited coord of
                []              ->
                    case Stack.pop toVisit of
                        (Nothing, _)         -> maze -- finished
                        (newCur, unvisited)  -> randDFS seed maze visited unvisited newCur
                unseenNeighbors ->
                    let
                        (randomNeighbor, newSeed) = stepNeighbor seed unseenNeighbors
                        updatedMaze = createEdge coord randomNeighbor maze
                        toSeeLater = Stack.push randomNeighbor toVisit
                        seen = Set.insert randomNeighbor visited
                    in
                        randDFS newSeed updatedMaze seen toSeeLater (Just randomNeighbor)



