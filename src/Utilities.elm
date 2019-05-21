module Utilities exposing (..)

import MazeTypes exposing (..)
import Grid 
import Maze exposing (..)


verifyMaze : Maze -> Bool
verifyMaze maze = 
    let (rows, cols) = Grid.dims maze in
    verifyMazeCoords maze (rows - 1, cols - 1)



verifyMazeCoords : Maze -> Coordinate -> Bool
verifyMazeCoords maze coord = 
    case coord of 
        (0, 0) -> verifyNodeEdges maze coord 
        (r, c) -> 
            case Grid.get coord maze of 
                Nothing -> True
                Just _  -> (verifyNodeEdges maze coord)
                           && (verifyMazeCoords maze (r - 1, c))
                           && (verifyMazeCoords maze (r, c - 1))



verifyNodeEdges : Maze -> Coordinate -> Bool 
verifyNodeEdges maze coord =
    let coordHasPath_ = coordHasPath maze in 
    case Grid.get coord maze of 
        Nothing   -> False
        Just node -> 
            (hasPath node N == coordHasPath_ (move coord N) (oppositeDir N))
            && (hasPath node E == coordHasPath_ (move coord E) (oppositeDir E))
            && (hasPath node W == coordHasPath_ (move coord W) (oppositeDir W))
            && (hasPath node S == coordHasPath_ (move coord S) (oppositeDir S))


     


