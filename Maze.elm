module Maze exposing (..)

import Grid

type alias Node = 
    { n : Bool
    , e : Bool
    , w : Bool
    , s : Bool 
    }

type alias Maze a = Grid.Grid a 


initialNode : Node 
initialNode =
    Node False False False False

walledMaze : Int -> Int -> Maze Node
walledMaze x y = 
    Grid.repeat x y initialNode 


--createEdge_ : Coordinate -> Coordinate -> Maze Node -> Maze Node

--createEdge : Coordinate -> Coordinate -> Maze Node -> Maze Node 