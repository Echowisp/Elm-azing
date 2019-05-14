module Maze exposing (..)

import Grid exposing (..)
import Maybe

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


addEdge_ : Node -> Grid.Direction -> Node 
addEdge_ node dir = 
    case dir of 
        N -> {node | n = True} 
        E -> {node | e = True} 
        W -> {node | w = True} 
        S -> {node | s = True} 


addEdge : Coordinate -> Grid.Direction -> Maze Node -> Maze Node 
addEdge coord dir maze = 
    let 
        node = Grid.get coord maze 
                |> Maybe.map (\node_ -> addEdge_ node_ dir)
    in 
        case node of 
            Nothing      -> maze 
            Just newNode -> Grid.set coord newNode maze 



createEdge : Coordinate -> Coordinate -> Maze Node -> Maze Node 
createEdge coord1 coord2 maze = 
    if Grid.verifyAdjacent coord1 coord2 then 
        case relativePosition coord1 coord2 of 
            (d1, d2) -> 
                addEdge coord1 d1 maze 
                |> addEdge coord2 d2 
    else 
        maze