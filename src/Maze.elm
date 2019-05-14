module Maze exposing (..)

import Grid exposing (..)
import Maybe

type alias Node =
    { n : Bool
    , e : Bool
    , w : Bool
    , s : Bool
    }

type alias Maze = Grid.Grid Node

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
{-- ========= Node Functions ================= --}

hasPath : Node -> Direction -> Bool
hasPath nd dir =
    case dir of
        N -> nd.n
        E -> nd.e
        S -> nd.s
        W -> nd.w

{-- ========= End Node Functions ================= --}


{-- ========= Coordinate Functions ================= --}

move : Coordinate -> Grid.Direction -> Coordinate
move (x, y) dir =
    case dir of
    N -> (x, y + 1)
    S -> (x, y - 1)
    E -> (x + 1, y)
    W -> (x - 1, y)

validMove : Coordinate -> Grid.Direction -> Maze a -> Bool
validMove coord dir g =
    case (get coord g) of
        Nothing -> False
        Just x -> hasPath x dir

movePlayer : Coordinate -> Grid.Direction -> Maze a -> Coordinate
movePlayer coord dir g =
    if (validMove coord dir g) then
        move coord dir
    else
        coord

{-- ========= End Coordinate Functions ================= --}