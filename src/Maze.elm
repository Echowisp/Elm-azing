module Maze exposing (..)

import Grid exposing (..)
import Maybe
import Random
import MazeTypes exposing (..)

initialNode : Node
initialNode =
    Node False False False False

openNode : Node 
openNode =
    Node True True True True

walledMaze : Int -> Int -> Maze
walledMaze x y =
    Grid.repeat x y initialNode


openMaze : Int -> Int -> Maze 
openMaze x y = 
    Grid.repeat x y openNode


getNode : Coordinate -> Maze -> Maybe Node
getNode coord maze =
    Grid.get coord maze


setNode : Coordinate -> Node -> Maze -> Maze
setNode coord newNode maze =
    Grid.set coord newNode maze


addEdge_ : Node -> MazeTypes.Direction -> Node
addEdge_ node dir =
    case dir of
        N -> {node | n = True}
        E -> {node | e = True}
        W -> {node | w = True}
        S -> {node | s = True}


addEdge : Coordinate -> MazeTypes.Direction -> Maze -> Maze
addEdge coord dir maze =
    let
        node = getNode coord maze
                |> Maybe.map (\node_ -> addEdge_ node_ dir)
    in
        case node of
            Nothing      -> maze
            Just newNode -> setNode coord newNode maze



createEdge : Coordinate -> Coordinate -> Maze -> Maze
createEdge coord1 coord2 maze =
    if Grid.verifyAdjacent coord1 coord2 then
        case relativePosition coord1 coord2 of
            (d1, d2) ->
                addEdge coord1 d1 maze
                |> addEdge coord2 d2
    else
        maze
{-- ========= Node Functions ================= --}

hasPath : Node -> MazeTypes.Direction -> Bool
hasPath nd dir =
    case dir of
        N -> nd.n
        E -> nd.e
        W -> nd.w
        S -> nd.s

{-- ========= End Node Functions ================= --}


{-- ========= Coordinate Functions ================= --}

move : Coordinate -> MazeTypes.Direction -> Coordinate
move (r, c) dir =
    case dir of
        N -> (r - 1, c)
        E -> (r, c + 1)
        W -> (r, c - 1)
        S -> (r + 1, c)

validMove : Coordinate -> MazeTypes.Direction -> Maze -> Bool
validMove coord dir g =
    case (get coord g) of
        Nothing -> False
        Just x  -> hasPath x dir

movePlayer : Coordinate -> MazeTypes.Direction -> Maze -> Coordinate
movePlayer coord dir g =
    if (validMove coord dir g) then
        move coord dir
    else
        coord

coordHasPath : Maze -> Coordinate -> Direction -> Bool
coordHasPath maze coord dir =
    case Grid.get coord maze of
        Nothing   -> False
        Just node -> hasPath node dir


selectMap : (Node -> Node) -> List Coordinate -> Maze -> Maze
selectMap f select maze = 
    case select of 
        []          -> maze 
        coord::rest -> 
            let 
                node =  Grid.get coord maze |> Maybe.withDefault openNode
            in
                selectMap f rest (Grid.set coord (f node) maze)

{-- ========= End Coordinate Functions ================= --}

getNeighbors_ : Coordinate -> List Coordinate
getNeighbors_ coord =
    [move coord N, move coord E, move coord W, move coord S]



getNeighbors : Maze -> Coordinate -> List Coordinate
getNeighbors maze coord =
    getNeighbors_ coord
    |> List.filter (validCoordinate maze)

validCoordinate : Maze -> Coordinate  -> Bool
validCoordinate maze (r, c) =
    let
        (len, wid) = dims maze
    in
        r < len && r >= 0 && c < wid && c >= 0


kth : Int -> List Coordinate -> Coordinate
kth k coords =
    case (k, coords) of
        (0, c::_) -> c
        (_, _::r) -> kth (k - 1) r
        (_, _)    -> Debug.todo "Improper call to kth"


{-- ========= End Coordinate Functions ================= --}



{-- ================ Random Functions ================== -}

seed0 : Random.Seed
seed0 =
  Random.initialSeed 41


-- TODO: Use Random.independentSeed in program
--       It is a generator for random seed

randomDirection : Random.Generator Direction
randomDirection =
    Random.uniform N [E, W, S]


-- Assumes that the list of coordinates given are the neighbors
-- of some node and that the list is nonempty
randomNeighbor : List Coordinate -> Random.Generator Coordinate
randomNeighbor neighbors =
    Random.int 0 ((+) -1 <| List.length neighbors)
    |> Random.map (\k -> kth k neighbors)



stepNeighbor : Random.Seed -> List Coordinate -> (Coordinate, Random.Seed)
stepNeighbor seed neighbors =
    Random.step (randomNeighbor neighbors) seed


randomElement : Random.Seed -> List Coordinate -> (Coordinate, Random.Seed) 
randomElement = stepNeighbor