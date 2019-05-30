module TremauxAI exposing (..)

 
import Maze exposing (..)
import Dict exposing (Dict)
import MazeTypes exposing (..)


startCoordinate : Coordinate
startCoordinate = (0, 0) 


-- We do not append to the history since the only thing needed
-- is the previous coordinate
tremaux : Model -> Model 
tremaux model = 
    let 
        state = model.aiState
        maze = model.aiMaze 
        coord = model.ai
        dir = state.dir 
        forward = state.forward
        history = state.hist
        junctions = state.junctions
    in
        if isIntersection coord maze then 
            let 
                newJunctions__ = addJunctionPaths coord junctions maze
                (newJunctions_, prev) = 
                    case history of 
                        []   -> (newJunctions__, coord)
                        p::_ -> (markPath (coord, p) newJunctions__, p)
                (newCoord, newJunctions) = choosePath coord prev newJunctions_ maze
                (newDir, _) = relativePosition coord newCoord 
            in
                {model | ai = newCoord, aiState = {state | dir = newDir, junctions = newJunctions, hist = [coord]}}

        else 
            if forward then 
                if validMove coord dir maze then 
                    {model | ai = movePlayer coord dir maze, aiState = {state | hist = [coord]}}
                else 
                    {model | aiState = {state | dir = oppositeDir dir}} -- hit a wall 
            else -- At (0, 0) with only one direction to go 
                case getValidDirs coord maze of 
                    []        -> Debug.todo "Unreachable"
                    newDir::_ -> {model | aiState = {state | forward = True, dir = newDir}}


addJunctionPaths : Coordinate -> JunctionMap -> Maze -> JunctionMap
addJunctionPaths coord map maze = 
    let 
        neighbors = getValidMoves coord maze 
        insert paths juncs = 
            case paths of 
                []   -> juncs 
                f::r -> (if Dict.member (coord, f) juncs then juncs    
                         else  Dict.insert (coord, f) 0 juncs)
                        |> insert r 
    in
        insert neighbors map


markPath : DEdge -> JunctionMap -> JunctionMap
markPath path map = 
    Dict.update path 
        (\marks -> 
            case marks of 
                Nothing     -> Nothing
                Just visits -> Just <| visits + 1)
        map
        

-- coord is a junction/intersection
choosePath : Coordinate -> Coordinate -> JunctionMap -> Maze -> (Coordinate, JunctionMap)
choosePath coord prev map maze =
    let 
        paths = getValidMoves coord maze |> List.map (\to -> (coord, to))
        pathMarks = List.map (\c -> (c, Dict.get c map |> Maybe.withDefault 2)) paths

        newIntersect : DEdge -> List (DEdge, Int) -> Bool
        newIntersect prev_ pMs = 
            case pMs of 
                []        -> True 
                (c, m)::r -> 
                    if c /= prev_ && m > 0 then
                        False 
                    else if c == prev_ && m > 1 then 
                        False 
                    else 
                        newIntersect prev_ r

        getToCoord : (DEdge, Int) -> Coordinate
        getToCoord path = 
            Tuple.second (Tuple.first path)

        chooseUnmarked : List (DEdge, Int) -> (Coordinate, JunctionMap)
        chooseUnmarked pMs = 
            case pMs of 
                []        -> Debug.todo "Called chooseUnmarked with no unmarked"
                (c, m)::r -> if (m > 0) then chooseUnmarked r
                             else (Tuple.second c, Dict.insert c 1 map)
                    
        choose_ : Maybe (DEdge, Int) -> List (DEdge, Int) -> (Coordinate, JunctionMap) 
        choose_ best pMs = 
            case (best, pMs) of 
                (Nothing, [])              -> Debug.todo "No paths to follow"
                (Just (c, m),  [])         -> (Tuple.second c, Dict.insert c (m + 1) map)
                (Nothing, (c, m)::r)       -> if m < 2 then choose_ (Just (c, m)) r 
                                              else choose_ best r
                (Just (c, m), (c2, m2)::r) -> if (m2 < m) then choose_ (Just (c2, m2)) r 
                                              else choose_ best r
    in
        if newIntersect (coord, prev) pathMarks then
            chooseUnmarked pathMarks
        else if Dict.get (coord, prev) map |> Maybe.andThen (\m -> Just <| m == 1) |> Maybe.withDefault False then
            (prev, Dict.insert (coord, prev) 2 map) 
        else 
            choose_ Nothing pathMarks 


