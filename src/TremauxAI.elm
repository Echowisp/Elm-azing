module TremauxAI exposing (..)

 
import Maze exposing (..)
import Dict exposing (Dict)
import MazeTypes exposing (..)


-- TODO: AI needs a history 
         -- It needs a Dictionary where junctions 
         -- are keys and values are directions taken
         -- from them (junctions)

startCoordinate : Coordinate
startCoordinate = (0, 0) 


{- 

  TODO: The bug is actually following the instructions here 

    The current bug is that a path might be an intersection point for
    multiple coordinates. Need to make unique lol 

        Fix make the map keys unique paths (Coordinate, Coordinate), (from, to)

-}

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
                newJunctions__ = Debug.log "ints" (addJunctionPaths coord junctions maze)
                (newJunctions_, prev) = 
                    case history of 
                        []   -> (newJunctions__, coord)
                        p::_ -> (markPath (coord, p) newJunctions__, p)
                (newCoord, newJunctions) = choosePath coord prev newJunctions_ maze
                (newDir, _) = relativePosition coord newCoord 
            in
                {model | ai = newCoord, aiState = {state | dir = newDir, junctions = newJunctions, hist = coord::history}}

        else 
            if forward then 
                if validMove coord dir maze then 
                    {model | ai = movePlayer coord dir maze, aiState = {state | hist = coord::history}} -- Will get stuck
                else 
                    {model | aiState = {state | dir = oppositeDir dir}}
            else -- At (0, 0) with only one direction to go 
                --let 
                    case getValidDirs coord maze of 
                        []       -> Debug.todo "Unreachable"
                        newDir::_ -> {model | aiState = {state | forward = True, dir = newDir}}
       {- if forward then 
            if not (isIntersection coord maze) then -- can I continue moving forward?
                if validMove coord dir maze then 
                    let newCoord = move coord dir in 
                    {model | ai = newCoord, 
                             aiState = {state | hist = coord::history}} -- Move in direction you've been going
                else -- I have reached a dead end 
                    {model | aiState = {state | forward = False}}
            else -- I have reached an intersection 
                case history of 
                    []    -> Debug.todo "Not Reached"
                    hd::hist_ ->
                        let 
                            (from, _) = Debug.log "here" (relativePosition coord hd)
                            juncs = updateJunction coord from junctions
                            _ = Debug.log "hmmm" juncs --(intersectionMove juncs coord maze)
                            _ = Debug.log "moves" (intersectionMove juncs coord maze)
                        in 
                        case intersectionMove juncs coord maze of 
                            Nothing     -> {model | ai = hd, aiState = {state | forward = False, hist = hist_} }
                            Just newDir -> 
                                {model | ai = move coord newDir, aiState = {state | dir = newDir, hist = coord::history, junctions = juncs}}
        else
            case history of 
                []          -> -- Should be at (0, 0) and haven't started moving
                    case getValidDirs coord maze of 
                        []    -> Debug.todo "No way to start"
                        newDir::_ -> {model | aiState = {state | dir = newDir, forward = True}}
                prev::hist_ ->
                    if Dict.member coord junctions then -- Retraced to an intersection
                        {model | aiState = {state | forward = True, hist = hist_}}
                    else 
                        {model | ai = prev, aiState = {state | hist = hist_}}
        -}
        

--intersectionMove : JunctionMap -> Coordinate -> Maze -> Maybe Direction
--intersectionMove map coord maze = 
--    case Dict.get coord map of 
--        Nothing   -> Debug.todo "Unmarked intersection"
--        Just dirs -> 
--            getValidDirs coord maze 
--            |> List.filter (\c -> not (List.member c dirs))
--            |> List.head

addJunctionPaths : Coordinate -> JunctionMap -> Maze -> JunctionMap
addJunctionPaths coord map maze = 
    let 
        neighbors = getValidMoves coord maze 
        _ = Debug.log "junctions added" neighbors
        insert paths juncs = 
            case paths of 
                []   -> juncs 
                f::r -> (if Dict.member (coord, f) juncs then juncs    
                         else  Dict.insert (coord, f) 0 juncs)
                        |> insert r 
    in
        insert neighbors map


markPath : (Coordinate, Coordinate) -> JunctionMap -> JunctionMap
markPath path map = 
    Dict.update path 
        (\marks -> 
            case marks of 
                Nothing     -> Nothing
                Just visits -> Just <| visits + 1)
        map
        


--intersectionMove : JunctionMap -> Coordinate -> Maze -> Coordinate
--intersectionMove map coord maze = 
--    let paths = getValidMoves coord maze in
--    choosePath paths map 

-- coord is a junction/intersection
choosePath : Coordinate -> Coordinate -> JunctionMap -> Maze -> (Coordinate, JunctionMap)
choosePath coord prev map maze =
    let 
        paths = getValidMoves coord maze |> List.map (\to -> (coord, to))
        _ = Debug.log "coord" coord
        _ = Debug.log "prev" prev
        _ = Debug.log "paths" paths
        pathMarks = List.map (\c -> (c, Dict.get c map |> Maybe.withDefault 2)) paths
        _ = Debug.log "pathMarks" pathMarks

        newIntersect : (Coordinate, Coordinate) -> List ((Coordinate, Coordinate), Int) -> Bool
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

        getToCoord : ((Coordinate, Coordinate), Int) -> Coordinate
        getToCoord path = 
            Tuple.second (Tuple.first path)

        chooseUnmarked : List ((Coordinate, Coordinate), Int) -> (Coordinate, JunctionMap)
        chooseUnmarked pMs = 
            case pMs of 
                []        -> Debug.todo "Called chooseUnmarked with no unmarked"
                (c, m)::r -> if (m > 0) then chooseUnmarked r
                             else (Tuple.second c, Dict.insert c 1 map)
                    
        choose_ : Maybe ((Coordinate, Coordinate), Int) -> List ((Coordinate, Coordinate), Int) -> (Coordinate, JunctionMap) 
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



-- Should be isIntersection
isJunction : Coordinate -> Maze -> Bool 
isJunction coord maze = 
    getValidMoves coord maze 
    |> List.length 
    |> (<) 2


-- Update the junction map to add the direction taken 
-- for a junction.
--updateJunction : Coordinate -> Direction -> JunctionMap -> JunctionMap
--updateJunction junction dir map = 
--    Dict.update junction
--        (\dirList -> 
--            case dirList of 
--                Nothing   -> Just [dir]
--                Just dirs -> Just <| dir::dirs)
--        map 


