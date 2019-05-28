module TremauxAI exposing (..)

 
import Maze exposing (..)
import Dict exposing (Dict)
import MazeTypes exposing (..)


-- TODO: AI needs a history 
         -- It needs a Dictionary where junctions 
         -- are keys and values are directions taken
         -- from them (junctions)



--tremaux : Model -> Model 
--tremaux model = 



isJunction : Coordinate -> Maze -> Bool 
isJunction coord maze = 
    getValidMoves coord maze 
    |> List.length 
    |> (<) 2


-- Update the junction map to add the direction taken 
-- for a junction.
updateJunction : Coordinate -> Direction -> JunctionMap -> JunctionMap
updateJunction junction dir map = 
    Dict.update junction
        (\dirList -> 
            case dirList of 
                Nothing   -> Just [dir]
                Just dirs -> Just <| dir::dirs)
        map 
