module Grid exposing (..)

{-
    This module is adapted from the Grid elm library that does 
    not currently support elm 0.19.0


-}

import Array exposing (Array)
import Maybe


type alias Grid a = Array (Array a)
type alias Coordinate = (Int, Int)

type Direction = N | E | W | S



toCoordinate : Int -> Int -> Coordinate
toCoordinate x y = 
    (x, y)


row : Int -> Grid a -> Maybe (Array a) 
row index grid = 
    Array.get index grid 


-- Filler is a function that given the coordinates
-- x and y for a grid, initializes x and y with 
-- the proper values
type alias Filler a = Int -> Int -> a 


rectangle : Int -> Int -> Filler a -> Grid a 
rectangle width height filler = 
    Array.initialize 
        height
        (\y -> Array.initialize width (\x -> filler x y)) 


square : Int -> Filler a -> Grid a 
square size filler = 
    rectangle size size filler 


repeat : Int -> Int -> a -> Grid a 
repeat x y occupant = 
    rectangle x y (always << always occupant)


repeatSquare : Int -> a -> Grid a 
repeatSquare size occupant = 
    square size (always << always occupant)


toColumn : Coordinate -> Int
toColumn coord = 
    Tuple.second coord


toRow : Coordinate -> Int 
toRow coord = 
    Tuple.first coord 


getRow : Coordinate -> Grid a -> Maybe (Array a)
getRow coord grid =
    grid 
    |>  row (toRow coord)


get : Coordinate -> Grid a -> Maybe a
get coord grid = 
    getRow coord grid   
    |> Maybe.andThen (Array.get (toColumn coord))


set : Coordinate -> a -> Grid a -> Grid a 
set coord newVal grid = 
    getRow coord grid 
    |> Maybe.map (Array.set (toColumn coord) newVal) 
    |> Maybe.map (\r -> Array.set (toRow coord) r grid)
    |> Maybe.withDefault grid 
    |> Debug.log "new"


map : (a -> b) -> Grid a -> Grid b 
map f grid =
    Array.map (Array.map f) grid 


verifyAdjacent : Coordinate -> Coordinate -> Bool
verifyAdjacent (r1, c1) (r2, c2) =
    abs (r1 - r2) <= 1 
    && abs (c1 - c2) <= 1
    && (r1 /= r2 || c1 /= c2)



-- Called with the assumption that the two coordinates 
-- are adjacent
--
-- For coordinates a and b returns a tuple of the form of the directions
-- (a -> b, b -> a) 
relativePosition : Coordinate -> Coordinate -> (Direction, Direction)
relativePosition (r1, c1) (r2, c2) =
    if r1 - r2 /= 0 then 
        if r1 - r2 < 0 then 
            (S, N)
        else 
            (N, S)
    else 
        if c1 - c1 < 0 then 
            (E, W)
        else 
            (W, E)













