{-- This module contains functions that render a maze --}

module Render exposing (..)

import Maze exposing (..)
import Grid exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Elmazing exposing (Model)
import Html exposing (Html)

cellSize = 16 -- Each cell is a cellSize by cellsize square
wallThickness = 2 -- Each wall is a 2px thick line

render : Model -> Html msg
render model = Debug.todo "???"

renderMaze : Maze -> Collage msg
renderMaze m =
    Grid.indexedMap renderNode m
    |> Grid.foldl (\a b -> a :: b) []

renderCell : Collage msg
renderCell =
    Collage.square cellSize
    |> filled (uniform lightPurple)

renderWalls : Node -> Collage msg
renderWalls node =
    let
        nPath = if node.n then [] else [(-cellSize/2, cellSize/2), (cellSize/2, cellSize/2)]
        ePath = if node.e then [] else [(cellSize/2, cellSize/2), (cellSize/2, -cellSize/2)]
        sPath = if node.s then [] else [(cellSize/2, -cellSize/2), (-cellSize/2, -cellSize/2)]
        wPath = if node.w then [] else [(-cellSize/2, -cellSize/2), (-cellSize/2, cellSize/2)]
        walls = nPath ++ ePath ++ sPath ++ wPath
    in
        path walls |> traced (solid semithick (uniform darkBlue))

renderNode : Int -> Int -> Node -> Collage msg
renderNode r c nd =
    let
        walls = renderWalls nd
        cell = renderCell
    in
        [walls, cell]
        |> stack
        |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
        |> shift (calculateShift r c)


renderPlayer : Coordinate -> Collage msg
renderPlayer (r, c) =
    let
        (x, y) = convertRCToXY r c
    in
    Collage.circle cellSize/2
    |> filled (uniform green)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderAI : Coordinate -> Collage msg
renderAI c =
    let
        (x, y) = convertRCToXY r c
    in
    Collage.circle cellSize/2
    |> filled (uniform red)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderVictory : Collage msg
renderVictory =
    let
        msg = Text.fromString "You win!"
              |> Text.size huge
              |> rendered
        bg = rectangle 500 200 |> filled (uniform darkgreen)
    in
        stack [msg, bg]

renderDefeat : Collage msg
renderDefeat =
    let
        msg = Text.fromString "You win!"
              |> Text.size huge
              |> rendered
        bg = rectangle 500 200 |> filled (uniform red)
    in
        stack [msg, bg]

convertRCToXY : Int -> Int -> Coordinate
convertRCToXY r c = (c - mazeSize/2, mazeSize/2 - r)

calculateShift : Int -> Int -> Coordinate
calculateShift r c =
    let
        (x, y) = convertRCToXY r c
    in
        (x * cellSize, y * cellSize)
