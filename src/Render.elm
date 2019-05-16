{-- This module contains functions that render a maze --}

module Render exposing (..)

import Maze exposing (..)
import Grid exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Html exposing (Html)

cellSize = 15 -- Each cell is a cellSize by cellsize square
wallThickness = 2 -- Each wall is a 2px thick line

render : Maze -> Coordinate -> Coordinate -> Html msg
render maze player ai =
    Debug.todo "??"

renderMaze : Node -> Coordinate -> Collage msg
renderMaze = Debug.todo ""

renderCell : Collage msg
renderCell = Collage.square cellSize |> filled (uniform lightPurple)

renderWalls : Node -> Collage msg
renderWalls node =
    let
        nPath = if node.n then [] else [(0, 0), (cellSize, 0)]
        ePath = if node.e then [] else [(cellSize, 0), (cellSize, -(cellSize))]
        sPath = if node.s then [] else [(cellSize, -(cellSize)), (0, -(cellSize))]
        wPath = if node.w then [] else [(0, -(cellSize)), (0, 0)]
        walls = nPath ++ ePath ++ sPath ++ wPath
    in
        path walls |> traced (solid thin (uniform darkPurple))

renderNode : Int -> Int -> Node -> Collage msg
renderNode r c nd =
    Debug.todo ""

renderPlayer : Coordinate -> Collage msg
renderPlayer c = Debug.todo ""

renderAI : Coordinate -> Collage msg
renderAI c = Debug.todo ""

renderVictory : Collage msg
renderVictory = Debug.todo ""

renderDefeat : Collage msg
renderDefeat = Debug.todo ""

