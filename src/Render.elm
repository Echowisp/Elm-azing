{-- This module contains functions that render a maze --}

module Render exposing (..)

import Maze exposing (..)
import Grid exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color

render : Maze -> Coordinate -> Coordinate -> Html msg
render maze player ai =

renderMaze : Node -> Coordinate -> Collage msg

renderCell : Node -> Collage msg

renderWalls : Node -> Collage msg

renderNode : Int -> Int -> Node -> Collage msg
renderNode : r c nd =
    
renderPlayer : Coordinate -> Collage msg

renderAI : Coordinate -> Collage msg

renderVictory : Collage msg

renderDefeat : Coordinate msg
