{-- This module contains functions that render a maze --}

module Render exposing (..)

import Maze exposing (..)
import Grid exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Text as Text exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import MazeTypes exposing (..)
import Html exposing (Html)

mazeSize = 30 -- Maze dimensions will be mazeSize x mazeSize
cellSize = 32 -- Each cell is a cellSize by cellsize square
wallThickness = 2 -- Each wall is a 2px thick line

render : Model -> Html msg
render model =
    let
        winLoss =
            if model.gameState == AIVictory then
                [renderVictory]
            else if model.gameState == PlayerVictory then
                [renderDefeat]
            else
                []
        player = renderPlayer model.player
        playerTitle = renderText "Player"
        ai = renderAI model.ai
        aiTitle = renderText "AI"
        playerMaze = vertical [playerTitle,
                               stack [player, renderMaze model.playerMaze]]
        aiMaze = vertical [aiTitle, stack [ai, renderMaze model.aiMaze]]
        spacing = spacer 50 0
        mazes = horizontal [playerMaze, spacing, aiMaze] |> svg
    in
    mazes

{-- Title Rendering Logic --}
renderText : String -> Collage msg
renderText s =
    Text.fromString s
    |> Text.size large
    |> rendered
    |> align top

{-- Maze Rendering Logic --}

renderMaze : Maze -> Collage msg
renderMaze m =
    Grid.indexedMap renderNode m
    |> Grid.foldl (\a b -> a :: b) []
    |> stack

renderCell : Collage msg
renderCell =
        Collage.square cellSize |> filled (uniform lightPurple)


renderWalls : Node -> Collage msg
renderWalls node =
    let
        nPath = if node.n then [] else [(-cellSize/2, cellSize/2), (cellSize/2, cellSize/2)]
        ePath = if node.e then [] else [(cellSize/2, cellSize/2), (cellSize/2, -cellSize/2)]
        sPath = if node.s then [] else [(cellSize/2, -cellSize/2), (-cellSize/2, -cellSize/2)]
        wPath = if node.w then [] else [(-cellSize/2, -cellSize/2), (-cellSize/2, cellSize/2)]
        walls = nPath ++ ePath ++ sPath ++ wPath
    in
        path walls |> traced (solid thin (uniform blue))

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
    (Collage.circle (cellSize/2))
    |> filled (uniform green)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderAI : Coordinate -> Collage msg
renderAI (r, c) =
    let
        (x, y) = convertRCToXY r c
    in
    (Collage.circle <| cellSize/2)
    |> filled (uniform red)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderVictory : Collage msg
renderVictory =
    let
        msg = Text.fromString "You win!"
              |> Text.size huge
              |> rendered
        bg = Collage.rectangle 500 200 |> filled (uniform darkGreen)
    in
        stack [msg, bg]

renderDefeat : Collage msg
renderDefeat =
    let
        msg = Text.fromString "You win!"
              |> Text.size huge
              |> rendered
        bg = Collage.rectangle 500 200 |> filled (uniform red)
    in
        stack [msg, bg]

convertRCToXY : Int -> Int -> (Float, Float)
convertRCToXY r c = ((toFloat c) - mazeSize / 2.0, mazeSize / 2.0 - (toFloat r))

calculateShift : Int -> Int -> (Float, Float)
calculateShift r c =
    let
        (x, y) = convertRCToXY r c
    in
        (x * cellSize, y * cellSize)
