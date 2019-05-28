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

cellSize = 32 -- Each cell is a cellSize by cellsize square
wallThickness = 2 -- Each wall is a 2px thick line

render : Model -> Html msg
render model =
    let
        winLoss =
            if model.gameState == PlayerVictory then
                [renderVictory]
            else if model.gameState == AIVictory then
                [renderDefeat]
            else
                []
        player = renderPlayer model.player
        playerTitle = renderText "Player"
        ai = renderAI model.ai
        aiTitle = renderText "AI"
        playerMaze = vertical [playerTitle,
                               stack [player,
                                      renderFin,
                                      renderMaze model.playerMaze,
                                      renderMazeBg]]
        aiMaze = vertical [aiTitle,
                           stack [ai,
                                  renderFin,
                                  renderMaze model.aiMaze,
                                  renderMazeBg]]
        spacing = spacer 50 0
        mazes = svg<| stack <| winLoss ++ [(horizontal ([playerMaze, spacing, aiMaze]))]
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

renderMazeBg : Collage msg
renderMazeBg =
        Collage.square (cellSize * mazeSize) |> filled (uniform lightPurple)

renderWalls : Node -> Collage msg
renderWalls node =
    let
        nPath = if node.n then
                    []
                else
                    [segment (-cellSize/2, cellSize/2) (cellSize/2, cellSize/2)
                    |> traced (solid thin (uniform blue))]
        ePath = if node.e then
                    nPath
                else
                    (segment (cellSize/2, cellSize/2) (cellSize/2, -cellSize/2)
                    |> traced (solid thin (uniform blue))) :: nPath
        sPath = if node.s then
                    ePath
                else
                    (segment (cellSize/2, -cellSize/2) (-cellSize/2, -cellSize/2)
                    |> traced (solid thin (uniform blue))) :: ePath
        wPath = if node.w then
                    sPath
                else
                    (segment (-cellSize/2, -cellSize/2) (-cellSize/2, cellSize/2)
                    |> traced (solid thin (uniform blue))) :: sPath
    in
        stack wPath

renderFin : Collage msg
renderFin =
    let
        xcolor = red
        l = segment (0, 0) (cellSize, -cellSize)
            |> traced (solid thick (uniform xcolor))
            |> scale 0.75
        r = segment (0, -cellSize) (cellSize, 0)
            |> traced (solid thick (uniform xcolor))
            |> scale 0.75
    in
        stack [l, r]
        |> shift (cellSize/7, -cellSize/7) --Top left of Node is now on the origin
        |> shift (calculateShift 29 29)

renderNode : Int -> Int -> Node -> Collage msg
renderNode r c nd =
    let
        walls = renderWalls nd
    in
        walls
        |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
        |> shift (calculateShift r c)


renderPlayer : Coordinate -> Collage msg
renderPlayer (r, c) =
    let
        (x, y) = convertRCToXY r c
    in
    (Collage.circle (cellSize/3))
    |> filled (uniform green)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderAI : Coordinate -> Collage msg
renderAI (r, c) =
    let
        (x, y) = convertRCToXY r c
    in
    (Collage.circle <| cellSize/3)
    |> filled (uniform orange)
    |> shift (cellSize/2, -cellSize/2) --Top left of Node is now on the origin
    |> shift (calculateShift r c)

renderVictory : Collage msg
renderVictory =
    let
        msg = Text.fromString "You win!"
              |> Text.size huge
              |> Text.weight Bold
              |> rendered
        msg2 = Text.fromString "Select a difficulty and press go to play again"
               |> Text.size large
               |> rendered
        bg = Collage.rectangle 500 200 |> filled (uniform darkGreen)
    in
        stack [vertical [msg, (spacer 0 20), msg2], bg]
        |> shift (cellSize * mazeSize / 1.9, -cellSize * mazeSize / 2)

renderDefeat : Collage msg
renderDefeat =
    let
        msg = Text.fromString "You Lose :\'("
              |> Text.size huge
              |> Text.weight Bold
              |> rendered
        msg2 = Text.fromString "Select a difficulty and press go to play again"
               |> Text.size large
               |> rendered
        bg = Collage.rectangle 500 200 |> filled (uniform red)
    in
        stack [vertical [msg, (spacer 0 20), msg2], bg]
        |> shift (cellSize * mazeSize / 1.9, -cellSize * mazeSize / 2)

convertRCToXY : Int -> Int -> (Float, Float)
convertRCToXY r c = ((toFloat c) - mazeSize / 2.0, mazeSize / 2.0 - (toFloat r))

calculateShift : Int -> Int -> (Float, Float)
calculateShift r c =
    let
        (x, y) = convertRCToXY r c
    in
        (x * cellSize, y * cellSize)
