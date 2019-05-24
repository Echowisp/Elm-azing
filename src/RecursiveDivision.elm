module RecursiveDivision exposing (..)

import Stack exposing (Stack)
import Random
import Set exposing (Set)
import MazeTypes exposing (..)
import Maze exposing (..)
import Grid


buildMaze : Int -> Int -> Maze 
buildMaze len wid = 
    let 
        intitialMaze = openMaze len wid 
        visited = Set.empty |> Set.insert (0, 0) 
        frontier = [(0,0)]
    in
        recursiveDivision Maze.seed0 intitialMaze



recursiveDivision : Random.Seed -> Maze -> Maze 
recursiveDivision seed maze = 
    let (w, h) = Grid.dims maze in 
    divide seed (0, 0) w h maze


randInt : Random.Seed -> Int -> Int -> (Int, Random.Seed)
randInt seed l u =
    Random.step (Random.int l u) seed 


addHorizontalWall : Maze -> Int -> Int -> Int -> Maze 
addHorizontalWall maze row start end = 
    let 
        columns = List.range start end 
        coords1 = columns |> List.map (\x -> (row, x))
        coords2 = columns |> List.map (\x -> (row + 1, x))
    in 
        Maze.selectMap (\node -> {node | s = False}) coords1 maze
        |> Maze.selectMap (\node -> {node | n = False}) coords2


addVerticalWall : Maze -> Int -> Int -> Int -> Maze 
addVerticalWall maze col start end = 
    let 
        rows = List.range start end 
        coords1 = rows |> List.map (\x -> (x, col))
        coords2 = rows |> List.map (\x -> (x, col + 1))
    in
        Maze.selectMap (\node -> {node | e = False}) coords1 maze
        |> Maze.selectMap (\node -> {node | w = False}) coords2 


openHorizontalDoor : Coordinate -> Maze -> Maze 
openHorizontalDoor (r, c) maze = 
    Maze.selectMap (\node -> {node | s = True}) [(r, c)] maze 
    |> Maze.selectMap (\node -> {node | n = True}) [(r + 1, c)] 


openVerticalDoor : Coordinate -> Maze -> Maze 
openVerticalDoor (r, c) maze = 
    Maze.selectMap (\node -> {node | e = True}) [(r, c)] maze 
    |> Maze.selectMap (\node -> {node | w = True}) [(r, c + 1)]  


divide : Random.Seed -> Coordinate -> Int -> Int -> Maze -> Maze 
divide seed (r, c) w h maze = 
    if w <= 1 || h <= 1 then
        maze
    else if w < h then 
        let 
            (wallRow, seed1) = randInt seed r (r + h)
            (doorCol, seed2) = randInt seed1 c (c + w)
            newMaze = addHorizontalWall maze wallRow c (c + w)
                        |> openHorizontalDoor (wallRow, doorCol)
        in 
            divide seed2 (r, c) w (wallRow - r) newMaze
            |> divide (Random.initialSeed wallRow) (wallRow, c) w (r + h - wallRow)
    else 
        let 
            (wallCol, seed1) = randInt seed c (c + w)
            (doorRow, seed2) = randInt seed1 r (r + h)
            newMaze = addVerticalWall maze wallCol r (r + h)
                        |> openVerticalDoor (wallCol, doorRow)
        in
            divide seed2 (r, c) (wallCol - c) h newMaze
            |> divide (Random.initialSeed wallCol) (r, wallCol) (c + w - wallCol) h



