module RecursiveDivision exposing (..)

import Stack exposing (Stack)
import Random
import Set exposing (Set)
import MazeTypes exposing (..)
import Maze exposing (..)
import Grid


buildMaze : Int -> Int -> Random.Seed -> Maze
buildMaze len wid seed =
    let
        intitialMaze = openMaze len wid
        visited = Set.empty |> Set.insert (0, 0)
        frontier = [(0,0)]
    in
        recursiveDivision seed intitialMaze |> borderMaze



recursiveDivision : Random.Seed -> Maze -> Maze
recursiveDivision seed maze =
    let (h, w) = Grid.dims maze in
    divide seed (0, 0) w h maze


randInt : Random.Seed -> Int -> Int -> (Int, Random.Seed)
randInt seed l u =
    Random.step (Random.int l u) seed


addHorizontalWall : Int -> Int -> Int -> Maze -> Maze
addHorizontalWall row start end maze =
    let
        columns = List.range start end
        coords1 = columns |> List.map (\x -> (row, x))
        coords2 = columns |> List.map (\x -> (row + 1, x))
    in
        Maze.selectMap (\node -> {node | s = False}) coords1 maze
        |> Maze.selectMap (\node -> {node | n = False}) coords2


addVerticalWall : Int -> Int -> Int -> Maze -> Maze
addVerticalWall col start end maze =
    let
        rows = List.range start end
        coords1 = rows |> List.map (\x -> (x, col))
        coords2 = (rows |> List.map (\x -> (x, col + 1)))
    in
        Maze.selectMap (\node -> {node | e = False}) coords1 maze
        |> Maze.selectMap (\node -> {node | w = False}) coords2


borderMaze : Maze -> Maze
borderMaze maze =
    let
        (w, h) = Grid.dims maze
    in
        addVerticalWall -1 0 h maze
        |> addVerticalWall (w - 1) 0 h
        |> addHorizontalWall -1 0 w
        |> addHorizontalWall (h - 1) 0 w



openHorizontalDoor : Coordinate -> Maze -> Maze
openHorizontalDoor (r, c) maze =
    Maze.selectMap (\node -> {node | s = True}) [(r, c)] maze
    |> Maze.selectMap (\node -> {node | n = True}) [(r + 1, c)]


openVerticalDoor : Coordinate -> Maze -> Maze
openVerticalDoor (r, c) maze =
    Maze.selectMap (\node -> {node | e = True}) [(r, c)] maze
    |> Maze.selectMap (\node -> {node | w = True}) [(r, c + 1)]


divide : Random.Seed -> Coordinate -> Int -> Int -> Maze  -> Maze
divide seed (r, c) w h maze =
    if w <= 1 && h <= 1 then
        maze
    else if w < h then
        let
            (wallRow, seed1) = randInt seed (r + 1) (r + h - 1)
            (doorCol, seed2) = randInt seed1 c (c + w - 1)
            newMaze =
                addHorizontalWall (wallRow - 1) c (c + w) maze
                       |> openHorizontalDoor (wallRow - 1, doorCol)
        in
          divide (Random.initialSeed wallRow) (wallRow, c) w (h - (wallRow - r))
                 (divide seed2 (r, c) w (wallRow - r) newMaze)
    else
        let
            (wallCol, seed1) = randInt seed (c + 1) (c + w - 1)
            (doorRow, seed2) = randInt seed1 r (r + h - 1)
            newMaze = addVerticalWall (wallCol - 1) r (r + h) maze
                        |> openVerticalDoor (doorRow, wallCol - 1)
        in
            divide (Random.initialSeed wallCol) (r, wallCol) (w - (wallCol - c)) h
                   (divide seed2 (r, c) (wallCol - c) h newMaze)

