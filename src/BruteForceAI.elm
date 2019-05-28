module BruteForceAI exposing (..)

import Maze exposing (..)
import MazeTypes exposing (..)

bruteForceAI : Model -> Model
bruteForceAI mdl =
    let
        state = mdl.aiState
        (ndir, nseed) = stepDirection state.seed
    in
    if validMove mdl.ai ndir mdl.aiMaze then
        { mdl | ai = (movePlayer mdl.ai ndir mdl.aiMaze) }
    else
        bruteForceAI { mdl | aiState = { dir = ndir, seed = nseed } }