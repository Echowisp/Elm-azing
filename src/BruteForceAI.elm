module BruteForceAI exposing (..)

import Maze exposing (..)
import MazeTypes exposing (..)

bruteForceAI : Model -> Model
bruteForceAI mdl =
    let
        state = mdl.aiState
        aiMz = mdl.aiMaze
        coord = mdl.ai
        currDir = state.dir
        ide = isDeadEnd aiMz coord currDir
        (ndir, nseed) = stepDirection state.seed
    in
   if (isJunction aiMz coord currDir) then
        if
            validMove coord ndir aiMz &&
            not ((not ide) && (currDir == oppositeDir ndir))
        then
            {
                mdl | ai = (movePlayer coord ndir aiMz),
                    aiState = { state | dir = ndir, seed = nseed }
            }
        else
            bruteForceAI { mdl | aiState = { dir = currDir, seed = nseed } }
    else
        { mdl | ai = (movePlayer coord currDir aiMz) }
