module MazeEdit exposing (..)

import Maze as M


toggleBlock : M.Position -> M.Maze -> M.Maze
toggleBlock ( x, y, z ) maze =
    case M.get ( x, y ) maze of
        Nothing ->
            M.set (M.Base ( x, y, z )) maze

        Just block ->
            let
                ( _, _, bz ) =
                    M.blockPosition block
            in
            if bz < z then
                M.set (M.Base ( x, y, z )) maze

            else
                M.set (M.Base ( x, y, z - 1 )) maze


toggleStairs : M.Position -> M.Maze -> M.Maze
toggleStairs ( x, y, z ) maze =
    case M.get ( x, y ) maze of
        Just (M.Stairs _ dir) ->
            M.set (M.Stairs ( x, y, z ) (M.nextDirection dir)) maze

        _ ->
            M.set (M.Stairs ( x, y, z ) M.SE) maze
