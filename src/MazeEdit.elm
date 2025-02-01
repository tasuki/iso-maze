module MazeEdit exposing (..)

import Maybe.Extra
import Maze as M

type Mode
    = Running
    | Editing

toggleBlock : M.Position -> M.Maze -> M.Maze
toggleBlock ( x, y, z ) maze =
    let
        newZ = M.get ( x, y ) maze
            |> Maybe.map M.blockPosition
            |> Maybe.Extra.filter (\( _, _, bz ) -> bz >= z)
            |> Maybe.Extra.unwrap z (\_ -> z - 1)
    in
    if newZ < 0 then M.clear (x, y) maze
    else M.set (M.Base ( x, y, newZ )) maze

toggleStairs : M.Position -> M.Maze -> M.Maze
toggleStairs ( x, y, z ) maze =
    case M.get ( x, y ) maze of
        Just (M.Stairs ( _, _, oz ) dir) ->
            if oz == z then
                M.set (M.Stairs ( x, y, z ) (M.nextDirection dir)) maze
            else
                M.set (M.Stairs ( x, y, z ) dir) maze
        _ ->
            M.set (M.Stairs ( x, y, z ) M.SE) maze

toggleBridge : M.Position -> M.Maze -> M.Maze
toggleBridge ( x, y, z ) maze =
    M.set (M.Bridge ( x, y, z )) maze

placeStart : M.Position -> M.Maze -> M.Maze
placeStart ( x, y, z ) maze =
    { maze | start = ( x, y ) }

placeEnd : M.Position -> M.Maze -> M.Maze
placeEnd ( x, y, z ) maze =
    { maze | end = ( x, y ) }
