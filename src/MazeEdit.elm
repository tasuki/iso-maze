module MazeEdit exposing (..)

import Maybe.Extra
import Maze as M

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
        Just (M.Stairs _ dir) ->
            M.set (M.Stairs ( x, y, z ) (M.nextDirection dir)) maze
        _ ->
            M.set (M.Stairs ( x, y, z ) M.SE) maze

toggleBridge : M.Position -> M.Maze -> M.Maze
toggleBridge ( x, y, z ) maze =
    M.set (M.Bridge ( x, y, z )) maze
