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
            let
                bestDir = M.allDirections
                    |> List.map (\d -> ( d, countConnections ( x, y, z ) d maze ))
                    |> List.sortBy (Tuple.second >> negate)
                    |> List.head |> Maybe.map Tuple.first |> Maybe.withDefault M.SE
            in
            M.set (M.Stairs ( x, y, z ) bestDir) maze

countConnections : M.Position -> M.Direction -> M.Maze -> Int
countConnections ( x, y, z ) dir maze =
    let
        bottomNeighbor = M.get (M.shiftPos2d ( x, y ) dir) maze
        topNeighbor = M.get (M.shiftPos2d ( x, y ) (M.oppositeDirection dir)) maze
        bottomConnect = bottomNeighbor |> Maybe.andThen (M.exitHeight (M.oppositeDirection dir) (z - 1)) |> Maybe.map (always 1) |> Maybe.withDefault 0
        topConnect = topNeighbor |> Maybe.andThen (M.exitHeight dir z) |> Maybe.map (always 1) |> Maybe.withDefault 0
    in
    bottomConnect + topConnect

toggleBridge : M.Position -> M.Maze -> M.Maze
toggleBridge ( x, y, z ) maze =
    if z >= 1 then M.set (M.Bridge ( x, y, z )) maze
    else maze

toggleGreenery : M.Position -> M.Maze -> M.Maze
toggleGreenery ( x, y, z ) maze =
    if z >= 1 then M.set (M.Greenery ( x, y, z - 1 )) maze
    else maze

clearTile : M.Position -> M.Maze -> M.Maze
clearTile ( x, y, _ ) maze =
    M.clear ( x, y ) maze

placeStart : M.Position -> M.Maze -> M.Maze
placeStart ( x, y, z ) maze =
    { maze | start = ( x, y ) }

placeEnd : M.Position -> M.Maze -> M.Maze
placeEnd ( x, y, z ) maze =
    { maze | end = ( x, y ) }
