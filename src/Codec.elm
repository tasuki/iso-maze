module Codec exposing (..)

import Array
import Maze as M

encode : M.Maze -> String
encode maze =
    let cut = cutout maze in
    "sz:"
        ++ (String.fromInt cut.xSize) ++ ","
        ++ (String.fromInt cut.ySize)
        ++ ";off:"
        ++ (String.fromInt cut.xOffset) ++ ","
        ++ (String.fromInt cut.yOffset)
        ++ ";mz:" ++ (String.join "" <| List.map encodeBlock cut.maze)

decode : String -> M.Maze
decode _ = Array.empty


-- Block encoder/decoder

heightChars : List Char
heightChars = String.toList "0123456789abcdefghijklmnopqrstuvwxyz"

charFromIndex : Int -> String
charFromIndex drop =
    List.drop drop heightChars
        |> List.head
        |> Maybe.withDefault '*'
        |> String.fromChar

encodeBlock : M.MazeBlock -> String
encodeBlock block =
    case block of
        M.EmptyBlock -> "x"
        M.BaseBlock z -> "o" ++ charFromIndex z
        M.StairsBlock z M.SE -> "s" ++ charFromIndex z
        M.StairsBlock z M.SW -> "z" ++ charFromIndex z
        M.StairsBlock z M.NE -> "Z" ++ charFromIndex z
        M.StairsBlock z M.NW -> "S" ++ charFromIndex z


-- Cutout maze to minimum rectangle

type alias SubMaze =
    { xSize : Int
    , ySize : Int
    , xOffset : Int
    , yOffset : Int
    , maze : List M.MazeBlock
    }

toSubIndex : SubMaze -> Int -> Int -> Int
toSubIndex subMaze x y = y * subMaze.xSize + x

cutout : M.Maze -> SubMaze
cutout maze =
    let
        limits = mazeLimits maze
        xRange = List.range limits.minX limits.maxX
        yRange = List.range limits.minY limits.maxY
        getBlock y x = Array.get (M.toIndex x y) maze
    in
    { xSize = limits.maxX - limits.minX + 1
    , ySize = limits.maxY - limits.minY + 1
    , xOffset = limits.minX
    , yOffset = limits.minY
    , maze = M.mapCoords yRange xRange getBlock |> List.filterMap identity
    }


-- Limits

type alias Limits =
    { minX: Int
    , maxX: Int
    , minY: Int
    , maxY: Int
    }

mazeLimits : M.Maze -> Limits
mazeLimits maze =
    let
        extremes : M.Position -> Limits -> Limits
        extremes (x, y, _) acc =
            { minX = min acc.minX x
            , maxX = max acc.maxX x
            , minY = min acc.minY y
            , maxY = max acc.maxY y
            }
    in
    M.mapAllCoords (\x y -> (M.get (x, y) maze))
        |> List.filterMap identity
        |> List.map M.blockPosition
        |> List.foldl extremes
            { minX = 1000
            , maxX = -100
            , minY = 1000
            , maxY = -100
            }
