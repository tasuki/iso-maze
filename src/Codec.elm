module Codec exposing (..)

import Array
import Maze as M


encode : M.Maze -> String
encode _ = ""

decode : String -> M.Maze
decode _ = Array.empty


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
