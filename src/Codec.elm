module Codec exposing (..)

import Array
import Maze as M


encode : M.Maze -> String
encode _ = ""

decode : String -> M.Maze
decode _ = Array.empty


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
