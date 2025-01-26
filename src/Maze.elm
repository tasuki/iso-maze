module Maze exposing (..)


type alias Maze =
    List Block


type Block
    = Base Position
    | Stairs Position Direction


type alias Position =
    ( Int, Int, Int )


type Direction
    = SE
    | SW
    | NE
    | NW


createBase : Int -> Int -> Int -> Block
createBase x y z =
    Base ( x, y, z )


createStairs : Int -> Int -> Int -> Direction -> Block
createStairs x y z dir =
    Stairs ( x, y, z ) dir
