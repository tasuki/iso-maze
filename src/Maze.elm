module Maze exposing (..)


type alias Maze =
    List Block


type Block
    = Base Position
    | Stairs Position Direction


type alias Position =
    ( Int, Int, Int )


type alias Vector =
    Position


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


isValidPosition : Position -> Bool
isValidPosition ( x, y, z ) =
    if x < -10 || y < -10 || x > 15 || y > 15 || z < 0 || z > 10 then
        -- out of reasonable bounds
        False

    else if x + y < -3 then
        -- too near player
        False

    else if x - y > 18 || y - x > 18 then
        -- too far to the side
        False

    else
        True


shiftPosition : Position -> Vector -> Position
shiftPosition ( x, y, z ) ( xd, yd, zd ) =
    ( x + xd, y + yd, z + zd )
