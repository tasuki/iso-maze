module Maze exposing (..)

import Array exposing (Array)

minTileCoord = -10
maxTileCoord = 15

minHeight = 0
maxHeight = 10

nearest = -3
sidest = 18

sideSize = maxTileCoord - minTileCoord + 1
coordsRange = List.range minTileCoord maxTileCoord

type alias Maze = Array MazeBlock
type MazeBlock
    = EmptyBlock
    | BaseBlock Int
    | StairsBlock Int Direction

type Block
    = Base Position
    | Stairs Position Direction

type alias Pos2d = ( Int, Int )
type alias Position = ( Int, Int, Int )
type alias Vector = Position

type Direction = SE | SW | NE | NW


-- Maze

emptyMaze : Maze
emptyMaze = emptyMazeSize sideSize sideSize

emptyMazeSize : Int -> Int -> Maze
emptyMazeSize xSize ySize = Array.initialize (ySize * xSize) (always EmptyBlock)

toIndex : Int -> Int -> Int
toIndex x y = (y - minTileCoord) * sideSize + (x - minTileCoord)

fromBlocks : List Block -> Maze
fromBlocks blocks = List.foldl set emptyMaze blocks

set : Block -> Maze -> Maze
set block maze =
    let
        ( xx, yy, mazeBlock ) = case block of
            Base ( x, y, z ) -> ( x, y, BaseBlock z )
            Stairs ( x, y, z ) dir -> ( x, y, StairsBlock z dir )
    in Array.set (toIndex xx yy) mazeBlock maze

clear : Pos2d -> Maze -> Maze
clear (x, y) maze = Array.set (toIndex x y) EmptyBlock maze

mapCoords : List Int -> List Int -> (Int -> Int -> a) -> List a
mapCoords rangeY rangeX fun =
    List.concatMap (\y -> List.map (fun y) rangeX) rangeY

mapAllCoords : (Int -> Int -> a) -> List a
mapAllCoords = mapCoords coordsRange coordsRange

toBlocks : Maze -> List Block
toBlocks maze = List.filterMap (\c -> get c maze) (mapAllCoords Tuple.pair)

toBlock : Pos2d -> MazeBlock -> Maybe Block
toBlock ( x, y ) mazeBlock = case mazeBlock of
    EmptyBlock -> Nothing
    BaseBlock z -> Just <| Base (x, y, z)
    StairsBlock z dir -> Just <| Stairs (x, y, z) dir

get : Pos2d -> Maze -> Maybe Block
get ( x, y ) maze =
    Array.get (toIndex x y) maze |> Maybe.andThen (toBlock ( x, y ))


-- Block

createBase : Int -> Int -> Int -> Block
createBase x y z = Base ( x, y, z )

createStairs : Int -> Int -> Int -> Direction -> Block
createStairs x y z dir = Stairs ( x, y, z ) dir

blockPosition : Block -> Position
blockPosition block = case block of
    Base pos -> pos
    Stairs pos _ -> pos


-- Position

invalidCoord : Int -> Bool
invalidCoord c = c < minTileCoord || c > maxTileCoord

invalidHeight : Int -> Bool
invalidHeight z = z < minHeight || z > maxHeight

isValidPosition : Position -> Bool
isValidPosition ( x, y, z ) =
    if invalidCoord x || invalidCoord y || invalidHeight z then
        False -- out of reasonable bounds
    else if x + y < nearest then
        False -- too near player
    else if x - y > sidest || y - x > sidest then
        False -- too far to the side
    else
        True

shiftPosition : Position -> Vector -> Position
shiftPosition ( x, y, z ) ( xd, yd, zd ) = ( x + xd, y + yd, z + zd )


-- Direction

nextDirection : Direction -> Direction
nextDirection dir = case dir of
    SE -> SW
    SW -> NW
    NW -> NE
    NE -> SE
