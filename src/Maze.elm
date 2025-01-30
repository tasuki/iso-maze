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

type alias Maze =
    { maze : Array MazeBlock
    , start : Pos2d
    , end : Pos2d
    }
type MazeBlock
    = EmptyBlock
    | BaseBlock Int
    | BridgeBlock Int
    | StairsBlock Int Direction

type Block
    = Base Position
    | Bridge Position
    | Stairs Position Direction

type alias Pos2d = ( Int, Int )
type alias Position = ( Int, Int, Int )
type alias Vector = Position

type Direction = SE | SW | NE | NW


-- Maze

emptyMaze : Maze
emptyMaze = emptyMazeSize sideSize sideSize

emptyMazeSize : Int -> Int -> Maze
emptyMazeSize xSize ySize =
    { maze = Array.initialize (ySize * xSize) (always EmptyBlock)
    , start = (0, 0)
    , end = (0, 0)
    }

toIndex : Int -> Int -> Int
toIndex x y = (y - minTileCoord) * sideSize + (x - minTileCoord)

fromBlocks : List Block -> Maze
fromBlocks blocks = List.foldl set emptyMaze blocks

set : Block -> Maze -> Maze
set block maze =
    let
        ( xx, yy, mazeBlock ) = case block of
            Base ( x, y, z ) -> ( x, y, BaseBlock z )
            Bridge ( x, y, z ) -> ( x, y, BridgeBlock z )
            Stairs ( x, y, z ) dir -> ( x, y, StairsBlock z dir )
    in
    if isValidPosition <| blockPosition block then
        { maze | maze = Array.set (toIndex xx yy) mazeBlock maze.maze }
    else
        maze

clear : Pos2d -> Maze -> Maze
clear ( x, y ) maze =
    if isValidPos2d ( x, y ) then
        { maze | maze = Array.set (toIndex x y) EmptyBlock maze.maze }
    else
        maze

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
    BridgeBlock z -> Just <| Bridge (x, y, z)
    StairsBlock z dir -> Just <| Stairs (x, y, z) dir

get : Pos2d -> Maze -> Maybe Block
get ( x, y ) maze =
    if isValidPos2d ( x, y ) then
        Array.get (toIndex x y) maze.maze
            |> Maybe.andThen (toBlock ( x, y ))
    else
        Nothing

getPosition : Pos2d -> Maze -> Maybe Position
getPosition pos = get pos >> Maybe.map blockPosition

startPosition : Maze -> Position
startPosition m = getPosition m.start m |> Maybe.withDefault ( 0, 0, 0 )

endPosition : Maze -> Position
endPosition m = getPosition m.end m |> Maybe.withDefault ( 0, 0, 0 )

canExit : Position -> Direction -> Maze -> Maybe Position
canExit ( x, y, z ) dir maze =
    let
        threedimensionalize zz ( xx, yy ) = ( xx, yy, zz )
    in
    case get ( x, y ) maze of
        Nothing -> Nothing
        Just (Base _) ->
            Just <| threedimensionalize z <| shiftPos2d ( x, y ) dir
        Just (Bridge _) ->
            Just <| threedimensionalize z <| shiftPos2d ( x, y ) dir
        Just (Stairs _ stairsDir) ->
            if stairsDir == dir then
                Just <| threedimensionalize (z - 1) <| shiftPos2d ( x, y ) dir
            else if stairsDir == oppositeDirection dir then
                Just <| threedimensionalize z <| shiftPos2d ( x, y ) dir
            else
                Nothing

move : Position -> Direction -> Maze -> Maybe Position
move pos dir maze =
    let
        canEnter : Position -> Maybe Position
        canEnter neighborPos =
            canExit neighborPos (oppositeDirection dir) maze
                |> Maybe.map (always pos)
    in
    canExit pos dir maze |>
        Maybe.andThen canEnter


-- Block

createBase : Int -> Int -> Int -> Block
createBase x y z = Base ( x, y, z )

createStairs : Int -> Int -> Int -> Direction -> Block
createStairs x y z dir = Stairs ( x, y, z ) dir

blockPosition : Block -> Position
blockPosition block = case block of
    Base pos -> pos
    Bridge pos -> pos
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

isValidPos2d : Pos2d -> Bool
isValidPos2d ( x, y ) = isValidPosition ( x, y, 0 )

shiftPosition : Position -> Vector -> Position
shiftPosition ( x, y, z ) ( xd, yd, zd ) = ( x + xd, y + yd, z + zd )

shiftPos2d : Pos2d -> Direction -> Pos2d
shiftPos2d ( x, y ) dir =
    case dir of
        SE -> ( x, y - 1 )
        SW -> ( x - 1, y )
        NW -> ( x, y + 1 )
        NE -> ( x + 1, y )

posX : Pos2d -> Int
posX = Tuple.first

posY : Pos2d -> Int
posY = Tuple.second


-- Direction

allDirections : List Direction
allDirections = [ SE, SW, NW, NE ]

nextDirection : Direction -> Direction
nextDirection dir = case dir of
    SE -> SW
    SW -> NW
    NW -> NE
    NE -> SE

oppositeDirection : Direction -> Direction
oppositeDirection dir = case dir of
    SE -> NW
    SW -> NE
    NW -> SE
    NE -> SW
