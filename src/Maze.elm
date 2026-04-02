module Maze exposing (..)

import Array exposing (Array)
import Maybe.Extra

minHeight = 0
maxHeight = 35

type alias LightConfig =
    { color : String
    , intensity : Float
    }

type alias MazeConfig =
    { left : LightConfig
    , right : LightConfig
    , above : LightConfig
    , bg : String
    }

type alias Maze =
    { maze : Array MazeBlock
    , width : Int
    , height : Int
    , offsetX : Int
    , offsetY : Int
    , start : Pos2d
    , end : Pos2d
    , config : MazeConfig
    }

type MazeBlock
    = EmptyBlock
    | BaseBlock Int
    | BridgeBlock Int
    | StairsBlock Int Direction
    | GreeneryBlock Int

type Block
    = Base Position
    | Bridge Position
    | Stairs Position Direction
    | Greenery Position

type alias Pos2d = ( Int, Int )
type alias Position = ( Int, Int, Int )
type alias Vector = Position

type Direction = SE | SW | NE | NW

type MovementIntent = Intent Float Float

type QueuedIntent
    = QueuedNone
    | QueuedStop
    | QueuedTurn Direction

type alias MovingData =
    { from : Position
    , to : Position
    , dir : Direction
    , progress : Float
    , speedFactor : Float
    , queuedIntent : QueuedIntent
    }

type PlayerState
    = Idle Position
    | Moving MovingData


-- Maze

defaultConfig : MazeConfig
defaultConfig =
    { bg = "689"
    , left = { color = "fc9", intensity = 40 }
    , right = { color = "6bf", intensity = 20 }
    , above = { color = "fff", intensity = 50 }
    }

emptyMaze : Maze
emptyMaze = emptyMazeSize 1 1

emptyMazeSize : Int -> Int -> Maze
emptyMazeSize xSize ySize =
    { maze = Array.initialize (ySize * xSize) (always EmptyBlock)
    , width = xSize
    , height = ySize
    , offsetX = 0
    , offsetY = 0
    , start = (0, 0)
    , end = (0, 0)
    , config = defaultConfig
    }

toIndex : Int -> Int -> Int -> Int -> Int -> Int
toIndex width offX offY x y = (y - offY) * width + (x - offX)

fromIndex : Int -> Int -> Int -> Int -> (Int, Int)
fromIndex width offX offY i = (offX + modBy width i, offY + i // width)

fromBlocks : List Block -> Maze
fromBlocks blocks =
    let
        extremes ( x, y, _ ) acc =
            { minX = min acc.minX x
            , maxX = max acc.maxX x
            , minY = min acc.minY y
            , maxY = max acc.maxY y
            }

        limits =
            case blocks of
                [] -> { minX = 0, maxX = 0, minY = 0, maxY = 0 }
                first :: rest ->
                    List.foldl (\b acc -> extremes (blockPosition b) acc)
                        (let ( fx, fy, _ ) = blockPosition first in { minX = fx, maxX = fx, minY = fy, maxY = fy })
                        rest

        width = limits.maxX - limits.minX + 1
        height = limits.maxY - limits.minY + 1

        maze =
            { maze = Array.initialize (width * height) (always EmptyBlock)
            , width = width
            , height = height
            , offsetX = limits.minX
            , offsetY = limits.minY
            , start = (0, 0)
            , end = (0, 0)
            , config = defaultConfig
            }

        setBlock b m =
            let ( x, y, _ ) = blockPosition b in
            { m | maze = Array.set (toIndex m.width m.offsetX m.offsetY x y) (blockToMazeBlock b) m.maze }
    in
    List.foldl setBlock maze blocks

blockToMazeBlock : Block -> MazeBlock
blockToMazeBlock block =
    case block of
        Base ( _, _, z ) -> BaseBlock z
        Bridge ( _, _, z ) -> BridgeBlock z
        Stairs ( _, _, z ) dir -> StairsBlock z dir
        Greenery ( _, _, z ) -> GreeneryBlock z

setAt : Position -> MazeBlock -> Maze -> Maze
setAt ( x, y, _ ) mazeBlock maze =
    if isValidPos2d maze ( x, y ) then
        { maze | maze = Array.set (toIndex maze.width maze.offsetX maze.offsetY x y) mazeBlock maze.maze }
    else
        -- grow the maze
        let
            newMinX = min maze.offsetX x
            newMinY = min maze.offsetY y
            newMaxX = max (maze.offsetX + maze.width - 1) x
            newMaxY = max (maze.offsetY + maze.height - 1) y
            newWidth = newMaxX - newMinX + 1
            newHeight = newMaxY - newMinY + 1

            newArray = Array.initialize (newWidth * newHeight) (always EmptyBlock)

            copy i block acc =
                let
                    ( ox, oy ) = fromIndex maze.width maze.offsetX maze.offsetY i
                    newI = toIndex newWidth newMinX newMinY ox oy
                in
                Array.set newI block acc

            expandedArray = Array.foldl (\b (i, arr) -> (i + 1, copy i b arr)) (0, newArray) maze.maze |> Tuple.second
            finalI = toIndex newWidth newMinX newMinY x y
        in
        { maze
            | maze = Array.set finalI mazeBlock expandedArray
            , width = newWidth
            , height = newHeight
            , offsetX = newMinX
            , offsetY = newMinY
        }

set : Block -> Maze -> Maze
set block maze =
    setAt (blockPosition block) (blockToMazeBlock block) maze

clear : Pos2d -> Maze -> Maze
clear ( x, y ) maze =
    if isValidPos2d maze ( x, y ) then
        { maze | maze = Array.set (toIndex maze.width maze.offsetX maze.offsetY x y) EmptyBlock maze.maze }
    else
        maze

toBlocks : Maze -> List Block
toBlocks maze =
    Array.toIndexedList maze.maze
        |> List.filterMap (\( i, mb ) -> toBlock (fromIndex maze.width maze.offsetX maze.offsetY i) mb)

mapCoords : List Int -> List Int -> (Int -> Int -> a) -> List a
mapCoords rangeY rangeX fun =
    List.concatMap (\y -> List.map (fun y) rangeX) rangeY

toBlock : Pos2d -> MazeBlock -> Maybe Block
toBlock ( x, y ) mazeBlock = case mazeBlock of
    EmptyBlock -> Nothing
    BaseBlock z -> Just <| Base (x, y, z)
    BridgeBlock z -> Just <| Bridge (x, y, z)
    StairsBlock z dir -> Just <| Stairs (x, y, z) dir
    GreeneryBlock z -> Just <| Greenery (x, y, z)

get : Pos2d -> Maze -> Maybe Block
get ( x, y ) maze =
    if x < maze.offsetX || y < maze.offsetY || x >= maze.offsetX + maze.width || y >= maze.offsetY + maze.height then
        Nothing
    else
        Array.get (toIndex maze.width maze.offsetX maze.offsetY x y) maze.maze
            |> Maybe.andThen (toBlock ( x, y ))

getPosition : Pos2d -> Maze -> Maybe Position
getPosition pos = get pos >> Maybe.map blockPosition

startPosition : Maze -> Position
startPosition m = getPosition m.start m |> Maybe.withDefault ( Tuple.first m.start, Tuple.second m.start, 0 )

endPosition : Maze -> Position
endPosition m = getPosition m.end m |> Maybe.withDefault ( Tuple.first m.end, Tuple.second m.end, 0 )

exitHeight : Direction -> Int -> Block -> Maybe Int
exitHeight dir playerHeight block =
    case block of
        Base ( _, _, z ) ->
            if playerHeight == z then
                Just playerHeight
            else
                Nothing
        Bridge ( _, _, z ) ->
            if playerHeight == z || playerHeight == z - 1 then
                Just playerHeight
            else
                Nothing
        Stairs ( _, _, z ) stairsDir ->
            if playerHeight == z && stairsDir == dir then
                Just <| playerHeight - 1
            else if playerHeight == z && stairsDir == oppositeDirection dir then
                Just <| playerHeight
            else
                Nothing
        Greenery _ ->
            Nothing

moveInHeight : Int -> Block -> Int
moveInHeight enterHeight block =
    case block of
        Stairs ( _, _, nz ) _ -> nz
        _ -> enterHeight

move : Position -> Direction -> Maze -> Maybe Position
move ( x, y, z ) dir maze =
    let
        oldExitHeight : Maybe Int
        oldExitHeight = Maybe.andThen (exitHeight dir z) (get ( x, y ) maze)

        newPos2d : Pos2d
        newPos2d = shiftPos2d ( x, y ) dir

        newBlock : Maybe Block
        newBlock = get newPos2d maze

        newHeight : Maybe Int
        newHeight = Maybe.map2 moveInHeight oldExitHeight newBlock

        newExitHeight : Maybe Int
        newExitHeight =
            Maybe.map2 (exitHeight (oppositeDirection dir)) newHeight newBlock
                |> Maybe.andThen identity
    in
    Maybe.map2 (==) oldExitHeight newExitHeight
        |> Maybe.Extra.filter identity
        |> Maybe.andThen (\_ -> newHeight)
        |> Maybe.map (\nz -> ( posX newPos2d, posY newPos2d, nz ))

getExits : Position -> Maze -> List Direction
getExits pos maze =
    List.filter (\d -> move pos d maze /= Nothing) allDirections

isJunction : Position -> Maze -> Bool
isJunction pos maze =
    let exits = getExits pos maze in
    pos == endPosition maze || List.length exits /= 2

playerPos : PlayerState -> Position
playerPos state =
    case state of
        Idle pos -> pos
        Moving m -> m.from

playerDir : PlayerState -> Direction
playerDir state =
    case state of
        Idle _ -> SE -- dummy
        Moving m -> m.dir


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
    Greenery pos -> pos


-- Position

type alias Limits =
    { minX: Int
    , maxX: Int
    , minY: Int
    , maxY: Int
    }

getLimits : Maze -> Limits
getLimits maze =
    let
        extremes ( x, y ) acc =
            { minX = min acc.minX x
            , maxX = max acc.maxX x
            , minY = min acc.minY y
            , maxY = max acc.maxY y
            }

        allBlocks = toBlocks maze
    in
    case allBlocks of
        [] -> { minX = 0, maxX = 0, minY = 0, maxY = 0 }
        first :: rest ->
            let ( fx, fy, _ ) = blockPosition first in
            List.foldl (\b acc -> let ( x, y, _ ) = blockPosition b in extremes ( x, y ) acc)
                { minX = fx, maxX = fx, minY = fy, maxY = fy }
                rest

normalize : Maze -> ( Maze, ( Int, Int ) )
normalize maze =
    let
        limits = getLimits maze
        dx = -limits.minX
        dy = -limits.minY
    in
    if dx == 0 && dy == 0 && maze.width == (limits.maxX - limits.minX + 1) && maze.height == (limits.maxY - limits.minY + 1) && maze.offsetX == 0 && maze.offsetY == 0 then
        ( maze, ( 0, 0 ) )
    else
        let
            newWidth = limits.maxX - limits.minX + 1
            newHeight = limits.maxY - limits.minY + 1

            newMazeArray = Array.initialize (newWidth * newHeight) (always EmptyBlock)

            reposition b arr =
                let ( x, y, _ ) = blockPosition b in
                Array.set (toIndex newWidth 0 0 (x + dx) (y + dy)) (blockToMazeBlock b) arr

            finalArray = List.foldl reposition newMazeArray (toBlocks maze)
        in
        ( { maze
            | maze = finalArray
            , width = newWidth
            , height = newHeight
            , offsetX = 0
            , offsetY = 0
            , start = ( Tuple.first maze.start + dx, Tuple.second maze.start + dy )
            , end = ( Tuple.first maze.end + dx, Tuple.second maze.end + dy )
          }
        , ( dx, dy )
        )

shiftPosition : Position -> Vector -> Position
shiftPosition ( x, y, z ) ( xd, yd, zd ) =
    if z + zd >= minHeight && z + zd <= maxHeight
        then ( x + xd, y + yd, z + zd )
        else ( x + xd, y + yd, z )

shiftPos2d : Pos2d -> Direction -> Pos2d
shiftPos2d ( x, y ) dir =
    case dir of
        SE -> ( x, y - 1 )
        SW -> ( x - 1, y )
        NW -> ( x, y + 1 )
        NE -> ( x + 1, y )

positionTo2d : Position -> Pos2d
positionTo2d ( px, py, _ ) = ( px, py )

positionZ : Position -> Int
positionZ ( _, _, pz ) = pz

posX : Pos2d -> Int
posX = Tuple.first

posY : Pos2d -> Int
posY = Tuple.second

isValidPos2d : Maze -> Pos2d -> Bool
isValidPos2d maze ( x, y ) =
    x >= maze.offsetX &&
    y >= maze.offsetY &&
    x < maze.offsetX + maze.width &&
    y < maze.offsetY + maze.height

isFocusValid : Pos2d -> Maze -> Bool
isFocusValid ( x, y ) maze =
    let
        blocks = toBlocks maze
        isNear (bx, by, _) =
            abs (x - bx) <= 1 && abs (y - by) <= 1
    in
    if List.isEmpty blocks then
        x == 0 && y == 0
    else
        List.any (blockPosition >> isNear) blocks

findClosestPoint : List Block -> Position -> Position
findClosestPoint blocks ( xf, yf, zf ) =
    let
        closestPoint b =
            let ( bx, by, _ ) = blockPosition b in
            ( clamp (bx - 1) (bx + 1) xf
            , clamp (by - 1) (by + 1) yf
            )

        dist ( x1, y1 ) ( x2, y2 ) = max (abs (x1 - x2)) (abs (y1 - y2))
        points = List.map closestPoint blocks

        bestPoint =
            case points of
                [] -> ( 0, 0 )
                first :: rest ->
                    List.foldl
                        (\p acc ->
                            if dist p ( xf, yf ) < dist acc ( xf, yf )
                                then p
                                else acc
                        )
                        first
                        rest
    in
    ( Tuple.first bestPoint, Tuple.second bestPoint, zf )

snapFocus : Position -> Maze -> Position
snapFocus position maze =
    let
        ( xf, yf, zf ) = position
        blocks = toBlocks maze
    in
    if List.isEmpty blocks then
        ( 0, 0, zf )
    else if isFocusValid ( xf, yf ) maze then
        position
    else
        findClosestPoint blocks position

mapAllCoords : (Int -> Int -> a) -> List a
mapAllCoords fun =
    mapCoords (List.range -10 20) (List.range -10 20) fun


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
