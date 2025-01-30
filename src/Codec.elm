module Codec exposing (..)

import Array
import List.Extra
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
        ++ ";st:"
        ++ (String.fromInt <| M.posX <| maze.start) ++ ","
        ++ (String.fromInt <| M.posY <| maze.start)
        ++ ";end:"
        ++ (String.fromInt <| M.posX <| maze.end) ++ ","
        ++ (String.fromInt <| M.posY <| maze.end)
        ++ ";mz:" ++ (String.join "" <| List.map encodeBlock cut.maze)

removeSpaces : String -> String
removeSpaces = String.filter (\c -> c /= ' ')

decode : String -> Maybe M.Maze
decode str =
    let
        parts : List String
        parts = str |> removeSpaces |> String.split ";"

        findPart : String -> Maybe String
        findPart prefix = parts
            |> List.filter (String.startsWith prefix)
            |> List.head
            |> Maybe.andThen (String.dropLeft (String.length prefix) >> Just)

        parseIntPair : String -> Maybe (Int, Int)
        parseIntPair s = case String.split "," s of
            [ a, b ] -> Maybe.map2 Tuple.pair (String.toInt a) (String.toInt b)
            _ -> Nothing

        sz = findPart "sz:" |> Maybe.andThen parseIntPair
        off = findPart "off:" |> Maybe.andThen parseIntPair
        st = findPart "st:" |> Maybe.andThen parseIntPair
        end = findPart "end:" |> Maybe.andThen parseIntPair
        mz = findPart "mz:"
    in
    Maybe.map4 SubMaze
        (sz |> Maybe.map Tuple.first)
        (sz |> Maybe.map Tuple.second)
        (off |> Maybe.map Tuple.first)
        (off |> Maybe.map Tuple.second)
        |> Maybe.andThen (\pf -> Maybe.map3 pf st end
            (mz |> Maybe.map (String.toList >> decodeBlocks)))
        |> Maybe.map insertCutout


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
        M.BridgeBlock z -> "l" ++ charFromIndex z
        M.StairsBlock z M.SE -> "s" ++ charFromIndex z
        M.StairsBlock z M.SW -> "z" ++ charFromIndex z
        M.StairsBlock z M.NE -> "Z" ++ charFromIndex z
        M.StairsBlock z M.NW -> "S" ++ charFromIndex z

charToIndex : Char -> Maybe Int
charToIndex c =
    List.Extra.elemIndex c heightChars

decodeBlock : Char -> Char -> Maybe M.MazeBlock
decodeBlock typeChar heightChar =
    charToIndex heightChar
        |> Maybe.andThen (\z ->
            case typeChar of
                'o' -> Just (M.BaseBlock z)
                's' -> Just (M.StairsBlock z M.SE)
                'z' -> Just (M.StairsBlock z M.SW)
                'Z' -> Just (M.StairsBlock z M.NE)
                'S' -> Just (M.StairsBlock z M.NW)
                'l' -> Just (M.BridgeBlock z)
                _ -> Nothing
        )

decodeBlocks : List Char -> List M.MazeBlock
decodeBlocks chars =
    case chars of
        [] -> []
        'x' :: rest -> M.EmptyBlock :: decodeBlocks rest
        typeChar :: heightChar :: rest ->
            case decodeBlock typeChar heightChar of
                Just block -> block :: decodeBlocks rest
                Nothing -> decodeBlocks rest
        _ -> []


-- Cutout maze to minimum rectangle

type alias SubMaze =
    { xSize : Int
    , ySize : Int
    , xOffset : Int
    , yOffset : Int
    , start : M.Pos2d
    , end : M.Pos2d
    , maze : List M.MazeBlock
    }

cutout : M.Maze -> SubMaze
cutout maze =
    let
        limits = mazeLimits maze
        xRange = List.range limits.minX limits.maxX
        yRange = List.reverse <| List.range limits.minY limits.maxY
        getBlock y x = Array.get (M.toIndex x y) maze.maze
    in
    { xSize = limits.maxX - limits.minX + 1
    , ySize = limits.maxY - limits.minY + 1
    , xOffset = limits.minX
    , yOffset = limits.minY
    , start = maze.start
    , end = maze.end
    , maze = M.mapCoords yRange xRange getBlock |> List.filterMap identity
    }

insertCutout : SubMaze -> M.Maze
insertCutout subMaze =
    let
        toBlock : Int -> M.MazeBlock -> Maybe M.Block
        toBlock i block =
            let
                x = (modBy subMaze.xSize i) + subMaze.xOffset
                y = (subMaze.ySize - 1) - (i // subMaze.xSize) + subMaze.yOffset
            in
            M.toBlock ( x, y ) block

        maze = M.emptyMaze
    in
    List.indexedMap toBlock subMaze.maze
        |> List.filterMap identity
        |> List.foldl M.set
            { maze | start = subMaze.start, end = subMaze.end }


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
