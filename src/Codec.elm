module Codec exposing (..)

import Array
import List.Extra
import Maze as M

encode : M.Maze -> String
encode maze =
    let
        limits = M.getLimits maze
        config = maze.config
        encodeLight l = l.color ++ "," ++ (String.fromInt <| round l.intensity)

        -- Relative start and end
        relStart = ( M.posX maze.start - limits.minX, M.posY maze.start - limits.minY )
        relEnd = ( M.posX maze.end - limits.minX, M.posY maze.end - limits.minY )

        xSize = limits.maxX - limits.minX + 1
        ySize = limits.maxY - limits.minY + 1

        -- Get blocks in the normalized range
        xRange = List.range limits.minX limits.maxX
        yRange = List.reverse <| List.range limits.minY limits.maxY
        getMazeBlock y x =
            case M.get (x, y) maze of
                Just (M.Base ( _, _, z )) -> M.BaseBlock z
                Just (M.Bridge ( _, _, z )) -> M.BridgeBlock z
                Just (M.Stairs ( _, _, z ) dir) -> M.StairsBlock z dir
                Nothing -> M.EmptyBlock

        parts = []
            ++ (if config.bg == M.defaultConfig.bg then [] else [ "bg:" ++ config.bg ])
            ++ (if config.left == M.defaultConfig.left then [] else [ "left:" ++ encodeLight config.left ])
            ++ (if config.right == M.defaultConfig.right then [] else [ "right:" ++ encodeLight config.right ])
            ++ (if config.above == M.defaultConfig.above then [] else [ "above:" ++ encodeLight config.above ])
            ++ [ "sz:" ++ (String.fromInt xSize) ++ "," ++ (String.fromInt ySize)
               , "st:" ++ (String.fromInt <| Tuple.first relStart) ++ "," ++ (String.fromInt <| Tuple.second relStart)
               , "end:" ++ (String.fromInt <| Tuple.first relEnd) ++ "," ++ (String.fromInt <| Tuple.second relEnd)
               , "mz:" ++ (String.join "" <| List.map encodeMazeBlock (M.mapCoords yRange xRange getMazeBlock))
               ]
    in
    String.join ";" parts

encodeMazeBlock : M.MazeBlock -> String
encodeMazeBlock block =
    case block of
        M.EmptyBlock -> "x"
        M.BaseBlock z -> "o" ++ charFromIndex z
        M.BridgeBlock z -> "l" ++ charFromIndex z
        M.StairsBlock z M.SE -> "s" ++ charFromIndex z
        M.StairsBlock z M.SW -> "z" ++ charFromIndex z
        M.StairsBlock z M.NE -> "Z" ++ charFromIndex z
        M.StairsBlock z M.NW -> "S" ++ charFromIndex z

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
        st = findPart "st:" |> Maybe.andThen parseIntPair
        end = findPart "end:" |> Maybe.andThen parseIntPair
        mz = findPart "mz:"

        parseLight s = case String.split "," s of
            [ c, i ] -> Maybe.map (M.LightConfig c) (String.toFloat i)
            _ -> Nothing

        left = findPart "left:" |> Maybe.andThen parseLight |> Maybe.withDefault M.defaultConfig.left
        right = findPart "right:" |> Maybe.andThen parseLight |> Maybe.withDefault M.defaultConfig.right
        above = findPart "above:" |> Maybe.andThen parseLight |> Maybe.withDefault M.defaultConfig.above
        bg = findPart "bg:" |> Maybe.withDefault M.defaultConfig.bg
        config = { left = left, right = right, above = above, bg = bg }
    in
    case ( ( sz, st ), ( end, mz ) ) of
        ( ( Just ( xSize, ySize ), Just ( stX, stY ) ), ( Just ( endX, endY ), Just mazeStr ) ) ->
            let
                blocks = decodeBlocks (String.toList mazeStr)
                toBlock_ i mb =
                    let
                        x = modBy xSize i
                        y = (ySize - 1) - (i // xSize)
                    in
                    case mb of
                        M.EmptyBlock -> Nothing
                        M.BaseBlock z -> Just (M.Base ( x, y, z ))
                        M.BridgeBlock z -> Just (M.Bridge ( x, y, z ))
                        M.StairsBlock z dir -> Just (M.Stairs ( x, y, z ) dir)

                mazeBlocks = List.indexedMap toBlock_ blocks |> List.filterMap identity
                finalMaze = M.fromBlocks mazeBlocks
            in
            Just { finalMaze | start = ( stX, stY ), end = ( endX, endY ), config = config }

        _ -> Nothing


-- Block encoder/decoder

heightChars : List Char
heightChars = String.toList "0123456789abcdefghijklmnopqrstuvwxyz"

charFromIndex : Int -> String
charFromIndex drop =
    List.drop drop heightChars
        |> List.head
        |> Maybe.withDefault '*'
        |> String.fromChar

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
