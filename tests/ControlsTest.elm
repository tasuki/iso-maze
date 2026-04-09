module ControlsTest exposing (..)

import Controls exposing (..)
import Expect
import Maze as M
import Test exposing (..)
import Codec

simpleCorridor = "sz:10,1;st:0,0;end:9,0;mz:o0o0o0o0o0o0o0o0o0o0"
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

shortDeadEnd = "sz:5,5;st:0,2;end:4,2;mz:"
    ++ "x x x x x "
    ++ "x x o0x x "
    ++ "o0o0o0o0o0"
    ++ "x x o0x x "
    ++ "x x x x x "
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

longPathMaze = "sz:10,5;st:0,2;end:9,2;mz:"
    ++ "x x x x x x x x x x "
    ++ "x x o0x x x x x x x "
    ++ "o0o0o0o0o0o0o0o0o0o0"
    ++ "x x o0x x x x x x x "
    ++ "x x x x x x x x x x "
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

hasPathTest =
    describe "hasPath tests"
        [ test "Path of length 4 exists in corridor" <|
            \_ -> Expect.equal True (hasPath 4 ( 0, 0, 0 ) M.NE simpleCorridor)
        , test "Path of length 11 does not exist in 10-cell corridor" <|
            \_ -> Expect.equal False (hasPath 11 ( 0, 0, 0 ) M.NE simpleCorridor)
        , test "Short dead end is correctly identified" <|
            \_ -> Expect.equal False (hasPath 4 ( 2, 2, 0 ) M.NW shortDeadEnd)
        , test "Long path is correctly identified" <|
            \_ -> Expect.equal True (hasPath 4 ( 2, 2, 0 ) M.NE longPathMaze)
        ]

intent0 = { intent = Nothing, primaryDir = Nothing, secondaryDir = Nothing, isLong = False, shouldStop = False, interactionStart = Nothing }

nextTileTest =
    describe "nextTile tests"
        [ test "Snowman automatically continues through junction with only one long forward path" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    result = nextTile pos 0.0 M.QueuedNone currentDir intent0 longPathMaze 1.0
                in
                case result of
                    M.Moving data ->
                        Expect.equal M.NE data.dir
                    _ ->
                        Expect.fail "Expected Moving NE"
        , test "Snowman stops at junction with multiple long forward paths" <|
            \_ ->
                let
                    crossBlocks =
                        List.map (\i -> M.Base ( i, 10, 0 )) (List.range 0 20) ++
                        List.map (\i -> M.Base ( 10, i, 0 )) (List.range 0 20)
                    maze = M.fromBlocks crossBlocks
                    res = nextTile ( 10, 10, 0 ) 0.0 M.QueuedNone M.NE intent0 maze 1.0
                in
                case res of
                    M.Idle _ -> Expect.pass
                    _ -> Expect.fail "Expected Idle at multiple-path junction"
        , test "Snowman stops at junction with only short dead ends" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    res = nextTile pos 0.0 M.QueuedNone currentDir intent0 shortDeadEnd 1.0
                in
                case res of
                    M.Idle _ -> Expect.pass
                    _ -> Expect.fail "Expected Idle at short dead end junction"
        , test "Queued turn to short path is ignored" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    res = nextTile pos 0.0 (M.QueuedTurn M.NW) currentDir intent0 longPathMaze 1.0
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.NE data.dir
                    _ ->
                        Expect.fail "Expected Moving NE (ignoring short queued turn)"
        ]
