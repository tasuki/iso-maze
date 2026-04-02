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
-- Junction at (2, 2). Path North is (2, 3), then Dead End. Length 1.
-- Path South is (2, 1), then Dead End. Length 1.
-- Path East is (3, 2), (4, 2). Length 2. (Wait, let me count)
-- (2,2) -> NE -> (3,2) -> NE -> (4,2) (End). Length 2.
-- Still all "short" (< 4).

longPathMaze = "sz:10,5;st:0,2;end:9,2;mz:"
    ++ "x x x x x x x x x x "
    ++ "x x o0x x x x x x x "
    ++ "o0o0o0o0o0o0o0o0o0o0"
    ++ "x x o0x x x x x x x "
    ++ "x x x x x x x x x x "
    |> Codec.decode |> Maybe.withDefault M.emptyMaze
-- Junction at (2, 2).
-- North: (2, 3) -> Dead. Length 1.
-- South: (2, 1) -> Dead. Length 1.
-- West: (1, 2), (0, 2). Length 2.
-- East: (3, 2), (4, 2), (5, 2), (6, 2), (7, 2), (8, 2), (9, 2). Length 7.

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

intent0 = { intent = Nothing, dir = Nothing, speed = 1.0, isLong = False, isDeadzone = False, interactionStart = Nothing }

nextTileTest =
    describe "nextTile tests"
        [ test "Snowman automatically continues through junction with only one long forward path" <|
            \_ ->
                let
                    -- Junction at (2, 2).
                    -- Entry from (1, 2) (dir NE).
                    -- Forward long path: NE (to (3,2)).
                    -- Forward short paths: NW (to (2,3)), SE (to (2,1)).
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
                    -- All paths are long.
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    longCrossMaze = "sz:20,20;st:0,2;end:19,2;mz:"
                        ++ String.repeat 20 "x x o0x x x x x x x x x x x x x x x x x \n"
                        |> String.replace "o0" (String.repeat 20 "o0")
                    -- Re-design cross maze manually to be sure
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
                    -- Entry from (1, 2) (dir NE).
                    -- All forward paths are short.
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
                    -- Try to turn to NW (short dead end)
                    res = nextTile pos 0.0 (M.QueuedTurn M.NW) currentDir intent0 longPathMaze 1.0
                in
                case res of
                    M.Moving data ->
                        -- Should follow the only long path (NE) instead of turning to NW
                        Expect.equal M.NE data.dir
                    _ ->
                        Expect.fail "Expected Moving NE (ignoring short queued turn)"
        ]
