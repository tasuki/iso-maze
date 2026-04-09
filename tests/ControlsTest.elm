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


intent0 = { intent = Nothing, primaryDir = Nothing, secondaryDir = Nothing, primarySpeed = 0, secondarySpeed = 0, isLong = False, shouldStop = False, interactionStart = Nothing }

nextTileTest =
    describe "nextTile tests"
        [ test "Snowman stops at junction when no intent" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    result = nextTile pos 0.0 M.QueuedNone currentDir intent0 longPathMaze 1.0
                in
                case result of
                    M.Idle _ -> Expect.pass
                    _ -> Expect.fail "Expected Idle at junction"
        , test "Snowman turns at junction if turn queued" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.NE
                    res = nextTile pos 0.0 (M.QueuedTurn M.NW) currentDir intent0 longPathMaze 1.0
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.NW data.dir
                    _ ->
                        Expect.fail "Expected Moving NW"
        ]
