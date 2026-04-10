module ControlsTest exposing (..)

import Controls exposing (..)
import Duration
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
                        Expect.all
                            [ \d -> Expect.equal M.NW d.dir
                            , \d -> Expect.equal M.QueuedNone d.queuedIntent
                            ]
                            data
                    _ ->
                        Expect.fail "Expected Moving NW with QueuedNone"
        , test "QueuedTurn is consumed when turning corner" <|
            \_ ->
                let
                    pos = ( 2, 2, 0 )
                    currentDir = M.SE
                    exits = [ M.NW, M.NE ] -- A corner at (2,2)
                    res = continueInPath pos 0.0 currentDir [ M.NE ] longPathMaze 1.0 (M.QueuedTurn M.NE) Nothing
                in
                case res of
                    M.Moving data ->
                        Expect.all
                            [ \d -> Expect.equal M.NE d.dir
                            , \d -> Expect.equal M.QueuedNone d.queuedIntent
                            ]
                            data
                    _ ->
                        Expect.fail "Expected turn consumed"
        , test "Same interaction does not queue a turn while moving" <|
            \_ ->
                let
                    interactionId = Just (Duration.seconds 123)
                    intent =
                        { intent0
                        | primaryDir = Just M.NE
                        , secondaryDir = Just M.NW
                        , interactionStart = interactionId
                        }
                    m =
                        { from = (0,0,0), to = (1,0,0), dir = M.NE, progress = 0.5
                        , speedFactor = 1.0, queuedIntent = M.QueuedNone, interactionStart = interactionId
                        }
                    res = updateMoving 0.0 m intent False longPathMaze
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedNone data.queuedIntent
                    _ ->
                        Expect.fail "Expected Moving with QueuedNone"
        , test "Different interaction can queue its own direction (redundant but allowed logic-wise, though effectively neutral)" <|
            \_ ->
                let
                    m =
                        { from = (0,0,0), to = (1,0,0), dir = M.NE, progress = 0.5
                        , speedFactor = 1.0, queuedIntent = M.QueuedNone, interactionStart = Just (Duration.seconds 100)
                        }
                    intent =
                        { intent0
                        | primaryDir = Just M.NE
                        , interactionStart = Just (Duration.seconds 101)
                        }
                    res = updateMoving 0.0 m intent False longPathMaze
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedNone data.queuedIntent
                    _ ->
                        Expect.fail "Expected Moving with QueuedNone (redundant dir ignore)"
        ]
