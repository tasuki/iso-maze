module ControlsTest exposing (..)

import Controls exposing (..)
import Expect
import Maze as M
import Test exposing (..)
import Codec
import Quantity

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

intent0 = { intent = Nothing, dir = Nothing, speed = 1.0, isLong = False, shouldStop = False, interactionStart = Nothing, isJoystick = False }

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

updateMovingTest =
    describe "updateMoving tests"
        [ test "Long press joystick enters deadzone: should stop in place and queue stop" <|
            \_ ->
                let
                    m = { from = ( 0, 0, 0 ), to = ( 1, 0, 0 ), dir = M.NE, progress = 0.5, speedFactor = 1.0, queuedIntent = M.QueuedNone, interactionStart = Just (Quantity.zero) }
                    intent = { intent = Nothing, dir = Nothing, speed = 1.0, isLong = True, shouldStop = True, interactionStart = Just (Quantity.zero), isJoystick = True }
                    res = updateMoving 0.016 m intent False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedStop data.queuedIntent
                        -- And speed should be 0 (so progress doesn't increase much)
                        -- wait, speed is not directly in MovingData, it's used to calculate newProgress
                        -- progress was 0.5, dt=0.016, speed=0.0 -> newProgress = 0.5
                        |> Expect.all
                            [ \_ -> Expect.equal M.QueuedStop data.queuedIntent
                            , \_ -> Expect.within (Expect.Absolute 0.001) 0.5 data.progress
                            ]
                    _ -> Expect.fail "Expected Moving"
        , test "Long press joystick release in deadzone: should move at speed 1.0 and keep QueuedStop" <|
            \_ ->
                let
                    m = { from = ( 0, 0, 0 ), to = ( 1, 0, 0 ), dir = M.NE, progress = 0.5, speedFactor = 0.0, queuedIntent = M.QueuedStop, interactionStart = Just (Quantity.zero) }
                    -- on release, intent.intent is Nothing, shouldStop is True, isJoystick is True (because pointerStart is still passed in Finished)
                    intent = { intent = Nothing, dir = Nothing, speed = 1.0, isLong = True, shouldStop = True, interactionStart = Just (Quantity.zero), isJoystick = True }
                    res = updateMoving 0.016 m intent True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedStop data.queuedIntent
                        -- isRelease = True -> speed = 1.0
                        -- newProgress = 0.5 + (0.016 * 1.0 / 0.2) = 0.5 + 0.08 = 0.58
                        |> Expect.all
                            [ \_ -> Expect.equal M.QueuedStop data.queuedIntent
                            , \_ -> Expect.within (Expect.Absolute 0.001) 0.58 data.progress
                            ]
                    _ -> Expect.fail "Expected Moving"
        , test "Long press joystick release OUTSIDE deadzone: should set QueuedNone" <|
            \_ ->
                let
                    m = { from = ( 0, 0, 0 ), to = ( 1, 0, 0 ), dir = M.NE, progress = 0.5, speedFactor = 1.0, queuedIntent = M.QueuedNone, interactionStart = Just (Quantity.zero) }
                    -- outside deadzone: maybeIntent is Just
                    intent = { intent = Just (M.Intent 0.0 {nwse=1, nesw=1}), dir = Just M.NE, speed = 1.0, isLong = True, shouldStop = False, interactionStart = Just (Quantity.zero), isJoystick = True }
                    res = updateMoving 0.016 m intent True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedNone data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "Short tap in deadzone: should NOT stop in place (smooth move)" <|
            \_ ->
                let
                    m = { from = ( 0, 0, 0 ), to = ( 1, 0, 0 ), dir = M.NE, progress = 0.5, speedFactor = 1.0, queuedIntent = M.QueuedNone, interactionStart = Just (Quantity.zero) }
                    intent = { intent = Nothing, dir = Nothing, speed = 1.0, isLong = False, shouldStop = True, interactionStart = Just (Quantity.zero), isJoystick = True }
                    res = updateMoving 0.016 m intent False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedStop data.queuedIntent
                        -- speed should be 1.0 for short tap in deadzone (once I fix it)
                        -- Currently it's 0.0 in my buggy version.
                        |> Expect.all
                            [ \_ -> Expect.equal M.QueuedStop data.queuedIntent
                            , \_ -> Expect.within (Expect.Absolute 0.001) 0.58 data.progress
                            ]
                    _ -> Expect.fail "Expected Moving"
        ]
