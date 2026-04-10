module ControlsTest exposing (..)

import Controls exposing (..)
import Duration
import Expect
import Maze as M
import Test exposing (..)
import Codec
import Quantity

simpleCorridor : M.Maze
simpleCorridor = "sz:10,1;st:0,0;end:9,0;mz:o0o0o0o0o0o0o0o0o0o0"
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

shortDeadEnd : M.Maze
shortDeadEnd = "sz:5,5;st:0,2;end:4,2;mz:"
    ++ "x x x x x "
    ++ "x x o0x x "
    ++ "o0o0o0o0o0"
    ++ "x x o0x x "
    ++ "x x x x x "
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

longPathMaze : M.Maze
longPathMaze = "sz:10,5;st:0,2;end:9,2;mz:"
    ++ "x x x x x x x x x x "
    ++ "x x o0x x x x x x x "
    ++ "o0o0o0o0o0o0o0o0o0o0"
    ++ "x x o0x x x x x x x "
    ++ "x x x x x x x x x x "
    |> Codec.decode |> Maybe.withDefault M.emptyMaze

hasPathTest : Test
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

intent0 : IntentInfo
intent0 =
    { intent = Nothing
    , dir = Nothing
    , speed = 1.0
    , isLong = False
    , shouldStop = False
    , interactionStart = Nothing
    , isJoystick = False
    }

nextTileTest : Test
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
                    queuedIntent = M.QueuedTurn M.NW
                    res = nextTile pos 0.0 queuedIntent currentDir intent0 longPathMaze 1.0
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.NE data.dir
                    _ ->
                        Expect.fail "Expected Moving NE (ignoring short queued turn)"
        ]

movingData : M.MovingData
movingData =
    { from = ( 0, 0, 0 )
    , to = ( 1, 0, 0 )
    , dir = M.NE
    , progress = 0.5
    , speedFactor = 1.0
    , queuedIntent = M.QueuedNone
    , interactionStart = Just (Quantity.zero)
    }

intentLong : IntentInfo
intentLong =
    { intent = Nothing
    , dir = Nothing
    , speed = 1.0
    , isLong = True
    , shouldStop = True
    , interactionStart = Just (Quantity.zero)
    , isJoystick = True
    }

intentShort : IntentInfo
intentShort = { intentLong | isLong = False }

updateMovingTest : Test
updateMovingTest =
    describe "updateMoving tests"
        [ test "Long press joystick enters deadzone: should stop in place and queue stop" <|
            \_ ->
                let
                    res = updateMoving 0.01 movingData intentLong False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        data |> Expect.all
                            [ .queuedIntent >> Expect.equal M.QueuedStop
                            , .progress >> Expect.within (Expect.Absolute 0.001) 0.5
                            ]
                    _ -> Expect.fail "Expected Moving"
        , test "Long press joystick release in deadzone: should move at speed 1.0 and keep QueuedStop" <|
            \_ ->
                let
                    m = { movingData | speedFactor = 0.0, queuedIntent = M.QueuedStop }
                    res = updateMoving 0.01 m intentLong True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        data |> Expect.all
                            [ .queuedIntent >> Expect.equal M.QueuedStop
                            , .progress >> Expect.within (Expect.Absolute 0.001) 0.55
                            ]
                    _ -> Expect.fail "Expected Moving"
        , test "Long press joystick release OUTSIDE deadzone: should set QueuedNone" <|
            \_ ->
                let
                    intent =
                        { intentLong
                        | intent = Just (Intent 0.0 { nwse=1, nesw=1 })
                        , dir = Just M.NE
                        , shouldStop = False
                        }
                    res = updateMoving 0.01 movingData intent True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedNone data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "Short tap in deadzone: should NOT stop in place and should NOT queue stop (until release)" <|
            \_ ->
                let
                    res = updateMoving 0.01 movingData intentShort False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        data |> Expect.all
                            [ .queuedIntent >> Expect.equal M.QueuedNone
                            , .progress >> Expect.within (Expect.Absolute 0.001) 0.55
                            ]
                    _ -> Expect.fail "Expected Moving"
        , test "Short tap release in deadzone: should queue stop" <|
            \_ ->
                let
                    res = updateMoving 0.01 movingData intentShort True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedStop data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "Short swipe and release: should preserve QueuedTurn" <|
            \_ ->
                let
                    m = { movingData | queuedIntent = M.QueuedTurn M.NW }
                    intent =
                        { intentShort
                        | intent = Just (Intent 0.0 { nwse=1, nesw=1 })
                        , dir = Just M.NE
                        , shouldStop = False
                        }
                    res = updateMoving 0.01 m intent True simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal (M.QueuedTurn M.NW) data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "When already moving, a new short swipe in the same direction queues a turn" <|
            \_ ->
                let
                    intent =
                        { intentShort
                        | intent = Just (Intent 0.0 { nwse=1, nesw=1 })
                        , dir = Just M.NE
                        , shouldStop = False
                        , interactionStart = Just (Duration.milliseconds 500)
                        }
                    res = updateMoving 0.01 movingData intent False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal (M.QueuedTurn M.NE) data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "When already moving, a new short swipe in a different direction queues a turn" <|
            \_ ->
                let
                    intent =
                        { intentShort
                        | intent = Just (Intent (5*pi/4) { nwse=-1, nesw=0 })
                        , dir = Just M.NW
                        , shouldStop = False
                        , interactionStart = Just (Duration.milliseconds 500)
                        }
                    res = updateMoving 0.01 movingData intent False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal (M.QueuedTurn M.NW) data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        , test "When the short swipe started the movement, nothing is queued even if direction changes" <|
            \_ ->
                let
                    intent =
                        { intentShort | intent = Just (Intent (5*pi/4) {nwse=-1, nesw=0})
                        , dir = Just M.NW
                        , speed = 1.0
                        , shouldStop = False
                        , interactionStart = Just (Quantity.zero)
                        }
                    res = updateMoving 0.01 movingData intent False simpleCorridor
                in
                case res of
                    M.Moving data ->
                        Expect.equal M.QueuedNone data.queuedIntent
                    _ -> Expect.fail "Expected Moving"
        ]
