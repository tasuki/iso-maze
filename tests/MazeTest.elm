module MazeTest exposing (..)

import Codec
import Expect
import Maze exposing (..)
import Test exposing (..)


simpleEightMaze = "sz:5,5;st:4,4;end:0,0;mz:"
    ++ "x x o1o1o1"
    ++ "x o0s1o1o1"
    ++ "o0z1l1o1o1"
    ++ "o0x o0x x "
    ++ "o0o0o0o1x "
        |> Codec.decode |> Maybe.withDefault emptyMaze

moveTest =
    describe "Move test"
        [ test "Can move on regular base block" <|
            \_ -> Expect.equal
                (Just ( 0, 1, 0 ))
                (move ( 0, 0, 0 ) NW simpleEightMaze)
        , test "Can move other way on regular base block" <|
            \_ -> Expect.equal
                (Just ( 1, 0, 0 ))
                (move ( 0, 0, 0 ) NE simpleEightMaze)
        , test "Can't jump off a regular base block" <|
            \_ -> Expect.equal
                Nothing
                (move ( 0, 0, 0 ) SE simpleEightMaze)
        , test "Can't jump the other way off a regular base block" <|
            \_ -> Expect.equal
                Nothing
                (move ( 0, 0, 0 ) SW simpleEightMaze)
        , test "Can't move to base above" <|
            \_ -> Expect.equal
                Nothing
                (move ( 2, 0, 0 ) NE simpleEightMaze)
        , test "Can't move to base below" <|
            \_ -> Expect.equal
                Nothing
                (move ( 3, 0, 0 ) SW simpleEightMaze)

        -- Tunnel/Bridge
        , test "Can move into the tunnel" <|
            \_ -> Expect.equal
                (Just ( 2, 2, 0 ))
                (move ( 2, 1, 0 ) NW simpleEightMaze)
        , test "Can move onto the bridge" <|
            \_ -> Expect.equal
                (Just ( 2, 2, 1 ))
                (move ( 3, 2, 1 ) SW simpleEightMaze)

        -- Stairs
        , test "Can move up to the stairs" <|
            \_ -> Expect.equal
                (Just ( 1, 2, 1 ))
                (move ( 0, 2, 0 ) NE simpleEightMaze)
        , test "Can move down from the stairs" <|
            \_ -> Expect.equal
                (Just ( 0, 2, 0 ))
                (move ( 1, 2, 1 ) SW simpleEightMaze)
        , test "Can move up from the stairs to the bridge" <|
            \_ -> Expect.equal
                (Just ( 2, 2, 1 ))
                (move ( 1, 2, 1 ) NE simpleEightMaze)
        , test "Can move from the bridge to the stairs" <|
            \_ -> Expect.equal
                (Just ( 1, 2, 1 ))
                (move ( 2, 2, 1 ) SW simpleEightMaze)

        -- Other stairs
        , test "Can move to the other stairs" <|
            \_ -> Expect.equal
                (Just ( 2, 3, 1 ))
                (move ( 2, 4, 1 ) SE simpleEightMaze)
        , test "Can move up from the other stairs" <|
            \_ -> Expect.equal
                (Just ( 2, 4, 1 ))
                (move ( 2, 3, 1 ) NW simpleEightMaze)
        , test "Can move down the other stairs into the tunnel" <|
            \_ -> Expect.equal
                (Just ( 2, 2, 0 ))
                (move ( 2, 3, 1 ) SE simpleEightMaze)
        , test "Can move up to the other stairs from the tunnel" <|
            \_ -> Expect.equal
                (Just ( 2, 3, 1 ))
                (move ( 2, 2, 0 ) NW simpleEightMaze)
        , test "Can't jump from bridge to stairs" <|
            \_ -> Expect.equal
                Nothing
                (move ( 2, 2, 1 ) NW simpleEightMaze)
        , test "Can't move sideways from the other stairs" <|
            \_ -> Expect.equal
                Nothing
                (move ( 2, 3, 1 ) SW simpleEightMaze)
        , test "Can't move sideways other dir from the other stairs" <|
            \_ -> Expect.equal
                Nothing
                (move ( 2, 3, 1 ) NE simpleEightMaze)

        -- Greenery
        , test "Can't move onto greenery" <|
            \_ ->
                let
                    mazeWithGreenery = "sz:3,1;st:0,0;end:2,0;mz:o0g0o0"
                        |> Codec.decode |> Maybe.withDefault emptyMaze
                in
                Expect.equal
                    Nothing
                    (move ( 0, 0, 0 ) NE mazeWithGreenery)
        ]


focusTest =
    describe "Focus test"
        [ test "isFocusValid: empty maze is valid at (0,0)" <|
            \_ -> Expect.equal True (isFocusValid ( 0, 0 ) emptyMaze)
        , test "isFocusValid: empty maze is invalid at (1,0)" <|
            \_ -> Expect.equal False (isFocusValid ( 1, 0 ) emptyMaze)
        , test "isFocusValid: valid at block position" <|
            \_ -> Expect.equal True (isFocusValid ( 0, 0 ) simpleEightMaze)
        , test "isFocusValid: valid in 3x3 neighborhood" <|
            \_ -> Expect.equal True (isFocusValid ( -1, -1 ) simpleEightMaze)
        , test "isFocusValid: invalid outside 3x3 neighborhood" <|
            \_ -> Expect.equal False (isFocusValid ( -2, 0 ) simpleEightMaze)
        , test "snapFocus: snaps to (0,0) in empty maze" <|
            \_ -> Expect.equal ( 0, 0, 5 ) (snapFocus ( 10, 10, 5 ) emptyMaze)
        , test "snapFocus: snaps to neighbor of existing block" <|
            \_ ->
                let
                    maze =
                        fromBlocks [ Base ( 0, 0, 0 ) ]
                in
                Expect.equal ( 1, 1, 5 ) (snapFocus ( 10, 10, 5 ) maze)
        , test "snapFocus: keeps position if already valid" <|
            \_ ->
                let
                    maze =
                        fromBlocks [ Base ( 0, 0, 0 ) ]
                in
                Expect.equal ( 1, 0, 5 ) (snapFocus ( 1, 0, 5 ) maze)
        , test "isFocusValid: invalid at grid boundary if no block is near" <|
            \_ ->
                -- simpleEightMaze has blocks, but (10, 10) is far away even if it's within some theoretical array
                Expect.equal False (isFocusValid ( 10, 10 ) simpleEightMaze)
        ]
