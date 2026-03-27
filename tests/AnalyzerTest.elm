module AnalyzerTest exposing (..)

import Analyzer exposing (analyze)
import Codec
import Expect
import Maze as M
import Set
import Test exposing (..)


simplePath = "sz:3,1;st:0,0;end:2,0;mz:o0o0o0"
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

loopMaze = "sz:3,3;st:0,0;end:2,0;mz:"
    ++ "o0o0o0"
    ++ "o0x o0"
    ++ "o0o0o0"
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

isolatedMaze = "sz:3,3;st:0,0;end:2,2;mz:"
    ++ "x x o1"
    ++ "x x x "
    ++ "x o0x "
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

bridgeMaze = "sz:3,1;st:0,0;end:2,0;mz:o1l1o1"
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

stairsMaze = "sz:2,1;st:0,0;end:1,0;mz:o0s1"
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

occludedMaze = "sz:6,5;st:3,4;end:4,4;mz:"
    ++ "x S1o0o0o0o0"
    ++ "o1o1o1o0o2o0"
    ++ "o2o2o0o0o0o1"
    ++ "o3z1o1x o3x "
    ++ "x o2o3x x x "
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

suite : Test
suite =
    let
        analyze_ m = analyze ( 0, 0, 0 ) m
    in
    describe "Analyzer Test"
        [ describe "Simple Path"
            [ test "Reachable" <| \_ -> (analyze_ simplePath).reachable |> Expect.equal True
            , test "Occluding" <| \_ -> (analyze_ simplePath).occluding |> Expect.equal (Set.fromList [])
            , test "Unreachable" <| \_ -> (analyze_ simplePath).unreachable |> Expect.equal (Set.fromList [])
            , test "Total Cells" <| \_ -> (analyze_ simplePath).totalCells |> Expect.equal 3
            , test "Shortest Path" <| \_ -> (analyze_ simplePath).shortestPathLength |> Expect.equal (Just 2)
            , test "River Factor" <| \_ -> (analyze_ simplePath).riverFactor |> Expect.within (Expect.Absolute 0.01) (3 / 2) -- 3 cells / (2 dead ends + 0 junctions)
            , test "Loop Count" <| \_ -> (analyze_ simplePath).loopCount |> Expect.equal 0
            ]
        , describe "Loop Maze"
            [ test "Reachable" <| \_ -> (analyze_ loopMaze).reachable |> Expect.equal True
            , test "Occluding" <| \_ -> (analyze_ simplePath).occluding |> Expect.equal (Set.fromList [])
            , test "Unreachable" <| \_ -> (analyze_ simplePath).unreachable |> Expect.equal (Set.fromList [])
            , test "Total Cells" <| \_ -> (analyze_ loopMaze).totalCells |> Expect.equal 8
            , test "Loop Count" <| \_ -> (analyze_ loopMaze).loopCount |> Expect.equal 1
            ]
        , describe "Isolated Maze"
            [ test "Not Reachable" <| \_ -> (analyze_ isolatedMaze).reachable |> Expect.equal False
            , test "Occluding" <| \_ -> (analyze_ simplePath).occluding |> Expect.equal (Set.fromList [])
            , test "Unreachable" <| \_ -> (analyze_ isolatedMaze).unreachable |> Expect.equal
                (Set.fromList
                    [ ( 1, 0, 0 )
                    , ( 2, 2, 1 )
                    ]
                )
            , test "Total Cells" <| \_ -> (analyze_ isolatedMaze).totalCells |> Expect.equal 2
            , test "Shortest Path" <| \_ -> (analyze_ isolatedMaze).shortestPathLength |> Expect.equal Nothing
            ]
        , describe "Bridge Maze"
            [ test "Reachable" <| \_ -> (analyze_ bridgeMaze).reachable |> Expect.equal True
            , test "Occluding" <| \_ -> (analyze_ simplePath).occluding |> Expect.equal (Set.fromList [])
            , test "Unreachable" <| \_ -> (analyze_ bridgeMaze).unreachable |> Expect.equal (Set.fromList [ ( 1, 0, 0 ) ])
            , test "Total Cells" <| \_ -> (analyze_ bridgeMaze).totalCells |> Expect.equal 4
            , test "Path through bridge" <| \_ -> (analyze_ bridgeMaze).shortestPathLength |> Expect.equal (Just 2)
            ]
        , describe "Occluded Maze"
            [ test "Occluding" <| \_ -> (analyze ( 3, 4, 0 ) occludedMaze).occluding |> Expect.equal
                (Set.fromList
                    [ ( 2, 0, 3 )
                    , ( 4, 1, 3 )
                    , ( 4, 3, 2 )
                    ]
                )
            ]
        , describe "Holes"
            [ test "No holes in simple path" <| \_ -> (analyze_ simplePath).holes |> Expect.equal 0
            , test "One hole enclosed" <| \_ ->
                let
                    holeMaze = "sz:3,3;st:0,0;end:2,0;mz:"
                        ++ "o0o0o0"
                        ++ "o0x o0"
                        ++ "o0o0o0"
                        |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ holeMaze).holes |> Expect.equal 1
            , test "Multiple holes enclosed" <| \_ ->
                let
                    holesMaze = "sz:4,4;st:0,0;end:3,0;mz:"
                        ++ "o0o0o0o0"
                        ++ "o0x x o0"
                        ++ "o0x x o0"
                        ++ "o0o0o0o0"
                        |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ holesMaze).holes |> Expect.equal 4
            , test "Not a hole if open to edge" <| \_ ->
                let
                    notAHoleMaze = "sz:3,3;st:0,0;end:2,0;mz:"
                        ++ "o0o0o0"
                        ++ "o0x x "
                        ++ "o0o0o0"
                        |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ notAHoleMaze).holes |> Expect.equal 0
            ]
        , describe "Squares"
            [ test "Squares: 2x2" <| \_ ->
                let
                    maze = "sz:2,2;st:0,0;end:1,1;mz:o0o0o0o0" |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ maze).squares |> Expect.equal 1
            , test "Squares: 2x3" <| \_ ->
                let
                    maze = "sz:3,2;st:0,0;end:2,1;mz:o0o0o0o0o0o0" |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ maze).squares |> Expect.equal 2
            , test "Squares: mixed height" <| \_ ->
                let
                    maze = "sz:2,2;st:0,0;end:1,1;mz:o0o0o0o1" |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ maze).squares |> Expect.equal 0
            ]
        , describe "Stairs"
            [ test "Stairs Proportion" <| \_ ->
                let
                    maze = "sz:2,1;st:0,0;end:1,0;mz:o0s1" |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ maze).stairsProportion |> Expect.within (Expect.Absolute 0.01) 0.5
            ]
        , describe "Bridges"
            [ test "Bridges Proportion" <| \_ ->
                let
                    maze = "sz:2,1;st:0,0;end:1,0;mz:o1l1" |> Codec.decode |> Maybe.withDefault M.emptyMaze
                in
                (analyze_ maze).bridgesProportion |> Expect.within (Expect.Absolute 0.01) (1 / 3)
            ]
        ]
