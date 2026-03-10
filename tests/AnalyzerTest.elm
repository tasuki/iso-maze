module AnalyzerTest exposing (..)

import Analyzer exposing (analyze)
import Codec
import Expect
import Maze exposing (emptyMaze)
import Test exposing (..)


simplePath = "sz:3,1;st:0,0;end:2,0;mz:o0o0o0"
        |> Codec.decode |> Maybe.withDefault emptyMaze

loopMaze = "sz:3,3;st:0,0;end:2,0;mz:"
    ++ "o0o0o0"
    ++ "o0x o0"
    ++ "o0o0o0"
        |> Codec.decode |> Maybe.withDefault emptyMaze

isolatedMaze = "sz:3,3;st:0,0;end:2,2;mz:"
    ++ "x x o0"
    ++ "x x x "
    ++ "x o0x "
        |> Codec.decode |> Maybe.withDefault emptyMaze

bridgeMaze = "sz:3,1;st:0,0;end:2,0;mz:o1l1o1"
        |> Codec.decode |> Maybe.withDefault emptyMaze

stairsMaze = "sz:2,1;st:0,0;end:1,0;mz:o0s1"
        |> Codec.decode |> Maybe.withDefault emptyMaze


suite : Test
suite =
    describe "Analyzer Test"
        [ describe "Simple Path"
            [ test "Reachable" <| \_ -> (analyze simplePath).reachable |> Expect.equal True
            , test "Total Cells" <| \_ -> (analyze simplePath).totalCells |> Expect.equal 3
            , test "Shortest Path" <| \_ -> (analyze simplePath).shortestPathLength |> Expect.equal (Just 2)
            , test "Unreachable" <| \_ -> (analyze simplePath).unreachable |> Expect.equal 0
            , test "Loop Count" <| \_ -> (analyze simplePath).loopCount |> Expect.equal 0
            , test "River Factor" <| \_ -> (analyze simplePath).riverFactor |> Expect.within (Expect.Absolute 0.01) (3 / 2) -- 3 cells / (2 dead ends + 0 junctions)
            ]
        , describe "Loop Maze"
            [ test "Reachable" <| \_ -> (analyze loopMaze).reachable |> Expect.equal True
            , test "Total Cells" <| \_ -> (analyze loopMaze).totalCells |> Expect.equal 8
            , test "Loop Count" <| \_ -> (analyze loopMaze).loopCount |> Expect.equal 1
            ]
        , describe "Isolated Maze"
            [ test "Not Reachable" <| \_ -> (analyze isolatedMaze).reachable |> Expect.equal False
            , test "Total Cells" <| \_ -> (analyze isolatedMaze).totalCells |> Expect.equal 2
            , test "Unreachable" <| \_ -> (analyze isolatedMaze).unreachable |> Expect.equal 1
            , test "Shortest Path" <| \_ -> (analyze isolatedMaze).shortestPathLength |> Expect.equal Nothing
            ]
        , describe "Bridge Maze"
            [ test "Reachable" <| \_ -> (analyze bridgeMaze).reachable |> Expect.equal True
            , test "Total Cells" <| \_ -> (analyze bridgeMaze).totalCells |> Expect.equal 4
            , test "Unreachable" <| \_ -> (analyze bridgeMaze).unreachable |> Expect.equal 1
            , test "Path through bridge" <| \_ -> (analyze bridgeMaze).shortestPathLength |> Expect.equal (Just 2)
            ]
        ]
