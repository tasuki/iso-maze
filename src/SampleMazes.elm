module SampleMazes exposing (..)

import Codec
import List.Nonempty as NE exposing (Nonempty)
import Maze as M

zeroedZigZag : Int -> List ( Int, Int )
zeroedZigZag tilesPerSide =
    let
        ( firstX, firstY ) =
            if modBy 2 tilesPerSide == 0 then
                ( -(ceiling <| toFloat tilesPerSide / 2)
                , ceiling <| toFloat tilesPerSide / 2
                )
            else
                ( -(ceiling <| toFloat tilesPerSide / 2)
                , (ceiling <| toFloat tilesPerSide / 2) - 1
                )

        next : Int -> Int -> ( Int, Int )
        next x y =
            if x == -y then ( x, y - 1 )
            else ( x + 1, y )

        build : Nonempty ( Int, Int ) -> Nonempty ( Int, Int )
        build acc =
            let ( x, y ) = NE.head acc in
            if x >= firstY && y <= firstX then acc
            else build <| NE.cons (next x y) acc
    in
    NE.singleton ( firstX, firstY ) |> build |> NE.toList

createZigZag : Int -> Int -> Int -> List M.Block
createZigZag tilesPerSide shiftBack height =
    zeroedZigZag tilesPerSide
        |> List.map (\( x, y ) -> M.Base ( x + shiftBack, y + shiftBack, height ))

setStartEnd : M.Pos2d -> M.Pos2d -> M.Maze -> M.Maze
setStartEnd start end maze =
    { maze | start = start, end = end }



-- Mazes

zigZagBlocks : List M.Block
zigZagBlocks =
    List.concat
        [ createZigZag 6 0 0
        , createZigZag 6 1 1
        , createZigZag 6 2 2
        , createZigZag 5 3 3
        , createZigZag 3 4 4
        ]

zigZag : M.Maze
zigZag = M.fromBlocks <| zigZagBlocks

roundabout : M.Maze
roundabout =
    setStartEnd ( 0, 0 ) ( 4, 4 ) <|
        M.fromBlocks <| zigZagBlocks ++
            [ M.createStairs -1  1 1 M.SE
            , M.createStairs  1 -1 1 M.SW
            , M.createStairs -1  3 2 M.SE
            , M.createStairs  3 -1 2 M.SW
            , M.createStairs  4  0 3 M.SE
            , M.createStairs  0  4 3 M.SW
            , M.createStairs  4  2 4 M.SE
            , M.createStairs  2  4 4 M.SW
            ]

fourStairs : M.Maze
fourStairs =
    setStartEnd ( -2, -2 ) ( 0, 0 ) <| M.fromBlocks
        [ M.createBase   -2 -2 0
        , M.createBase    0  0 2
        , M.createStairs  0  1 2 M.NW
        , M.createStairs  0  2 1 M.NW
        , M.createStairs  1  0 2 M.NE
        , M.createStairs  2  0 1 M.NE
        , M.createStairs  0 -1 2 M.SE
        , M.createStairs  0 -2 1 M.SE
        , M.createStairs -1  0 2 M.SW
        , M.createStairs -2  0 1 M.SW
        ]

maxMaze : M.Maze
maxMaze =
    M.mapAllCoords (\x y -> ( x, y, 0 ))
        |> List.filter M.isValidPosition
        |> List.map M.Base
        |> M.fromBlocks
        |> setStartEnd ( 0, 0 ) ( 8, 8 )

assymetric : M.Maze
assymetric =
    M.fromBlocks
        [ M.createBase 0 -1 0
        , M.createBase 0  0 0
        , M.createBase 0  1 0
        , M.createBase 0  2 0
        , M.createBase 0  3 0
        , M.createBase 1  3 0
        , M.createBase 1  4 0
        ]
        |> setStartEnd ( 0, -1 ) ( 1, 4 )


-- enter a new era of representing mazes

ziggurat = "sz:9,9;off:-3,-3;st:0,0;end:4,4;mz:"
    ++ "x x x x o0o0x x x "
    ++ "x x x o0o0o1o1x x "
    ++ "x x x o0z1o1z2o2o2"
    ++ "x o0o0o0o1o1o1s3o2"
    ++ "o0o0s1o1o1s2o3o3o3"
    ++ "o0o1o1o1z2o2o3s4o3"
    ++ "x o1s2o1o3o3o3s5o3"
    ++ "x x o2z3o3z4z5o5o3"
    ++ "x x o2o2o3o3o3o3o3"
        |> Codec.decode |> Maybe.withDefault M.emptyMaze

prototypeFirstMaze = "sz:22,23;off:-8,-8;st:0,14;end:7,-8;mz:"
    ++ "x x x x x x x x x x x x x o0o0o1z2o2x x x x "
    ++ "x x x x x x x x x x x x x s1o0x x S2x x x x "
    ++ "x x x x x x x x x x x o1o1o1o0x o1o1x x x x "
    ++ "x x x x x x x x x x x o1x x o0x o1x x x x x "
    ++ "x x x x x x x x x x o0l1o0z1l1Z1l1o0o0o0o0x "
    ++ "x x x x x x x x o0o0o0o1x x o0x S1x x x o0x "
    ++ "x x x x x x x x o0o1o1o1Z1o0o0x o0x o0z1l1o1"
    ++ "x x x x x x x o0o0o1x o1x o1o1x o0x o0x o0s2"
    ++ "x x x x x x o0o0o1o1x o1Z1l1o0o0o0x o0x s1o2"
    ++ "x x x x o0z1l1o1o1x x x x o1o2Z2o1o1l1o1o1o2"
    ++ "x x x x o0x o0x x x x x x o1o2x x x o0x o2o2"
    ++ "x x x x o0x s1x o0o0o0o1o1o1o2x o1o1s1x S2x "
    ++ "x x x o0o0x s2x s1o0o0o1o2o2o2x s2o1o1o1o1x "
    ++ "x x o0o0o1z2o2o2l2o2o2o1s3o2o2o2o2x x x x x "
    ++ "o0z1l1o1o1x x x o1x o2s2o3x x x x x x x x x "
    ++ "o0x o0x o1x x x s2x o2o2o3x x x x x x x x x "
    ++ "o0x s1x o1x o0o1l2z2o2o3o3x x x x x x x x x "
    ++ "o0o0l1o0o1x o0o1o2x x o3x x x x x x x x x x "
    ++ "x x o1o0l1o0o0o1o2o2z3o3x x x x x x x x x x "
    ++ "x x o1o1o1x o1o1o2x x x x x x x x x x x x x "
    ++ "x x x x x x s2o1o2x x x x x x x x x x x x x "
    ++ "x x x x x x o2o1s3x x x x x x x x x x x x x "
    ++ "x x x x x x o2z3o3x x x x x x x x x x x x x "
        |> Codec.decode |> Maybe.withDefault M.emptyMaze
