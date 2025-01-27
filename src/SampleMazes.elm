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
    M.fromBlocks
        [ M.createBase    0  0 1
        , M.createStairs  0  1 1 M.NW
        , M.createStairs  0  2 0 M.NW
        , M.createStairs  1  0 1 M.NE
        , M.createStairs  2  0 0 M.NE
        , M.createStairs  0 -1 1 M.SE
        , M.createStairs  0 -2 0 M.SE
        , M.createStairs -1  0 1 M.SW
        , M.createStairs -2  0 0 M.SW
        ]

maxMaze : M.Maze
maxMaze =
    M.mapAllCoords (\x y -> ( x, y, 0 ))
        |> List.filter M.isValidPosition
        |> List.map M.Base
        |> M.fromBlocks

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


-- enter a new era of representing mazes

ziggurat = "sz:9,9;off:-3,-3;mz:"
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
