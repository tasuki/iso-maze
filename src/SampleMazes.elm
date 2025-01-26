module SampleMazes exposing (..)

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
            if x == -y then
                ( x, y - 1 )

            else
                ( x + 1, y )

        build : Nonempty ( Int, Int ) -> Nonempty ( Int, Int )
        build acc =
            let
                ( x, y ) =
                    NE.head acc
            in
            if x >= firstY && y <= firstX then
                acc

            else
                build <| NE.cons (next x y) acc
    in
    NE.singleton ( firstX, firstY )
        |> build
        |> NE.toList


createZigZag : Int -> Int -> Int -> List M.Block
createZigZag tilesPerSide shiftBack height =
    zeroedZigZag tilesPerSide
        |> List.map (\( x, y ) -> M.Base ( x + shiftBack, y + shiftBack, height ))



-- Mazes


zigZag : M.Maze
zigZag =
    List.concat
        [ createZigZag 6 0 0
        , createZigZag 6 1 1
        , createZigZag 6 2 2
        , createZigZag 5 3 3
        , createZigZag 3 4 4
        ]


roundabout : M.Maze
roundabout =
    zigZag
        ++ [ M.createStairs -1 1 1 M.SE
           , M.createStairs 1 -1 1 M.SW
           , M.createStairs -1 3 2 M.SE
           , M.createStairs 3 -1 2 M.SW
           , M.createStairs 4 0 3 M.SE
           , M.createStairs 0 4 3 M.SW
           , M.createStairs 4 2 4 M.SE
           , M.createStairs 2 4 4 M.SW
           ]


fourStairs : M.Maze
fourStairs =
    [ M.createBase 0 0 1
    , M.createStairs 0 1 1 M.NW
    , M.createStairs 0 2 0 M.NW
    , M.createStairs 1 0 1 M.NE
    , M.createStairs 2 0 0 M.NE
    , M.createStairs 0 -1 1 M.SE
    , M.createStairs 0 -2 0 M.SE
    , M.createStairs -1 0 1 M.SW
    , M.createStairs -2 0 0 M.SW
    ]


maxMaze : M.Maze
maxMaze =
    List.concatMap (\x -> List.map (\y -> ( x, y, 0 )) (List.range -10 15)) (List.range -10 15)
        |> List.filter M.isValidPosition
        |> List.map M.Base
