module CodecTest exposing (..)

import Array
import Codec exposing (..)
import Expect
import Maze exposing (..)
import SampleMazes as SM
import Test exposing (..)


limitsTest =
    describe "Limit finder"
        [ test "Finds limits for roundabout" <|
            \_ -> Expect.equal
                { minX = -3, maxX = 5, minY = -3, maxY = 5 }
                (mazeLimits SM.roundabout)
        , test "Finds limits for assymetric" <|
            \_ -> Expect.equal
                { minX = 0, maxX = 1, minY = -1, maxY = 4 }
                (mazeLimits SM.assymetric)
        ]

mazeLimits : Maze -> Limits
mazeLimits = getLimits

encodeTest =
    describe "Encode"
        [ test "Encodes assymetric (with defaults omitted)" <|
            \_ -> Expect.equal
                (removeSpaces ("sz:2,6;st:0,0;end:1,5"
                    ++ ";mz:"
                    ++ "x o0"
                    ++ "o0o0"
                    ++ "o0x "
                    ++ "o0x "
                    ++ "o0x "
                    ++ "o0x "
                ))
                (encode SM.assymetric)
        , test "Encodes the roundabout (with defaults omitted)" <|
            \_ -> Expect.equal
                (removeSpaces ("sz:9,9;st:3,3;end:7,7"
                    ++ ";mz:"
                    ++ "x x o2o3o3o4o4x x "
                    ++ "x o1o2z3o3z4o4o4x "
                    ++ "o0o1s2o2o2o3o3o4o4"
                    ++ "o0o0o1o1o2o2o3s4o4"
                    ++ "x o0s1o1o1o2o2o3o3"
                    ++ "x x o0o0o1o1o2s3o3"
                    ++ "x x x o0z1o1z2o2o2"
                    ++ "x x x x o0o0o1o1x "
                    ++ "x x x x x o0o0x x "
                ))
                (encode SM.roundabout)
        , test "Encodes with custom config" <|
            \_ ->
                let
                    customConfig =
                        { left = { color = "123", intensity = 50 }
                        , right = defaultConfig.right
                        , above = defaultConfig.above
                        , bg = "abc"
                        }
                    oldMaze = SM.assymetric
                    maze = { oldMaze | config = customConfig }
                in
                Expect.equal
                    (removeSpaces (
                        "bg:abc;left:123,50;"
                        ++ "sz:2,6;st:0,0;end:1,5;mz:"
                        ++ "x o0"
                        ++ "o0o0"
                        ++ "o0x "
                        ++ "o0x "
                        ++ "o0x "
                        ++ "o0x "
                    ))
                    (encode maze)
        ]

decodeTest =
    describe "Decode"
        [ test "Decodes assymetric" <|
            \_ ->
                let
                    decoded = decode <| encode SM.assymetric
                in
                case decoded of
                    Just m ->
                        Expect.equal
                            (toBlocks SM.assymetric |> List.length)
                            (toBlocks m |> List.length)
                    Nothing -> Expect.fail "Failed to decode"
        , test "Decodes roundabout" <|
            \_ ->
                let
                    decoded = decode <| encode SM.roundabout
                in
                case decoded of
                    Just m ->
                        Expect.equal
                            (toBlocks SM.roundabout |> List.length)
                            (toBlocks m |> List.length)
                    Nothing -> Expect.fail "Failed to decode"
        ]
