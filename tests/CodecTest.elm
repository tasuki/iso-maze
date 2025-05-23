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

cutoutTest =
    describe "Cutout"
        [ test "Cuts out the roundabout" <|
            \_ -> Expect.equal
                { xSize = 9, ySize = 9, xOffset = -3, yOffset = -3, start = (0, 0), end = (4, 4), maze =
                    [  EmptyBlock, EmptyBlock,       BaseBlock 2,      BaseBlock 3,      BaseBlock 3,      BaseBlock 4,      BaseBlock 4,       EmptyBlock,  EmptyBlock
                    ,  EmptyBlock, BaseBlock 1,      BaseBlock 2, StairsBlock 3 SW,      BaseBlock 3, StairsBlock 4 SW,      BaseBlock 4,      BaseBlock 4,  EmptyBlock
                    , BaseBlock 0, BaseBlock 1, StairsBlock 2 SE,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3,      BaseBlock 3,      BaseBlock 4, BaseBlock 4
                    , BaseBlock 0, BaseBlock 0,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3, StairsBlock 4 SE, BaseBlock 4
                    ,  EmptyBlock, BaseBlock 0, StairsBlock 1 SE,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3, BaseBlock 3
                    ,  EmptyBlock,  EmptyBlock,      BaseBlock 0,      BaseBlock 0,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2, StairsBlock 3 SE, BaseBlock 3
                    ,  EmptyBlock,  EmptyBlock,       EmptyBlock,      BaseBlock 0, StairsBlock 1 SW,      BaseBlock 1, StairsBlock 2 SW,      BaseBlock 2, BaseBlock 2
                    ,  EmptyBlock,  EmptyBlock,       EmptyBlock,       EmptyBlock,      BaseBlock 0,      BaseBlock 0,      BaseBlock 1,      BaseBlock 1,  EmptyBlock
                    ,  EmptyBlock,  EmptyBlock,       EmptyBlock,       EmptyBlock,       EmptyBlock,      BaseBlock 0,      BaseBlock 0,       EmptyBlock,  EmptyBlock
                    ]
                }
                (cutout SM.roundabout)
        , test "Cuts out assymetric" <|
            \_ -> Expect.equal
                { xSize = 2, ySize = 6, xOffset = 0, yOffset = -1, start = (0, -1), end = (1, 4), maze =
                    [ EmptyBlock , BaseBlock 0
                    , BaseBlock 0, BaseBlock 0
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    ]
                }
                (cutout SM.assymetric)
        ]

insertCutoutTest =
    describe "Insert cutout"
        [ test "Puts the cut out roundabout back into full size maze" <|
            \_ -> Expect.equal
                SM.roundabout
                (insertCutout <| cutout SM.roundabout)
        , test "Puts the cut out assymetric back into full size maze" <|
            \_ -> Expect.equal
                SM.assymetric
                (insertCutout <| cutout SM.assymetric)
        ]

encodeTest =
    describe "Encode"
        [ test "Encodes assymetric" <|
            \_ -> Expect.equal
                (removeSpaces ("sz:2,6;off:0,-1;st:0,-1;end:1,4;mz:"
                    ++ "x o0"
                    ++ "o0o0"
                    ++ "o0x "
                    ++ "o0x "
                    ++ "o0x "
                    ++ "o0x "
                ))
                (encode SM.assymetric)
        , test "Encodes the roundabout" <|
            \_ -> Expect.equal
                (removeSpaces ("sz:9,9;off:-3,-3;st:0,0;end:4,4;mz:"
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
        ]

decodeTest =
    describe "Decode"
        [ test "Decodes assymetric" <|
            \_ -> Expect.equal
                (Just SM.assymetric)
                (decode <| encode SM.assymetric)
        , test "Decodes roundabout" <|
            \_ -> Expect.equal
                (Just SM.roundabout)
                (decode <| encode SM.roundabout)
        ]
