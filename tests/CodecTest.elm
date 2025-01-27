module CodecTest exposing (..)

import Array
import Codec exposing (..)
import Expect
import Maze exposing (..)
import SampleMazes as SM
import Test exposing (..)


determineLimitsTest =
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
                { xSize = 9, ySize = 9, xOffset = -3, yOffset = -3, maze =
                    [  EmptyBlock,  EmptyBlock,       EmptyBlock,       EmptyBlock,       EmptyBlock,      BaseBlock 0,      BaseBlock 0,       EmptyBlock,  EmptyBlock
                    ,  EmptyBlock,  EmptyBlock,       EmptyBlock,       EmptyBlock,      BaseBlock 0,      BaseBlock 0,      BaseBlock 1,      BaseBlock 1,  EmptyBlock
                    ,  EmptyBlock,  EmptyBlock,       EmptyBlock,      BaseBlock 0, StairsBlock 1 SW,      BaseBlock 1, StairsBlock 2 SW,      BaseBlock 2, BaseBlock 2
                    ,  EmptyBlock,  EmptyBlock,      BaseBlock 0,      BaseBlock 0,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2, StairsBlock 3 SE, BaseBlock 3
                    ,  EmptyBlock, BaseBlock 0, StairsBlock 1 SE,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3, BaseBlock 3
                    , BaseBlock 0, BaseBlock 0,      BaseBlock 1,      BaseBlock 1,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3, StairsBlock 4 SE, BaseBlock 4
                    , BaseBlock 0, BaseBlock 1, StairsBlock 2 SE,      BaseBlock 2,      BaseBlock 2,      BaseBlock 3,      BaseBlock 3,      BaseBlock 4, BaseBlock 4
                    ,  EmptyBlock, BaseBlock 1,      BaseBlock 2, StairsBlock 3 SW,      BaseBlock 3, StairsBlock 4 SW,      BaseBlock 4,      BaseBlock 4,  EmptyBlock
                    ,  EmptyBlock, EmptyBlock,       BaseBlock 2,      BaseBlock 3,      BaseBlock 3,      BaseBlock 4,      BaseBlock 4,       EmptyBlock,  EmptyBlock
                    ]
                }
                (cutout SM.roundabout)
        , test "Cuts out assymetric maze" <|
            \_ -> Expect.equal
                { xSize = 2, ySize = 6, xOffset = 0, yOffset = -1, maze =
                    [ BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, EmptyBlock
                    , BaseBlock 0, BaseBlock 0
                    , EmptyBlock , BaseBlock 0
                    ]
                }
                (cutout SM.assymetric)
        ]
