module CodecTest exposing (..)

import Array
import Codec exposing (..)
import Expect
import Maze as M
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
