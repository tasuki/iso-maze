module DrawThreeTest exposing (..)

import DrawThree as DT
import Expect
import Test exposing (..)

numStripesCases =
    [ (  0, ( 0, 0 ) )
    , (  4, ( 0, 0 ) )
    , (  5, ( 0, 1 ) )
    , (  9, ( 0, 1 ) )
    , ( 10, ( 1, 1 ) )
    , ( 14, ( 1, 1 ) )
    , ( 15, ( 1, 2 ) )
    , ( 19, ( 1, 2 ) )
    , ( 20, ( 2, 2 ) )
    , ( 24, ( 2, 2 ) )
    , ( 25, ( 2, 3 ) )
    , ( 29, ( 2, 3 ) )
    , ( 30, ( 3, 3 ) )
    ]

stripesMatch : ( Int, ( Int, Int ) ) -> Test
stripesMatch testCase =
    case testCase of
        ( input, expected ) ->
            test ("gets stripes for " ++ String.fromInt input) <|
                \_ -> Expect.equal expected (DT.numStripes input)

numStripesTest =
    numStripesCases |> List.map stripesMatch |> describe "DrawTree.numStripes"
