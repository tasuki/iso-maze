module DrawThreeTest exposing (..)

import DrawThree as DT
import Expect
import Test exposing (..)

numStripesCases =
    [ (  0, ( 0, 0 ) )
    , (  9, ( 0, 0 ) )
    , ( 10, ( 0, 1 ) )
    , ( 19, ( 0, 1 ) )
    , ( 20, ( 1, 1 ) )
    , ( 29, ( 1, 1 ) )
    , ( 30, ( 1, 2 ) )
    , ( 39, ( 1, 2 ) )
    , ( 40, ( 2, 2 ) )
    , ( 49, ( 2, 2 ) )
    , ( 50, ( 2, 3 ) )
    ]

stripesMatch : ( Int, ( Int, Int ) ) -> Test
stripesMatch testCase =
    case testCase of
        ( input, expected ) ->
            test ("gets stripes for " ++ String.fromInt input) <|
                \_ -> Expect.equal expected (DT.numStripes input)

numStripesTest =
    numStripesCases |> List.map stripesMatch |> describe "DrawTree.numStripes"
