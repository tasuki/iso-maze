module DocumentDecoders exposing (DocumentCoords, decodePrimary)

import Json.Decode as D


type alias DocumentCoords =
    { x : Float, y : Float }


decodePrimary : (DocumentCoords -> msg) -> D.Decoder msg
decodePrimary toMsg =
    D.field "isPrimary" D.bool
        |> D.andThen
            (\isPrimary ->
                if isPrimary then
                    D.map2 DocumentCoords
                        (D.field "pageX" D.float)
                        (D.field "pageY" D.float)
                        |> D.map toMsg

                else
                    D.fail "Not primary pointer"
            )
