port module DrawThree exposing (drawScene)

import Json.Encode as E
import Html as H
import Html.Attributes as HA

port setStorage : E.Value -> Cmd msg

drawScene model =
    H.div [ HA.id "three-container" ] []
