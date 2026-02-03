port module DrawThree exposing (drawScene, renderThreeJS, sceneData)

import Angle
import Decorations as D
import Html as H
import Html.Attributes as HA
import Json.Encode as E
import Maze as M
import MazeEdit as ME
import Quantity


port renderThreeJS : E.Value -> Cmd msg


drawScene : { a | azimuth : Angle.Angle, elevation : Angle.Angle, maze : M.Maze, player : M.Position, focus : M.Position } -> H.Html msg
drawScene model =
    H.div
        [ HA.id "three-canvas"
        , HA.style "width" "100%"
        , HA.style "height" "100vh"
        ]
        []


sceneData : { a | azimuth : Angle.Angle, elevation : Angle.Angle, maze : M.Maze, player : M.Position, focus : M.Position, mode : ME.Mode } -> E.Value
sceneData model =
    E.object
        [ ( "camera"
          , E.object
                [ ( "azimuth", E.float (Angle.inDegrees model.azimuth) )
                , ( "elevation", E.float (Angle.inDegrees model.elevation) )
                ]
          )
        , ( "player", encodePosition model.player )
        , ( "focus", encodePosition model.focus )
        , ( "blocks", E.list encodeBlock (M.toBlocks model.maze) )
        , ( "railings", E.list encodeRailing (D.getRailings model.maze) )
        , ( "goal", encodePosition (M.endPosition model.maze) )
        , ( "mode", encodeMode model.mode )
        ]


encodeMode : ME.Mode -> E.Value
encodeMode mode =
    case mode of
        ME.Running ->
            E.string "running"

        ME.Editing ->
            E.string "editing"


encodePosition : ( Int, Int, Int ) -> E.Value
encodePosition ( x, y, z ) =
    E.object
        [ ( "x", E.float (toFloat x) )
        , ( "y", E.float (toFloat y) )
        , ( "z", E.float (toFloat z) )
        ]


encodeBlock : M.Block -> E.Value
encodeBlock b =
    case b of
        M.Base ( x, y, z ) ->
            E.object
                [ ( "type", E.string "base" )
                , ( "x", E.int x )
                , ( "y", E.int y )
                , ( "z", E.int z )
                ]

        M.Bridge ( x, y, z ) ->
            E.object
                [ ( "type", E.string "bridge" )
                , ( "x", E.int x )
                , ( "y", E.int y )
                , ( "z", E.int z )
                ]

        M.Stairs ( x, y, z ) dir ->
            E.object
                [ ( "type", E.string "stairs" )
                , ( "x", E.int x )
                , ( "y", E.int y )
                , ( "z", E.int z )
                , ( "direction", E.string (directionToString dir) )
                ]


directionToString : M.Direction -> String
directionToString d =
    case d of
        M.NE ->
            "NE"

        M.NW ->
            "NW"

        M.SE ->
            "SE"

        M.SW ->
            "SW"


encodeRailing : ( M.Block, M.Direction ) -> E.Value
encodeRailing ( block, dir ) =
    let
        ( x, y, z ) =
            M.blockPosition block
    in
    E.object
        [ ( "x", E.int x )
        , ( "y", E.int y )
        , ( "z", E.int z )
        , ( "direction", E.string (directionToString dir) )
        , ( "blockType", E.string (blockTypeToString block) )
        , ( "blockDirection", encodeBlockDirection block )
        ]


encodeBlockDirection : M.Block -> E.Value
encodeBlockDirection b =
    case b of
        M.Stairs _ dir ->
            E.string (directionToString dir)

        _ ->
            E.null


blockTypeToString : M.Block -> String
blockTypeToString b =
    case b of
        M.Base _ ->
            "base"

        M.Bridge _ ->
            "bridge"

        M.Stairs _ _ ->
            "stairs"
