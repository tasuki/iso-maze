port module DrawThree exposing (drawScene, renderThreeJS, sceneData)

import Angle
import Decorations as D
import Html as H
import Html.Attributes as HA
import Json.Encode as E
import Maze as M
import MazeEdit as ME

port renderThreeJS : E.Value -> Cmd msg


type alias Model m =
    { m
        | azimuth : Angle.Angle
        , elevation : Angle.Angle
        , maze : M.Maze
        , player : M.Position
        , focus : M.Position
        , mode : ME.Mode
    }

type alias Box =
    { x : Float
    , y : Float
    , z : Float
    , sizeX : Float
    , sizeY : Float
    , sizeZ : Float
    , material : String
    , rotationZ : Float
    }


type alias Sphere =
    { x : Float
    , y : Float
    , z : Float
    , radius : Float
    , material : String
    }


drawScene : m -> H.Html msg
drawScene _ =
    H.div
        [ HA.id "three-container"
        , HA.style "width" "100%"
        , HA.style "height" "100vh"
        ]
        []


sceneData : Model m -> E.Value
sceneData model =
    let
        ( x, y, z ) = model.player
        pLightPos = playerPos ( x, y, z ) 5 model.maze
    in
    E.object
        [ ( "camera"
          , E.object
                [ ( "azimuth", E.float (Angle.inDegrees model.azimuth) )
                , ( "elevation", E.float (Angle.inDegrees model.elevation) )
                ]
          )
        , ( "playerLight"
          , E.object
                [ ( "x", E.float pLightPos.x )
                , ( "y", E.float pLightPos.y )
                , ( "z", E.float pLightPos.z )
                ]
          )
        , ( "boxes", E.list encodeBox (allBoxes model) )
        , ( "spheres", E.list encodeSphere (allSpheres model) )
        ]


allBoxes : Model m -> List Box
allBoxes model =
    List.concat
        [ List.concatMap drawBlock (M.toBlocks model.maze)
        , drawEnd (M.endPosition model.maze) (M.isAtEnd model.player model.maze)
        -- , List.concatMap drawRailing (D.getRailings model.maze)
        ]


allSpheres : Model m -> List Sphere
allSpheres model =
    List.concat
        [ drawPlayer model.player model.maze
        , drawFocus model.mode model.focus
        ]


encodeBox : Box -> E.Value
encodeBox b =
    E.object
        [ ( "x", E.float b.x )
        , ( "y", E.float b.y )
        , ( "z", E.float b.z )
        , ( "sizeX", E.float b.sizeX )
        , ( "sizeY", E.float b.sizeY )
        , ( "sizeZ", E.float b.sizeZ )
        , ( "material", E.string b.material )
        , ( "rotationZ", E.float b.rotationZ )
        ]


encodeSphere : Sphere -> E.Value
encodeSphere s =
    E.object
        [ ( "x", E.float s.x )
        , ( "y", E.float s.y )
        , ( "z", E.float s.z )
        , ( "radius", E.float s.radius )
        , ( "material", E.string s.material )
        ]


-- Drawing (Internal helpers)


playerPos : M.Position -> Float -> M.Maze -> { x : Float, y : Float, z : Float }
playerPos ( x, y, z ) zOffset maze =
    let
        zStairsFix = case M.get ( x, y ) maze of
            Just (M.Stairs _ _) -> -5
            _ -> 0
    in
    { x = toFloat x * 10
    , y = toFloat y * 10
    , z = toFloat z * 10 + zOffset + zStairsFix
    }


drawBase : String -> Float -> Float -> Float -> Box
drawBase material x y z =
    { x = x * 10
    , y = y * 10
    , z = z * 5 - 5
    , sizeX = 10
    , sizeY = 10
    , sizeZ = z * 10 + 10
    , material = material
    , rotationZ = 0
    }


drawBlock : M.Block -> List Box
drawBlock block =
    case block of
        M.Base ( x, y, z ) ->
            [ drawBase "base" (toFloat x) (toFloat y) (toFloat z) ]

        M.Bridge ( x, y, z ) ->
            [ { x = toFloat x * 10
              , y = toFloat y * 10
              , z = toFloat z * 10 + 0.5
              , sizeX = 10
              , sizeY = 10
              , sizeZ = 1
              , material = "bridge"
              , rotationZ = 0
              }
            , drawBase "base" (toFloat x) (toFloat y) (toFloat z - 1)
            ]

        M.Stairs ( x, y, z ) dir ->
            let
                fx = toFloat x
                fy = toFloat y
                fz = toFloat z

                stepBox cx cy cz sw sd sh =
                    { x = fx * 10 + cx
                    , y = fy * 10 + cy
                    , z = fz * 10 + cz
                    , sizeX = sw
                    , sizeY = sd
                    , sizeZ = sh
                    , material = "stairs"
                    , rotationZ = 0
                    }

                ( centerFun, dimsFun ) = case dir of
                    M.SE ->
                        ( \i -> ( 0, 4.5 - toFloat i, -5.0 - 0.5 * toFloat i )
                        , \i -> ( 10, 1, 10 - toFloat i )
                        )
                    M.SW ->
                        ( \i -> ( 4.5 - toFloat i, 0, -5.0 - 0.5 * toFloat i )
                        , \i -> ( 1, 10, 10 - toFloat i )
                        )
                    M.NE ->
                        ( \i -> ( 4.5 - toFloat i, 0, -9.5 + 0.5 * toFloat i )
                        , \i -> ( 1, 10, 1 + toFloat i )
                        )
                    M.NW ->
                        ( \i -> ( 0, 4.5 - toFloat i, -9.5 + 0.5 * toFloat i )
                        , \i -> ( 10, 1, 1 + toFloat i )
                        )

                oneBox i =
                    let
                        ( cx, cy, cz ) = centerFun i
                        ( sw, sd, sh ) = dimsFun i
                    in
                    stepBox cx cy cz sw sd sh
            in
            List.map oneBox (List.range 0 9) ++ [ drawBase "stairs" fx fy (fz - 1) ]


drawEnd : M.Position -> Bool -> List Box
drawEnd ( x, y, z ) isAtEnd =
    let
        zd =
            if isAtEnd then
                9.5

            else
                0

        hatPart rotation =
            { x = toFloat x * 10
            , y = toFloat y * 10
            , z = toFloat z * 10 + 1 + zd
            , sizeX = 1.6
            , sizeY = 1.6
            , sizeZ = 1.6
            , material = "goal"
            , rotationZ = rotation
            }
    in
    [ hatPart 0, hatPart 30, hatPart 60 ]


drawPlayer : M.Position -> M.Maze -> List Sphere
drawPlayer ( x, y, z ) maze =
    let
        playerSphere zOffset r =
            let
                p = playerPos ( x, y, z ) zOffset maze
            in
            { x = p.x, y = p.y, z = p.z, radius = r, material = "player" }
    in
    [ playerSphere 2.0 2.2
    , playerSphere 5.5 1.8
    , playerSphere 8.5 1.4
    ]


drawFocus : ME.Mode -> M.Position -> List Sphere
drawFocus mode ( x, y, z ) =
    case mode of
        ME.Running -> []
        ME.Editing ->
            let
                xmin = toFloat x * 10 - 5
                xmax = toFloat x * 10 + 5
                ymin = toFloat y * 10 - 5
                ymax = toFloat y * 10 + 5
                zmin = toFloat z * 10 - 10
                zmax = toFloat z * 10
                s xpos ypos zpos =
                    { x = xpos, y = ypos, z = zpos, radius = 0.5, material = "focus" }
            in
            [ s xmin ymin zmin
            , s xmax ymin zmin
            , s xmin ymax zmin
            , s xmax ymax zmin
            , s xmin ymin zmax
            , s xmax ymin zmax
            , s xmin ymax zmax
            , s xmax ymax zmax
            ]


drawRailing : ( M.Block, M.Direction ) -> List Box
drawRailing ( block, dir ) =
    let
        ( x, y, z ) = M.blockPosition block

        baseCoords =
            case block of
                M.Stairs _ _ ->
                    [ -4.5, -3.5, -2.5, -1.5, -0.5
                    ,  0.5,  1.5,  2.5,  3.5,  4.5 ]
                _ -> [ -4, -2, 0, 2, 4 ]

        zd xd yd =
            case block of
                M.Base _ -> 0.2
                M.Bridge _ -> 1.2
                M.Stairs _ stairsDir ->
                    case stairsDir of
                        M.SE -> yd - 4.3
                        M.SW -> xd - 4.3
                        M.NW -> -4.3 - yd
                        M.NE -> -4.3 - xd

        createRailing ( xd, yd ) =
            { x = toFloat x * 10 + xd
            , y = toFloat y * 10 + yd
            , z = toFloat z * 10 + zd xd yd
            , sizeX = 0.3
            , sizeY = 0.3
            , sizeZ = 0.5
            , material = "railing"
            , rotationZ = 0
            }

        centers : List ( Float, Float )
        centers = case dir of
            M.SE -> List.map (\c -> ( c, -4 )) baseCoords
            M.SW -> List.map (\c -> ( -4, c )) baseCoords
            M.NW -> List.map (\c -> ( c, 4 )) baseCoords
            M.NE -> List.map (\c -> ( 4, c )) baseCoords
    in
    List.map createRailing centers
