port module DrawThree exposing (initialAzimuth, initialElevation, renderThreeJS, sceneData)

import Angle
import Json.Encode as E
import Maze as M
import MazeEdit as ME


port renderThreeJS : E.Value -> Cmd msg


initialAzimuth = -135
initialElevation = 45


type alias Vec3 =
    { x : Float, y : Float, z : Float }


type alias Model m =
    { m
        | azimuth : Angle.Angle
        , elevation : Angle.Angle
        , maze : M.Maze
        , playerSpheres : ( Vec3, Vec3, Vec3 )
        , focus : M.Position
        , mode : ME.Mode
        , widthPx : Int
        , heightPx : Int
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


sceneData : Model m -> E.Value
sceneData model =
    let
        ( p, _, _ ) = model.playerSpheres
        pLightPos = { x = p.x, y = p.y, z = p.z + 3.0 }
        config = computeCameraConfig model
    in
    E.object
        [ ( "mode", E.string (if model.mode == ME.Running then "running" else "editing") )
        , ( "camera"
          , E.object
                [ ( "viewSize", E.float config.viewSize )
                , ( "focalPoint", encodeVec3 config.focalPoint )
                , ( "position", encodeVec3 config.cameraPosition )
                ]
          )
        , ( "playerLight", encodeVec3 pLightPos )
        , ( "boxes", E.list encodeBox (allBoxes model) )
        , ( "spheres", E.list encodeSphere (allSpheres model) )
        ]

allBoxes : Model m -> List Box
allBoxes model =
    let
        ( p, _, _ ) = model.playerSpheres
        discretePlayer = ( round (p.x / 10), round (p.y / 10), round (p.z / 10) )
    in
    List.concat
        [ List.concatMap drawBlock (M.toBlocks model.maze)
        , drawEnd (M.endPosition model.maze) (M.isAtEnd discretePlayer model.maze)
        ]

allSpheres : Model m -> List Sphere
allSpheres model =
    List.concat
        [ drawPlayer model.playerSpheres
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


encodeVec3 : Vec3 -> E.Value
encodeVec3 v =
    E.object
        [ ( "x", E.float v.x )
        , ( "y", E.float v.y )
        , ( "z", E.float v.z )
        ]


-- Drawing (Internal helpers)

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
            if isAtEnd then 9.5
            else 0

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


drawPlayer : ( Vec3, Vec3, Vec3 ) -> List Sphere
drawPlayer ( p1, p2, p3 ) =
    [ { x = p1.x, y = p1.y, z = p1.z, radius = 2.2, material = "player" }
    , { x = p2.x, y = p2.y, z = p2.z, radius = 1.8, material = "player" }
    , { x = p3.x, y = p3.y, z = p3.z, radius = 1.4, material = "player" }
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


type alias CameraConfig =
    { viewSize : Float
    , focalPoint : Vec3
    , cameraPosition : Vec3
    }


computeCameraConfig : Model m -> CameraConfig
computeCameraConfig model =
    let
        blockToPoints block =
            let
                ( bx, by, bz ) = M.blockPosition block
                fx = toFloat bx * 10
                fy = toFloat by * 10
                fz = toFloat bz * 10
            in
            -- Floor is at -10, head space up to fz + 10
            [ ( fx - 5, fy - 5, -10 )
            , ( fx + 5, fy - 5, -10 )
            , ( fx - 5, fy + 5, -10 )
            , ( fx + 5, fy + 5, -10 )
            , ( fx - 5, fy - 5, fz + 10 )
            , ( fx + 5, fy - 5, fz + 10 )
            , ( fx - 5, fy + 5, fz + 10 )
            , ( fx + 5, fy + 5, fz + 10 )
            ]

        points = case List.concatMap blockToPoints <| M.toBlocks model.maze of
            [] -> [ ( 0, 0, 0 ) ]
            ps -> ps

        a = Angle.inRadians model.azimuth
        e = Angle.inRadians model.elevation

        -- Basis vectors (matching Three.js with camera.up = 0,0,1)
        r = { x = -(sin a), y = cos a, z = 0 }
        u = { x = -(cos a * sin e), y = -(sin a * sin e), z = cos e }

        project ( px, py, pz ) =
            ( px * r.x + py * r.y + pz * r.z
            , px * u.x + py * u.y + pz * u.z
            )
        projected = List.map project points
        rs = List.map Tuple.first projected
        us = List.map Tuple.second projected

        minR = List.minimum rs |> Maybe.withDefault 0
        maxR = List.maximum rs |> Maybe.withDefault 0
        minU = List.minimum us |> Maybe.withDefault 0
        maxU = List.maximum us |> Maybe.withDefault 0

        w = maxR - minR
        h = maxU - minU
        midR = (minR + maxR) / 2
        midU = (minU + maxU) / 2

        -- Mapping back to 3D focal point
        focal3d =
            { x = midR * r.x + midU * u.x
            , y = midR * r.y + midU * u.y
            , z = midR * r.z + midU * u.z
            }

        distance =
            300.0

        cameraPosition3d =
            { x = focal3d.x + distance * cos a * cos e
            , y = focal3d.y + distance * sin a * cos e
            , z = focal3d.z + distance * sin e
            }

        widthPx =
            model.widthPx |> toFloat
        heightPx = model.heightPx |> toFloat

        aspect =
            if heightPx > 0 then widthPx / heightPx
            else 1.0

        -- Apply padding (0.9 factor means 5% on each side)
        paddingFactor = 0.9
        viewSize = max h (w / aspect) / paddingFactor
    in
    { viewSize = viewSize * 0.01
    , focalPoint =
        { x = focal3d.x * 0.01
        , y = focal3d.y * 0.01
        , z = focal3d.z * 0.01
        }
    , cameraPosition =
        { x = cameraPosition3d.x * 0.01
        , y = cameraPosition3d.y * 0.01
        , z = cameraPosition3d.z * 0.01
        }
    }
