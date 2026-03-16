port module DrawThree exposing (initialAzimuth, initialElevation, renderThreeJS, sceneData)

import Analyzer
import Angle
import Animate
import Json.Encode as E
import Maze as M
import MazeEdit as ME
import Set


port renderThreeJS : E.Value -> Cmd msg


initialAzimuth = -135
initialElevation = 45


type alias Vec3 =
    { x : Float, y : Float, z : Float }


type alias Model =
    { azimuth : Angle.Angle
    , elevation : Angle.Angle
    , maze : M.Maze
    , playerState : M.PlayerState
    , playerSpheres : ( Vec3, Vec3, Vec3 )
    , animatorTimer : Float
    , animatorInitialFall : Bool
    , focus : M.Position
    , mode : ME.Mode
    , widthPx : Int
    , heightPx : Int
    , staticUpdate : Bool
    , performance : String
    , analysis : Maybe Analyzer.Analysis
    }


type alias Box =
    { x : Float
    , y : Float
    , z : Float
    , sizeX : Float
    , sizeY : Float
    , sizeZ : Float
    , material : String
    , rotationX : Float
    , rotationY : Float
    , rotationZ : Float
    }

type alias Sphere =
    { x : Float
    , y : Float
    , z : Float
    , radius : Float
    , material : String
    }

type alias Plane =
    { x : Float
    , y : Float
    , z : Float
    , sizeX : Float
    , sizeY : Float
    , material : String
    , rotationX : Float
    , rotationY : Float
    , rotationZ : Float
    }

type Renderable
    = BoxRenderable Box
    | SphereRenderable Sphere
    | PlaneRenderable Plane


sceneData : Model -> E.Value
sceneData model =
    let
        config = computeCameraConfig model
        mazeConfig = model.maze.config

        limits = M.getLimits model.maze
        maxZ = M.toBlocks model.maze
            |> List.map (\b -> let ( _, _, z ) = M.blockPosition b in z)
            |> List.maximum |> Maybe.withDefault 0 |> toFloat

        leftPos = { x = toFloat limits.minX * 10 - 200, y = (toFloat (limits.minY + limits.maxY) / 2) * 10, z = 300 }
        rightPos = { x = (toFloat (limits.minX + limits.maxX) / 2) * 10, y = toFloat limits.minY * 10 - 200, z = 300 }
        abovePos = { x = toFloat limits.maxX * 10 + 200, y = toFloat limits.maxY * 10 + 200, z = maxZ * 10 + 600 }

        encodeLight l pos =
            E.object
                [ ( "color", E.string l.color )
                , ( "intensity", E.float l.intensity )
                , ( "position", encodeVec3 pos )
                ]

        common =
            [ ( "mode", E.string (if model.mode == ME.Running then "running" else "editing") )
            , ( "performance", E.string model.performance )
            , ( "camera"
              , E.object
                    [ ( "viewSize", E.float config.viewSize )
                    , ( "focalPoint", encodeVec3 config.focalPoint )
                    , ( "position", encodeVec3 config.cameraPosition )
                    ]
              )
            , ( "dynamic", E.list encodeRenderable (dynamicRenderables model) )
            , ( "staticUpdate", E.bool model.staticUpdate )
            ]
    in
    if model.staticUpdate then
        E.object
            ( ( "static", E.list encodeRenderable (staticRenderables model) ) ::
            ( "config", E.object
                [ ( "left", encodeLight mazeConfig.left leftPos )
                , ( "right", encodeLight mazeConfig.right rightPos )
                , ( "above", encodeLight mazeConfig.above abovePos )
                , ( "bg", E.string mazeConfig.bg )
                ]
            ) :: common)
    else
        E.object common

staticRenderables : Model -> List Renderable
staticRenderables model =
    List.concatMap drawBlock (M.toBlocks model.maze)
        ++ drawDebugSpheres model.analysis


drawDebugSpheres : Maybe Analyzer.Analysis -> List Renderable
drawDebugSpheres maybeAnalysis =
    case maybeAnalysis of
        Nothing ->
            []

        Just a ->
            let
                unreachableSpheres =
                    Set.toList a.unreachable
                        |> List.map
                            (\( x, y, z ) ->
                                SphereRenderable
                                    { x = toFloat x * 10
                                    , y = toFloat y * 10
                                    , z = toFloat z * 10 + 2.0
                                    , radius = 2.0
                                    , material = "debugUnreachable"
                                    }
                            )

                occludingSpheres =
                    Set.toList a.occluding
                        |> List.map
                            (\( x, y, z ) ->
                                SphereRenderable
                                    { x = toFloat x * 10
                                    , y = toFloat y * 10
                                    , z = toFloat z * 10 + 3.0
                                    , radius = 3.0
                                    , material = "debugOccluding"
                                    }
                            )

                hangingSpheres =
                    Set.toList a.hanging
                        |> List.map
                            (\( x, y, z ) ->
                                SphereRenderable
                                    { x = toFloat x * 10
                                    , y = toFloat y * 10
                                    , z = toFloat z * 10 + 3.0
                                    , radius = 3.0
                                    , material = "debugHanging"
                                    }
                            )
            in
            unreachableSpheres ++ occludingSpheres ++ hangingSpheres

dynamicRenderables : Model -> List Renderable
dynamicRenderables model =
    List.concat
        [ drawHalo model.maze model.playerState
        , List.map SphereRenderable (drawPlayer model.playerSpheres)
        , drawFocus model.mode model.focus
        , List.map BoxRenderable (drawEnd model.maze model.playerState model.playerSpheres model.animatorTimer model.animatorInitialFall)
        ]

encodeRenderable : Renderable -> E.Value
encodeRenderable r =
    case r of
        BoxRenderable b ->
            E.object
                [ ( "type", E.string "box" )
                , ( "x", E.float b.x )
                , ( "y", E.float b.y )
                , ( "z", E.float b.z )
                , ( "sizeX", E.float b.sizeX )
                , ( "sizeY", E.float b.sizeY )
                , ( "sizeZ", E.float b.sizeZ )
                , ( "material", E.string b.material )
                , ( "rotationX", E.float b.rotationX )
                , ( "rotationY", E.float b.rotationY )
                , ( "rotationZ", E.float b.rotationZ )
                ]

        SphereRenderable s ->
            E.object
                [ ( "type", E.string "sphere" )
                , ( "x", E.float s.x )
                , ( "y", E.float s.y )
                , ( "z", E.float s.z )
                , ( "radius", E.float s.radius )
                , ( "material", E.string s.material )
                ]

        PlaneRenderable p ->
            E.object
                [ ( "type", E.string "plane" )
                , ( "x", E.float p.x )
                , ( "y", E.float p.y )
                , ( "z", E.float p.z )
                , ( "sizeX", E.float p.sizeX )
                , ( "sizeY", E.float p.sizeY )
                , ( "material", E.string p.material )
                , ( "rotationX", E.float p.rotationX )
                , ( "rotationY", E.float p.rotationY )
                , ( "rotationZ", E.float p.rotationZ )
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
    let bottom = 7 in
    { x = x * 10
    , y = y * 10
    , z = z * 5 - bottom
    , sizeX = 10
    , sizeY = 10
    , sizeZ = z * 10 + 2 * bottom
    , material = material
    , rotationX = 0
    , rotationY = 0
    , rotationZ = 0
    }

drawBlock : M.Block -> List Renderable
drawBlock block =
    case block of
        M.Base ( x, y, z ) ->
            [ BoxRenderable <| drawBase "base" (toFloat x) (toFloat y) (toFloat z) ]

        M.Bridge ( x, y, z ) ->
            [ BoxRenderable
                { x = toFloat x * 10
                , y = toFloat y * 10
                , z = toFloat z * 10 + 0.5
                , sizeX = 10
                , sizeY = 10
                , sizeZ = 1
                , material = "bridge"
                , rotationX = 0
                , rotationY = 0
                , rotationZ = 0
                }
            , BoxRenderable <| drawBase "base" (toFloat x) (toFloat y) (toFloat z - 1)
            ]

        M.Greenery ( x, y, z ) ->
            BoxRenderable (drawBase "base" (toFloat x) (toFloat y) (toFloat z)) :: drawGreenery x y z

        M.Stairs ( x, y, z ) dir ->
            let
                fx = toFloat x
                fy = toFloat y
                fz = toFloat z

                stepBox ( cx, cy, cz ) ( sw, sd, sh ) =
                    { x = fx * 10 + cx
                    , y = fy * 10 + cy
                    , z = fz * 10 + cz
                    , sizeX = sw
                    , sizeY = sd
                    , sizeZ = sh
                    , material = "stairs"
                    , rotationX = 0
                    , rotationY = 0
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

                oneBox i = BoxRenderable (stepBox (centerFun i) (dimsFun i))
            in
            List.map oneBox (List.range 0 9) ++ [ BoxRenderable <| drawBase "stairs" fx fy (fz - 1) ]


drawGreenery : Int -> Int -> Int -> List Renderable
drawGreenery x y z =
    let
        fx = toFloat x * 10
        fy = toFloat y * 10
        fz = toFloat z * 10

        s r ( dx, dy, dz ) =
            SphereRenderable
                { x = fx + dx
                , y = fy + dy
                , z = fz + dz
                , radius = r
                , material = "greenery"
                }
    in
    [ s 3.5 ( 0, 2.3, 2.8 )
    , s 3.0 ( 3.0, -2.0, 2.4 )
    , s 2.7 ( -1.5, -2.0, 1.8 )
    ]


drawEnd : M.Maze -> M.PlayerState -> ( Vec3, Vec3, Vec3 ) -> Float -> Bool -> List Box
drawEnd maze playerState ( _, _, head ) timer initialFall =
    let
        hatTransform = Animate.computeHatTransform maze playerState head timer initialFall

        sZ = 1.0 - 0.6 * hatTransform.squash
        sXY = 1.0 + 0.6 * hatTransform.squash

        hatPart rotation =
            { x = hatTransform.x
            , y = hatTransform.y
            , z = hatTransform.z + (0.8 * sZ)
            , sizeX = 1.6 * sXY
            , sizeY = 1.6 * sXY
            , sizeZ = 1.6 * sZ
            , material = "goal"
            , rotationX = 0
            , rotationY = 0
            , rotationZ = rotation
            }
    in
    [ hatPart 0, hatPart 30, hatPart 60 ]


drawHalo : M.Maze -> M.PlayerState -> List Renderable
drawHalo maze playerState =
    let
        ( x, y, z ) = Animate.interpolatedPosition playerState
        fix = Animate.getFix maze ( x, y, z )
        haloZ = z * 10 + fix
    in
    [ PlaneRenderable
        { x = x * 10
        , y = y * 10
        , z = haloZ
        , sizeX = 10
        , sizeY = 10
        , material = "halo"
        , rotationX = 0
        , rotationY = 0
        , rotationZ = 0
        }
    ]


drawPlayer : ( Vec3, Vec3, Vec3 ) -> List Sphere
drawPlayer ( p1, p2, p3 ) =
    [ { x = p1.x, y = p1.y, z = p1.z, radius = 2.2, material = "player" }
    , { x = p2.x, y = p2.y, z = p2.z, radius = 1.8, material = "player" }
    , { x = p3.x, y = p3.y, z = p3.z, radius = 1.4, material = "player" }
    ]


drawFocus : ME.Mode -> M.Position -> List Renderable
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
                xmid = toFloat x * 10
                ymid = toFloat y * 10
                zmid = toFloat z * 10 - 5


                s xpos ypos zpos =
                    SphereRenderable { x = xpos, y = ypos, z = zpos, radius = 0.5, material = "focus" }

                -- 12 lines, line thickness:
                r = 0.2

                -- 4 along X
                cx xpos ypos zpos =
                    BoxRenderable { x = xpos, y = ypos, z = zpos, sizeX = 10, sizeY = r, sizeZ = r, material = "focus", rotationX = 0, rotationY = 0, rotationZ = 0 }
                -- 4 along Y
                cy xpos ypos zpos =
                    BoxRenderable { x = xpos, y = ypos, z = zpos, sizeX = r, sizeY = 10, sizeZ = r, material = "focus", rotationX = 0, rotationY = 0, rotationZ = 0 }
                -- 4 along Z
                cz xpos ypos zpos =
                    BoxRenderable { x = xpos, y = ypos, z = zpos, sizeX = r, sizeY = r, sizeZ = 10, material = "focus", rotationX = 0, rotationY = 0, rotationZ = 0 }
            in
            [ s xmin ymin zmin, s xmax ymin zmin, s xmin ymax zmin, s xmax ymax zmin
            , s xmin ymin zmax, s xmax ymin zmax, s xmin ymax zmax, s xmax ymax zmax
            -- Connections along X
            , cx xmid ymin zmin, cx xmid ymax zmin, cx xmid ymin zmax, cx xmid ymax zmax
            -- Connections along Y
            , cy xmin ymid zmin, cy xmax ymid zmin, cy xmin ymid zmax, cy xmax ymid zmax
            -- Connections along Z
            , cz xmin ymin zmid, cz xmax ymin zmid, cz xmin ymax zmid, cz xmax ymax zmid
            ]


type alias CameraConfig =
    { viewSize : Float
    , focalPoint : Vec3
    , cameraPosition : Vec3
    }


computeCameraConfig : Model -> CameraConfig
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

        distance = 300.0
        cameraPosition3d =
            { x = focal3d.x + distance * cos a * cos e
            , y = focal3d.y + distance * sin a * cos e
            , z = focal3d.z + distance * sin e
            }

        widthPx = model.widthPx |> toFloat
        heightPx = model.heightPx |> toFloat
        aspect =
            if heightPx > 0 then widthPx / heightPx
            else 1.0

        -- Apply padding (0.9 factor means 5% on each side)
        paddingFactor = 0.9
        viewSize = max h (w / aspect) / paddingFactor
    in
    { viewSize = viewSize
    , focalPoint =
        { x = focal3d.x
        , y = focal3d.y
        , z = focal3d.z
        }
    , cameraPosition =
        { x = cameraPosition3d.x
        , y = cameraPosition3d.y
        , z = cameraPosition3d.z
        }
    }
