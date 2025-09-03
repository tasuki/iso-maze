module Draw exposing (drawScene)

import Angle
import Block3d
import Camera3d
import Color
import Decorations as D
import Direction3d
import Frame3d
import Illuminance
import Length
import Luminance
import LuminousFlux
import Maze as M
import MazeEdit as ME
import Point3d
import Scene3d exposing (Entity)
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d

type WorldCoordinates = WorldCoordinates


-- Camera

camera azimuth elevation =
    Camera3d.perspective
        { viewpoint = viewpoint 50 azimuth elevation
        , verticalFieldOfView = Angle.degrees 5
        }

cameraOrtho azimuth elevation =
    Camera3d.orthographic
        { viewpoint = viewpoint 55 azimuth elevation
        , viewportHeight = Length.meters 1.4
        }

viewpoint focalHeight azimuth elevation =
    Viewpoint3d.orbitZ
        { focalPoint = Point3d.centimeters 0 0 focalHeight
        , azimuth = azimuth
        , elevation = elevation
        , distance = Length.meters 15
        }


-- Lights

leftLightChroma = Light.chromaticity { x = 0.47, y = 0.4 }

spotLight = Light.point (Light.castsShadows False)
    { position = Point3d.meters -2 3 5
    , chromaticity = leftLightChroma
    , intensity = LuminousFlux.lumens 100000
    }

spotLightSh = Light.point (Light.castsShadows True)
    { position = Point3d.meters -2 3 5
    , chromaticity = leftLightChroma
    , intensity = LuminousFlux.lumens 100000
    }

fillLightAbove = Light.soft
    { upDirection = Direction3d.xyZ (Angle.degrees 55) (Angle.degrees 25)
    , chromaticity = Light.sunlight
    , intensityAbove = Illuminance.lux 250
    , intensityBelow = Illuminance.lux 0
    }

lights : Scene3d.Lights coordinates
lights = Scene3d.threeLights spotLight spotLightSh fillLightAbove


-- Materials

baseMaterial = Material.matte <| Color.rgb255 255 255 255
stairsMaterial = Material.matte <| Color.rgb255 255 230 200
bridgeMaterial = Material.matte <| Color.rgb255 200 100 100
railingMaterial = Material.matte <| Color.rgb255 200 200 200

playerMaterial = Material.metal
    { baseColor = Color.rgb255 160 240 255
    , roughness = 0.5
    }
goalMaterial = Material.metal
    { baseColor = Color.rgb255 20 20 20
    , roughness = 0.5
    }


-- Drawing

createBlock material center dimensions =
    Scene3d.blockWithShadow material
        (Block3d.centeredOn (Frame3d.atPoint center) dimensions)

drawBase : Material.Uniform coordinates -> Float -> Float -> Float -> Entity coordinates
drawBase material x y z =
    createBlock material
        ( Point3d.centimeters (x * 10) (y * 10) (z * 5 - 5) )
        ( Length.centimeters 10
        , Length.centimeters 10
        , Length.centimeters <| z * 10 + 10
        )

drawStairs : Float -> Float -> Float -> M.Direction -> List (Entity WorldCoordinates)
drawStairs x y z dir =
    let
        stepCenter xx yy zz =
            Point3d.centimeters (x * 10 + xx) (y * 10 + yy) (z * 10 + zz)
        stepDimensions xx yy zz =
            ( Length.centimeters xx
            , Length.centimeters yy
            , Length.centimeters zz
            )

        ( centerFun, dimsFun ) = case dir of
            M.SE ->
                ( \i -> stepCenter 0 (4.5 - toFloat i) (-5.0 - 0.5 * toFloat i)
                , \i -> stepDimensions 10 1 (10 - toFloat i)
                )
            M.SW ->
                ( \i -> stepCenter (4.5 - toFloat i) 0 (-5.0 - 0.5 * toFloat i)
                , \i -> stepDimensions 1 10 (10 - toFloat i)
                )
            M.NE ->
                ( \i -> stepCenter (4.5 - toFloat i) 0 (-9.5 + 0.5 * toFloat i)
                , \i -> stepDimensions 1 10 (1 + toFloat i)
                )
            M.NW ->
                ( \i -> stepCenter 0 (4.5 - toFloat i) (-9.5 + 0.5 * toFloat i)
                , \i -> stepDimensions 10 1 (1 + toFloat i)
                )

        oneBox i = createBlock stairsMaterial (centerFun i) (dimsFun i)
    in
    (List.map oneBox (List.range 0 9)) ++ [ drawBase stairsMaterial x y (z - 1) ]

drawBridge : Float -> Float -> Float -> List (Entity WorldCoordinates)
drawBridge x y z =
    let
        center = Point3d.centimeters (x * 10) (y * 10) (z * 10 + 0.5)
        bridge = createBlock bridgeMaterial center
            ( Length.centimeters 10
            , Length.centimeters 10
            , Length.centimeters 1
            )
    in
    [ bridge ] ++ [ drawBase baseMaterial x y (z - 1) ]

drawBlock : M.Block -> List (Entity WorldCoordinates)
drawBlock block = case block of
    M.Base ( x, y, z ) ->
        [ drawBase baseMaterial (toFloat x) (toFloat y) (toFloat z) ]
    M.Bridge ( x, y, z ) ->
        drawBridge (toFloat x) (toFloat y) (toFloat z)
    M.Stairs ( x, y, z ) dir ->
        drawStairs (toFloat x) (toFloat y) (toFloat z) dir

drawRailing : ( M.Block, M.Direction ) -> List (Entity WorldCoordinates)
drawRailing ( block, dir ) =
    let
        ( x, y, z ) = M.blockPosition block

        baseCoords = case block of
            M.Stairs _ _ ->
                [ -4.5, -3.5, -2.5, -1.5, -0.5
                ,  0.5,  1.5,  2.5,  3.5,  4.5 ]
            _ -> [-4, -2, 0, 2, 4]

        generateCoordsX xd = List.map (\yd -> (xd, yd)) baseCoords
        generateCoordsY yd = List.map (\xd -> (xd, yd)) baseCoords

        zd xd yd = case block of
            M.Base _ -> 0.2
            M.Bridge _ -> 1.2
            M.Stairs _ stairsDir ->
                case stairsDir of
                    M.SE -> yd - 4.3
                    M.SW -> xd - 4.3
                    M.NW -> -4.3 - yd
                    M.NE -> -4.3 - xd

        createRailing ( xd, yd ) = createBlock railingMaterial
            (Point3d.centimeters
                (toFloat x * 10 + xd)
                (toFloat y * 10 + yd)
                (toFloat z * 10 + (zd xd yd))
            )
            ( Length.centimeters 0.5
            , Length.centimeters 0.5
            , Length.centimeters 0.5
            )

        centers : List ( Float, Float )
        centers = case dir of
            M.SE -> generateCoordsY -4
            M.SW -> generateCoordsX -4
            M.NW -> generateCoordsY 4
            M.NE -> generateCoordsX 4
    in
    List.map createRailing centers

drawMaze : M.Maze -> List (Entity WorldCoordinates)
drawMaze = M.toBlocks >> List.concatMap drawBlock

drawPlayer : M.Position -> M.Maze -> List (Entity WorldCoordinates)
drawPlayer ( x, y, z ) maze =
    let
        zd = case M.get ( x, y ) maze of
            Just (M.Stairs _ _) -> -5
            _ -> 0

        playerSphere zShift r =
            Scene3d.sphereWithShadow playerMaterial <|
                Sphere3d.atPoint
                    (Point3d.centimeters
                        (toFloat x * 10)
                        (toFloat y * 10)
                        (toFloat z * 10 + zShift)
                    )
                    (Length.centimeters r)
    in
    [ playerSphere (2.0 + zd) 2.2
    , playerSphere (5.5 + zd) 1.8
    , playerSphere (8.5 + zd) 1.4
    ]

drawEnd : M.Position -> Bool -> List (Entity WorldCoordinates)
drawEnd ( x, y, z ) isAtEnd =
    let
        zd = if isAtEnd then 9.5 else 0
        point = Frame3d.atPoint <|
            Point3d.centimeters (toFloat x * 10) (toFloat y * 10) (toFloat z * 10 + 1 + zd)
        hatPart rotation = Scene3d.blockWithShadow goalMaterial <|
            Block3d.centeredOn
                ( point |> Frame3d.rotateAroundOwn Frame3d.zAxis (Angle.degrees rotation) )
                ( Length.centimeters 1.6
                , Length.centimeters 1.6
                , Length.centimeters 1.6
                )
    in
    [ hatPart  0
    , hatPart 30
    , hatPart 60
    ]

drawFocus : ME.Mode -> M.Position -> List (Entity WorldCoordinates)
drawFocus mode ( x, y, z ) =
    let
        selectedSphere xmm ymm zmm =
            Scene3d.sphere (Material.color Color.orange) <|
                Sphere3d.atPoint
                    (Point3d.millimeters xmm ymm zmm)
                    (Length.millimeters 5)

        xmin = toFloat <| x * 100 -  50
        xmax = toFloat <| x * 100 +  50
        ymin = toFloat <| y * 100 -  50
        ymax = toFloat <| y * 100 +  50
        zmin = toFloat <| z * 100 - 100
        zmax = toFloat <| z * 100
    in
    case mode of
        ME.Running -> []
        ME.Editing ->
            [ selectedSphere xmin ymin zmin
            , selectedSphere xmax ymin zmin
            , selectedSphere xmin ymax zmin
            , selectedSphere xmax ymax zmin
            , selectedSphere xmin ymin zmax
            , selectedSphere xmax ymin zmax
            , selectedSphere xmin ymax zmax
            , selectedSphere xmax ymax zmax
            ]

drawScene model =
    Scene3d.custom
        { lights = lights
        , camera = cameraOrtho model.azimuth model.elevation
        , clipDepth = Length.centimeters 1
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor Color.lightBlue
        , entities =
            drawPlayer model.player model.maze ++
            drawFocus model.mode model.focus ++
            drawMaze model.maze ++
            drawEnd (M.endPosition model.maze) (M.isAtEnd model.player model.maze) ++
            List.concatMap drawRailing (D.getRailings model.maze)
        }
