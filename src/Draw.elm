module Draw exposing (drawScene)

import Angle
import Block3d
import Camera3d
import Color
import Direction3d
import Frame3d
import Illuminance
import Length
import LuminousFlux
import Maze as M
import Point3d
import Scene3d exposing (Entity)
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d

type WorldCoordinates = WorldCoordinates


-- Camera

camera azimuth elevation = Camera3d.perspective
    { viewpoint = Viewpoint3d.orbitZ
        { focalPoint = Point3d.centimeters 0 0 50
        , azimuth = azimuth
        , elevation = elevation
        , distance = Length.meters 15
        }
    , verticalFieldOfView = Angle.degrees 5
    }


-- Lights

lightLeft1 = Light.point (Light.castsShadows True)
    { position = Point3d.meters -2 3 5
    , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
    , intensity = LuminousFlux.lumens 150000
    }

lightLeft2 = Light.point (Light.castsShadows True)
    { position = Point3d.meters -2 3 6
    , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
    , intensity = LuminousFlux.lumens 150000
    }

lightLeft3 = Light.point (Light.castsShadows True)
    { position = Point3d.meters -3 3 5
    , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
    , intensity = LuminousFlux.lumens 150000
    }

lightLeft4 = Light.point (Light.castsShadows True)
    { position = Point3d.meters -3 3 6
    , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
    , intensity = LuminousFlux.lumens 150000
    }

softLeft = Light.soft
    { upDirection = Direction3d.xyZ (Angle.degrees 55) (Angle.degrees 25)
    , chromaticity = Light.sunlight
    , intensityAbove = Illuminance.lux 300
    , intensityBelow = Illuminance.lux 0
    }

lights : Scene3d.Lights coordinates
lights = Scene3d.fiveLights lightLeft1 lightLeft2 lightLeft3 lightLeft4 softLeft


-- Materials

baseMaterial = Material.metal
    { baseColor = Color.rgb255 255 255 255
    , roughness = 0.8
    }

stairsMaterial = Material.metal
    { baseColor = Color.rgb255 255 200 150
    , roughness = 0.8
    }

bridgeMaterial = Material.metal
    { baseColor = Color.rgb255 255 200 200
    , roughness = 0.8
    }

playerMaterial = Material.metal
    { baseColor = Color.rgb255 255 255 255
    , roughness = 0.4
    }

goalMaterial = Material.metal
    { baseColor = Color.rgb255 20 20 20
    , roughness = 0.7
    }

selectedMaterial = Material.color Color.orange


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

drawBridge : Float -> Float -> Float -> M.Direction -> List (Entity WorldCoordinates)
drawBridge x y z dir =
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
    M.Stairs ( x, y, z ) dir ->
        drawStairs (toFloat x) (toFloat y) (toFloat z) dir
    M.Bridge ( x, y, z ) dir ->
        drawBridge (toFloat x) (toFloat y) (toFloat z) dir

drawMaze : M.Maze -> List (Entity WorldCoordinates)
drawMaze = M.toBlocks >> List.concatMap drawBlock

drawPlayer : M.Position -> List (Entity WorldCoordinates)
drawPlayer ( x, y, z ) =
    let
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
    [ playerSphere 2.0 2.2
    , playerSphere 5.5 1.8
    , playerSphere 8.5 1.4
    ]

drawEnd : M.Position -> List (Entity WorldCoordinates)
drawEnd ( x, y, z ) =
    let
        point = Frame3d.atPoint <| Point3d.centimeters (toFloat x * 10) (toFloat y * 10) (toFloat z * 10)
        hatPart rotation = Scene3d.blockWithShadow goalMaterial <|
            Block3d.centeredOn
                ( point |> Frame3d.rotateAroundOwn Frame3d.zAxis (Angle.degrees rotation) )
                ( Length.centimeters 2
                , Length.centimeters 2
                , Length.centimeters 5
                )
    in
    [ hatPart  0
    , hatPart 30
    , hatPart 60
    ]

drawFocus : M.Position -> List (Entity WorldCoordinates)
drawFocus ( x, y, z ) =
    let
        selectedSphere xmm ymm zmm =
            Scene3d.sphere selectedMaterial <|
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
        , camera = camera model.azimuth model.elevation
        , clipDepth = Length.centimeters 1
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor Color.lightBlue
        , entities =
            drawPlayer model.player ++
            drawFocus model.focus ++
            drawMaze model.maze ++
            drawEnd (M.endPosition model.maze)
        }
