module Main exposing (main)

import Angle exposing (Angle)
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length
import LuminousFlux
import Maze as M
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import SampleMazes as SM
import Scene3d exposing (Entity)
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Task
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , elapsedTime : Duration
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    }


type Msg
    = Noop
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility
    | CameraReset


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initialAzimuth : Float
initialAzimuth =
    -135


initialElevation : Float
initialElevation =
    30


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , elapsedTime = Quantity.zero
      , orbiting = False
      , azimuth = Angle.degrees initialAzimuth
      , elevation = Angle.degrees initialElevation
      }
    , Task.perform
        (\{ viewport } ->
            Resize
                (Pixels.int (round viewport.width))
                (Pixels.int (round viewport.height))
        )
        Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )

        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        Tick elapsed ->
            ( { model | elapsedTime = model.elapsedTime |> Quantity.plus elapsed }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        CameraReset ->
            ( { model | azimuth = Angle.degrees initialAzimuth, elevation = Angle.degrees initialElevation }
            , Cmd.none
            )


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> Resize (Pixels.int width) (Pixels.int height))

        -- TODO , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , Browser.Events.onVisibilityChange VisibilityChange
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        , Browser.Events.onKeyDown (Decode.map keydown <| Decode.field "key" Decode.string)
        ]


keydown : String -> Msg
keydown keycode =
    case keycode of
        "c" ->
            CameraReset

        _ ->
            Noop



-- Materials


baseMaterial : Material.Material coordinates { a | normals : () }
baseMaterial =
    Material.metal
        { baseColor = Color.rgb255 255 255 255
        , roughness = 0.8
        }


playerMaterial : Material.Material coordinates { a | normals : () }
playerMaterial =
    Material.metal
        { baseColor = Color.rgb255 255 255 255
        , roughness = 0.7
        }



-- Drawing


drawBase : Float -> Float -> Float -> Entity coordinates
drawBase x y z =
    let
        boxCenter =
            Point3d.centimeters
                (x * 10)
                (y * 10)
                (z * 5 - 5)

        boxDimensions =
            ( Length.centimeters 10
            , Length.centimeters 10
            , Length.centimeters <| z * 10 + 10
            )
    in
    Scene3d.blockWithShadow baseMaterial
        (Block3d.centeredOn (Frame3d.atPoint boxCenter) boxDimensions)


drawStairs : Float -> Float -> Float -> M.Direction -> List (Entity WorldCoordinates)
drawStairs x y z dir =
    let
        stepCenter xx yy zz =
            Point3d.centimeters
                (x * 10 + xx)
                (y * 10 + yy)
                (z * 10 + zz)

        stepDimensions xx yy zz =
            ( Length.centimeters xx
            , Length.centimeters yy
            , Length.centimeters zz
            )

        ( centerFun, dimsFun ) =
            case dir of
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

        boxes =
            List.map (\i -> ( centerFun i, dimsFun i )) (List.range 0 9)

        createBlock ( center, dimensions ) =
            Scene3d.blockWithShadow baseMaterial
                (Block3d.centeredOn (Frame3d.atPoint center) dimensions)
    in
    List.map createBlock boxes ++ [ drawBase x y (z - 1) ]


drawPlayer : Float -> Float -> Float -> List (Entity WorldCoordinates)
drawPlayer x y z =
    let
        playerSphere xcm ycm zcm r =
            Scene3d.sphereWithShadow playerMaterial <|
                Sphere3d.atPoint
                    (Point3d.centimeters xcm ycm zcm)
                    (Length.centimeters r)
    in
    [ playerSphere x y (z + 2) 2.2
    , playerSphere x y (z + 5.5) 1.8
    , playerSphere x y (z + 8.5) 1.4
    ]


drawBlock : M.Block -> List (Entity WorldCoordinates)
drawBlock block =
    case block of
        M.Base ( x, y, z ) ->
            [ drawBase (toFloat x) (toFloat y) (toFloat z) ]

        M.Stairs ( x, y, z ) dir ->
            drawStairs (toFloat x) (toFloat y) (toFloat z) dir


drawMaze : M.Maze -> List (Entity WorldCoordinates)
drawMaze =
    List.concatMap drawBlock


view : Model -> Browser.Document Msg
view model =
    let
        lightLeft =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters -3 0 0.9
                , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
                , intensity = LuminousFlux.lumens 150000
                }

        lightRight =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters 0 -3 0.9
                , chromaticity = Light.chromaticity { x = 0.1, y = 0.35 }
                , intensity = LuminousFlux.lumens 2000
                }

        backLight =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters 2 4 5
                , chromaticity = Light.chromaticity { x = 0.3, y = 0.4 }
                , intensity = LuminousFlux.lumens 30000
                }

        softLeft =
            Light.soft
                { upDirection = Direction3d.xyZ (Angle.degrees 90) (Angle.degrees 0)
                , chromaticity = Light.sunlight
                , intensityAbove = Illuminance.lux 300
                , intensityBelow = Illuminance.lux 0
                }

        softRight =
            Light.soft
                { upDirection = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees 0)
                , chromaticity = Light.sunlight
                , intensityAbove = Illuminance.lux 10
                , intensityBelow = Illuminance.lux 0
                }

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 30
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 15
                        }
                , verticalFieldOfView = Angle.degrees 5
                }
    in
    { title = "Iso Maze"
    , body =
        [ Scene3d.custom
            { lights = Scene3d.fiveLights lightLeft lightRight backLight softLeft softRight
            , camera = camera
            , clipDepth = Length.centimeters 1
            , exposure = Scene3d.exposureValue 6
            , toneMapping = Scene3d.hableFilmicToneMapping
            , whiteBalance = Light.fluorescent
            , antialiasing = Scene3d.multisampling
            , dimensions = ( model.width, model.height )
            , background = Scene3d.backgroundColor Color.lightBlue
            , entities = drawPlayer 0 0 0 ++ drawMaze SM.roundabout
            }
        ]
    }
