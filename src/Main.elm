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
import List.Nonempty as NE exposing (Nonempty)
import LuminousFlux
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
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
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , elapsedTime = Quantity.zero
      , orbiting = False
      , azimuth = Angle.degrees -135
      , elevation = Angle.degrees 30
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
        ]


createBox : Int -> Int -> Int -> Entity coordinates
createBox x y z =
    let
        boxCenter =
            Point3d.centimeters
                (toFloat (x * 10))
                (toFloat (y * 10))
                (toFloat (z * 5) - 2.5)

        boxDimensions =
            ( Length.centimeters 10
            , Length.centimeters 10
            , Length.centimeters 5
            )

        boxMaterial =
            Material.metal
                { baseColor = Color.rgb255 255 255 255
                , roughness = 0.8
                }
    in
    Scene3d.blockWithShadow boxMaterial
        (Block3d.centeredOn (Frame3d.atPoint boxCenter) boxDimensions)


zigZag : Int -> List ( Int, Int )
zigZag tilesPerSide =
    let
        ( firstX, firstY ) =
            if modBy 2 tilesPerSide == 0 then
                ( -(ceiling <| toFloat tilesPerSide / 2)
                , ceiling <| toFloat tilesPerSide / 2
                )

            else
                ( -(ceiling <| toFloat tilesPerSide / 2)
                , (ceiling <| toFloat tilesPerSide / 2) - 1
                )

        next : Int -> Int -> ( Int, Int )
        next x y =
            if x == -y then
                ( x, y - 1 )

            else
                ( x + 1, y )

        build : Nonempty ( Int, Int ) -> Nonempty ( Int, Int )
        build acc =
            let
                ( x, y ) =
                    NE.head acc
            in
            if x >= firstY && y <= firstX then
                acc

            else
                build <| NE.cons (next x y) acc
    in
    NE.singleton ( firstX, firstY )
        |> build
        |> NE.toList


zz : Int -> Int -> Int -> List (Entity WorldCoordinates)
zz tilesPerSide shiftBack height =
    zigZag tilesPerSide
        |> List.map (\( x, y ) -> createBox (x + shiftBack) (y + shiftBack) height)


playerMaterial : Material.Material coordinates { a | normals : () }
playerMaterial =
    Material.metal
        { baseColor = Color.rgb255 255 255 255
        , roughness = 0.7
        }


createPlayer : Float -> Float -> Float -> List (Entity WorldCoordinates)
createPlayer x y z =
    let
        playerSphere xcm ycm zcm r =
            Scene3d.sphereWithShadow playerMaterial <|
                Sphere3d.atPoint
                    (Point3d.centimeters xcm ycm zcm)
                    (Length.centimeters r)
    in
    [ playerSphere x y (z + 0.9) 1
    , playerSphere x y (z + 2.4) 0.8
    , playerSphere x y (z + 3.6) 0.6
    ]


view : Model -> Browser.Document Msg
view model =
    let
        lightLeft =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters -3 0.5 0.9
                , chromaticity = Light.chromaticity { x = 0.5, y = 0.4 }
                , intensity = LuminousFlux.lumens 150000
                }

        lightRight =
            Light.point (Light.castsShadows True)
                { position = Point3d.meters 0.5 -3 0.9
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

        boxes : List (Entity WorldCoordinates)
        boxes =
            List.concat
                [ zz 6 0 0
                , zz 6 1 1
                , zz 5 2 2
                , zz 3 3 3
                , zz 3 4 4
                , zz 1 5 5
                ]

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 30
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 10
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
            , entities = createPlayer 0 0 0 ++ boxes
            }
        ]
    }
