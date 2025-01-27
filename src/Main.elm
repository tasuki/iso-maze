module Main exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events as BE
import Draw as D
import Duration exposing (Duration)
import Json.Decode as Decode exposing (Decoder)
import Maze as M
import MazeEdit as ME
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import SampleMazes as SM
import Task

initialAzimuth = -135
initialElevation = 30

type alias Model =
    { width : Quantity Int Pixels
    , height : Quantity Int Pixels
    , elapsedTime : Duration
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , maze : M.Maze
    , player : M.Position
    , focus : M.Position
    }

type Msg
    = Noop
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange BE.Visibility
    | CameraReset
    | FocusShift M.Vector
    | ToggleBlock
    | ToggleStairs

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
      , azimuth = Angle.degrees initialAzimuth
      , elevation = Angle.degrees initialElevation
      , maze = SM.assymetric
      , player = ( 0, 0, 0 )
      , focus = ( 0, 0, 1 )
      }
    , Task.perform
        (\{ viewport } -> Resize
            (Pixels.int (round viewport.width))
            (Pixels.int (round viewport.height))
        )
        Browser.Dom.getViewport
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model = case message of
    Noop -> ( model, Cmd.none )
    Resize width height ->
        ( { model | width = width, height = height }, Cmd.none )
    Tick elapsed ->
        ( { model | elapsedTime = model.elapsedTime |> Quantity.plus elapsed }
        , Cmd.none
        )

    MouseDown -> ( { model | orbiting = True }, Cmd.none )
    MouseUp -> ( { model | orbiting = False }, Cmd.none )

    VisibilityChange BE.Visible -> ( model, Cmd.none )
    VisibilityChange BE.Hidden -> ( { model | orbiting = False }, Cmd.none )

    MouseMove dx dy ->
        if model.orbiting then
            let
                rotationRate = Angle.degrees 0.5 |> Quantity.per Pixels.pixel
                newAzimuth = model.azimuth
                    |> Quantity.minus (dx |> Quantity.at rotationRate)
                newElevation = model.elevation
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
        ( { model
            | azimuth = Angle.degrees initialAzimuth
            , elevation = Angle.degrees initialElevation
          }
        , Cmd.none
        )

    FocusShift vector ->
        let
            newFocus = M.shiftPosition model.focus vector
        in
        if M.isValidPosition newFocus then
            ( { model | focus = newFocus }, Cmd.none )
        else
            ( model, Cmd.none )

    ToggleBlock ->
        ( { model | maze = ME.toggleBlock model.focus model.maze }, Cmd.none )
    ToggleStairs ->
        ( { model | maze = ME.toggleStairs model.focus model.maze }, Cmd.none )

mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onResize (\width height -> Resize (Pixels.int width) (Pixels.int height))
        -- TODO , BE.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , BE.onVisibilityChange VisibilityChange
        , if model.orbiting then
            Sub.batch
                [ BE.onMouseMove mouseMoveDecoder
                , BE.onMouseUp (Decode.succeed MouseUp)
                ]
          else
            BE.onMouseDown (Decode.succeed MouseDown)
        , BE.onKeyDown (Decode.map keydown <| Decode.field "key" Decode.string)
        ]

keydown : String -> Msg
keydown keycode =
    case keycode of
        "c" -> CameraReset
        "h" -> FocusShift (  0,  1,  0 )
        "l" -> FocusShift (  0, -1,  0 )
        "k" -> FocusShift (  1,  0,  0 )
        "j" -> FocusShift ( -1,  0,  0 )
        "i" -> FocusShift (  0,  0,  1 )
        "u" -> FocusShift (  0,  0, -1 )
        " " -> ToggleBlock
        "s" -> ToggleStairs
        _ -> Noop

view : Model -> Browser.Document Msg
view model =
    { title = "Iso Maze"
    , body = [ D.drawScene model ]
    }
