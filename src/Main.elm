module Main exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events as BE
import Browser.Navigation as Nav
import Codec
import DrawThree as D
import DocumentDecoders as DD
import Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Maze as M
import MazeEdit as ME
import Pixels
import Quantity
import SampleMazes as SM
import Task
import Url exposing (Url)

defaultMaze = SM.ziggurat2

type alias Model =
    { navKey : Nav.Key
    , widthPx : Int
    , heightPx : Int
    , elapsedTime : Duration
    , pointerStart : Maybe DD.DocumentCoords
    , pointerLast : Maybe DD.DocumentCoords
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , mode : ME.Mode
    , maze : M.Maze
    , player : M.Position
    , focus : M.Position
    }

type Msg
    = Noop
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Resize Int Int
    | Tick Duration
    | Started DD.DocumentCoords
    | Moved DD.DocumentCoords
    | Finished DD.DocumentCoords
    | Cancelled DD.DocumentCoords
    | VisibilityChange BE.Visibility
    | CameraReset
    | FocusShift M.Vector
    | ToggleMode
    | ToggleBlock
    | ToggleStairs
    | ToggleBridge
    | PlaceStart
    | PlaceEnd
    | Go M.Direction

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url navKey =
    (changeRouteTo url
        { navKey = navKey
        , widthPx = 0
        , heightPx = 0
        , elapsedTime = Quantity.zero
        , pointerStart = Nothing
        , pointerLast = Nothing
        , orbiting = False
        , azimuth = Angle.degrees D.initialAzimuth
        , elevation = Angle.degrees D.initialElevation
        , mode = ME.Running
        , maze = defaultMaze
        , player = M.startPosition defaultMaze
        , focus = ( 0, 0, 1 )
        }
    , Task.perform
        (\{ viewport } -> Resize
            (round viewport.width)
            (round viewport.height)
        )
        Browser.Dom.getViewport
    )


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( newModel, cmd ) =
            updateModel message model
    in
    ( newModel, Cmd.batch [ cmd, D.renderThreeJS (D.sceneData newModel) ] )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel message model =
    case message of
        UrlChanged url ->
            ( changeRouteTo url model, Cmd.none )

        Resize width height ->
            ( { model | widthPx = width, heightPx = height }, Cmd.none )

        Tick elapsed ->
            ( { model | elapsedTime = model.elapsedTime |> Quantity.plus elapsed }
            , Cmd.none
            )

        Started dc ->
            ( { model
                | orbiting = model.mode == ME.Editing
                , pointerStart = Just dc
                , pointerLast = Just dc
              }
            , Cmd.none
            )

        Moved dc ->
            case ( model.orbiting, model.pointerLast ) of
                ( True, Just lastDc ) ->
                    let
                        rotationRate = Angle.degrees 0.5 |> Quantity.per Pixels.pixel
                        newAzimuth =
                            model.azimuth
                                |> Quantity.minus (dc.x - lastDc.x |> Pixels.pixels |> Quantity.at rotationRate)
                        newElevation =
                            model.elevation
                                |> Quantity.plus (dc.y - lastDc.y |> Pixels.pixels |> Quantity.at rotationRate)
                                |> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
                    in
                    ( { model
                        | pointerLast = Just dc
                        , azimuth = newAzimuth
                        , elevation = newElevation
                      }
                    , Cmd.none
                    )
                _ ->
                    ( model, Cmd.none )

        Finished dc ->
            case ( model.mode, model.pointerStart ) of
                ( ME.Running, Just startDc ) ->
                    movePlayer { model | orbiting = False, pointerStart = Nothing, pointerLast = Nothing } startDc dc

                _ ->
                    updateModel (Cancelled dc) model

        Cancelled _ ->
            ( { model | orbiting = False, pointerStart = Nothing, pointerLast = Nothing }, Cmd.none )

        VisibilityChange BE.Visible ->
            ( model, Cmd.none )

        VisibilityChange BE.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        CameraReset ->
            ( { model
                | azimuth = Angle.degrees D.initialAzimuth
                , elevation = Angle.degrees D.initialElevation
              }
            , Cmd.none
            )

        FocusShift vector ->
            let
                newFocus =
                    M.shiftPosition model.focus vector
            in
            if M.isValidPosition newFocus then
                ( { model | focus = newFocus }, Cmd.none )

            else
                ( model, Cmd.none )

        ToggleMode ->
            ( { model
                | mode =
                    if model.mode == ME.Running then
                        ME.Editing

                    else
                        ME.Running
              }
            , Cmd.none
            )

        ToggleBlock ->
            updateMaze ME.toggleBlock model

        ToggleStairs ->
            updateMaze ME.toggleStairs model

        ToggleBridge ->
            updateMaze ME.toggleBridge model

        PlaceStart ->
            updateMaze ME.placeStart model

        PlaceEnd ->
            updateMaze ME.placeEnd model

        Go dir ->
            let
                newPos : M.Position
                newPos =
                    M.move model.player dir model.maze
                        |> Maybe.withDefault model.player
            in
            ( { model | player = newPos }, Cmd.none )

        _ ->
            ( model, Cmd.none )

movePlayer : Model -> DD.DocumentCoords -> DD.DocumentCoords -> ( Model, Cmd Msg )
movePlayer model startDc dc =
    let
        (up, left) =
            if startDc == dc then
                -- simple click: see quadrant
                ( dc.y * 2 < (model.heightPx |> toFloat)
                , dc.x * 2 < (model.widthPx |> toFloat)
                )
            else
                -- drag: see diff
                ( dc.y < startDc.y
                , dc.x < startDc.x
                )
        msg =
            if up then
                if left then Go M.NW
                else Go M.NE
            else
                if left then Go M.SW
                else Go M.SE
    in
    updateModel msg model

changeRouteTo : Url.Url -> Model -> Model
changeRouteTo url model =
    case Maybe.andThen Codec.decode url.query of
        Just maze -> { model | maze = maze, player = M.startPosition maze }
        Nothing -> model

updateMaze : (M.Position -> M.Maze -> M.Maze) -> Model -> ( Model, Cmd Msg )
updateMaze fun model =
    let newMaze = fun model.focus model.maze in
    ( { model | maze = newMaze }
    , pushUrl model.navKey newMaze
    )

pushUrl : Nav.Key -> M.Maze -> Cmd msg
pushUrl navKey maze =
    Nav.pushUrl navKey <| "?" ++ Codec.encode maze


-- Subscriptions

keydown : ME.Mode -> String -> Msg
keydown mode keycode =
    case mode of
        ME.Running ->
            case keycode of
                "e" -> ToggleMode
                "c" -> CameraReset
                "ArrowLeft"  -> Go M.NW
                "ArrowDown"  -> Go M.SW
                "ArrowUp"    -> Go M.NE
                "ArrowRight" -> Go M.SE
                _   -> Noop
        ME.Editing ->
            case keycode of
                "e" -> ToggleMode
                "c" -> CameraReset
                "h" -> FocusShift (  0,  1,  0 )
                "l" -> FocusShift (  0, -1,  0 )
                "k" -> FocusShift (  1,  0,  0 )
                "j" -> FocusShift ( -1,  0,  0 )
                "i" -> FocusShift (  0,  0,  1 )
                "u" -> FocusShift (  0,  0, -1 )
                " " -> ToggleBlock
                "s" -> ToggleStairs
                "b" -> ToggleBridge
                "a" -> PlaceStart
                "z" -> PlaceEnd
                _   -> Noop

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onResize Resize
        -- TODO , BE.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , BE.onVisibilityChange VisibilityChange
        , BE.onKeyDown (Decode.map (keydown model.mode) <| Decode.field "key" Decode.string)
        ]


-- View

view : Model -> Browser.Document Msg
view model =
    let
        alwaysWatch =
            [ HE.on "pointerdown" <| DD.decodePrimary Started
            , HE.on "pointerup" <| DD.decodePrimary Finished
            , HE.on "pointercancel" <| DD.decodePrimary Cancelled
            ]
        watchNow =
            if model.mode == ME.Editing && model.orbiting then
                (HE.preventDefaultOn "pointermove" <| Decode.map (\m -> ( m, True )) (DD.decodePrimary Moved))
                    :: alwaysWatch
            else alwaysWatch
    in
    { title = "Iso Maze"
    , body =
        [ H.div
            (watchNow ++
                [ HA.id "three-container"
                , HA.style "width" "100%"
                , HA.style "height" "100vh"
                ]
            )
            []
        ]
    }
