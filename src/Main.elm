port module Main exposing (main)

import Angle exposing (Angle)
import Animate
import Browser
import Browser.Dom
import Browser.Events as BE
import Browser.Navigation as Nav
import Codec
import DrawThree as D
import DocumentDecoders as DD
import Duration exposing (Duration)
import Set exposing (Set)
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
secondsPerStep = 0.25


type alias Model =
    { navKey : Nav.Key
    , widthPx : Int
    , heightPx : Int
    , elapsedTime : Duration
    , pointerStart : Maybe DD.DocumentCoords
    , pointerLast : Maybe DD.DocumentCoords
    , keysDown : Set String
    , debugInfo : Bool
    , orbiting : Bool
    , elevation : Angle
    , azimuth : Angle
    , mode : ME.Mode
    , maze : M.Maze
    , playerState : M.PlayerState
    , animator : Animate.AnimatorState
    , focus : M.Position
    , dpr : Float
    , frameHistory : List { timestamp : Float, duration : Float }
    }

type Msg
    = Noop
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Resize Int Int
    | Tick Duration
    | VisibilityChange BE.Visibility
    | Started DD.DocumentCoords
    | Moved DD.DocumentCoords
    | Finished DD.DocumentCoords
    | Cancelled DD.DocumentCoords
    | KeyDown String
    | KeyUp String
    | CameraReset
    | FocusShift M.Vector
    | ToggleMode
    | ToggleBlock
    | ToggleStairs
    | ToggleBridge
    | PlaceStart
    | PlaceEnd
    | ToggleDebug
    | DprUpdated Float

main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init dpr url navKey =
    let
        initialPos = M.startPosition defaultMaze
        initialTargets = Animate.getPlayerTargets (M.Idle initialPos) defaultMaze

        model =
            { navKey = navKey
            , widthPx = 0
            , heightPx = 0
            , elapsedTime = Quantity.zero
            , pointerStart = Nothing
            , pointerLast = Nothing
            , keysDown = Set.empty
            , debugInfo = False
            , orbiting = False
            , elevation = Angle.degrees D.initialElevation
            , azimuth = Angle.degrees D.initialAzimuth
            , mode = ME.Running
            , maze = defaultMaze
            , playerState = M.Idle initialPos
            , animator = Animate.initAnimator initialTargets
            , focus = ( 0, 0, 1 )
            , dpr = dpr
            , frameHistory = []
            }
    in
    ( changeRouteTo url model
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
        ( newModel, cmd ) = updateModel message model
        targets = Animate.getPlayerTargets newModel.playerState newModel.maze
        isMoving =
            case newModel.playerState of
                M.Idle _ -> Animate.isAnimatorMoving targets newModel.animator
                M.Moving _ -> True

        shouldRender =
            case message of
                Tick _ -> isMoving
                Moved _ -> newModel.mode == ME.Editing && newModel.orbiting
                _ -> True

        ( s1, s2, s3 ) = newModel.animator.spheres
        sceneDataValue =
            D.sceneData
                { azimuth = newModel.azimuth
                , elevation = newModel.elevation
                , maze = newModel.maze
                , playerSpheres = ( s1.current, s2.current, s3.current )
                , focus = newModel.focus
                , mode = newModel.mode
                , widthPx = newModel.widthPx
                , heightPx = newModel.heightPx
                }
    in
    if shouldRender then
        ( newModel
        , Cmd.batch [ cmd, D.renderThreeJS sceneDataValue ]
        )
    else ( newModel, cmd )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel message model =
    case message of
        UrlChanged url ->
            ( changeRouteTo url model, Cmd.none )

        Resize width height ->
            ( { model | widthPx = width, heightPx = height }, Cmd.none )

        Tick elapsed ->
            let
                dt = Duration.inSeconds elapsed
                dtMs = Duration.inMilliseconds elapsed
                newElapsedTime = model.elapsedTime |> Quantity.plus elapsed
                newPlayerState =
                    if model.mode == ME.Running then
                        updatePlayerState dt model.keysDown model.pointerStart model.pointerLast model.maze model.playerState
                    else
                        model.playerState

                targets = Animate.getPlayerTargets newPlayerState model.maze
                newAnimator = Animate.updateAnimator dt targets model.animator

                currentTime = Duration.inMilliseconds newElapsedTime
                newFrameHistory =
                    { timestamp = currentTime, duration = dtMs }
                        :: model.frameHistory
                        |> List.filter (\f -> currentTime - f.timestamp < 1000)
            in
            ( { model
                | elapsedTime = newElapsedTime
                , playerState = newPlayerState
                , animator = newAnimator
                , frameHistory = newFrameHistory
              }
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
                        | azimuth = newAzimuth
                        , elevation = newElevation
                        , pointerLast = Just dc
                      }
                    , Cmd.none
                    )
                _ ->
                    ( { model | pointerLast = Just dc }, Cmd.none )

        Finished _ ->
            ( { model | pointerStart = Nothing, pointerLast = Nothing, orbiting = False }, Cmd.none )

        Cancelled _ ->
            ( { model | pointerStart = Nothing, pointerLast = Nothing, orbiting = False }, Cmd.none )

        VisibilityChange BE.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        VisibilityChange BE.Visible ->
            ( model, Cmd.none )

        CameraReset ->
            ( { model
                | azimuth = Angle.degrees D.initialAzimuth
                , elevation = Angle.degrees D.initialElevation
              }
            , Cmd.none
            )

        FocusShift vector ->
            let
                newFocus = M.shiftPosition model.focus vector
            in
            if M.isValidPosition newFocus then ( { model | focus = newFocus }, Cmd.none )
            else ( model, Cmd.none )

        ToggleMode ->
            ( { model | mode =
                if model.mode == ME.Running then ME.Editing
                else ME.Running
              }
            , Cmd.none
            )

        ToggleBlock -> updateMaze ME.toggleBlock model
        ToggleStairs -> updateMaze ME.toggleStairs model
        ToggleBridge -> updateMaze ME.toggleBridge model
        PlaceStart -> updateMaze ME.placeStart model
        PlaceEnd -> updateMaze ME.placeEnd model

        KeyDown key ->
            let
                newKeysDown = Set.insert key model.keysDown
                newModel = { model | keysDown = newKeysDown }
            in
            case (model.mode, key) of
                (_, "e") -> updateModel ToggleMode newModel
                (_, "c") -> updateModel CameraReset newModel
                (ME.Editing, _) ->
                    let
                        msg = keydown model.mode key
                    in
                    if msg == Noop then (newModel, Cmd.none)
                    else updateModel msg newModel
                _ -> (newModel, Cmd.none)

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        ToggleDebug ->
            ( { model | debugInfo = not model.debugInfo }, Cmd.none )

        DprUpdated dpr ->
            ( { model | dpr = dpr }, Cmd.none )

        _ ->
            ( model, Cmd.none )

updatePlayerState : Float -> Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> M.Maze -> M.PlayerState -> M.PlayerState
updatePlayerState dt keysDown pointerStart pointerLast maze playerState =
    let
        maybeMove pos progress =
            case getDesiredDirection keysDown pointerStart pointerLast of
                Just dir ->
                    case M.move pos dir maze of
                        Just nextTo ->
                            M.Moving { from = pos, to = nextTo, dir = dir, progress = progress }
                        Nothing ->
                            M.Idle pos
                Nothing ->
                    M.Idle pos
    in
    case playerState of
        M.Idle pos -> maybeMove pos 0
        M.Moving m ->
            let
                newProgress = m.progress + (dt / secondsPerStep)
            in
            if newProgress >= 1 then maybeMove m.to (newProgress - 1)
            else M.Moving { m | progress = newProgress }

getDesiredDirection : Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe M.Direction
getDesiredDirection keysDown pointerStart pointerLast =
    let
        kbdDir =
            if Set.member "ArrowLeft" keysDown then Just M.SW
            else if Set.member "ArrowDown" keysDown then Just M.SE
            else if Set.member "ArrowUp" keysDown then Just M.NW
            else if Set.member "ArrowRight" keysDown then Just M.NE
            else Nothing

        joyDir =
            case (pointerStart, pointerLast) of
                (Just start, Just last) ->
                    let
                        dx = last.x - start.x
                        dy = last.y - start.y
                        dist = sqrt (dx*dx + dy*dy)
                        deadzone = 10
                    in
                    if dist > deadzone then
                        if dy < 0 then
                            if dx < 0 then Just M.NW else Just M.NE
                        else
                            if dx < 0 then Just M.SW else Just M.SE
                    else
                        Nothing
                _ -> Nothing
    in
    case kbdDir of
        Just d -> Just d
        Nothing -> joyDir

changeRouteTo : Url.Url -> Model -> Model
changeRouteTo url model =
    case Maybe.andThen Codec.decode url.query of
        Just maze ->
            let
                startPos = M.startPosition maze
                targets = Animate.getPlayerTargets (M.Idle startPos) maze
            in
            { model
                | maze = maze
                , playerState = M.Idle startPos
                , animator = Animate.initAnimator targets
            }
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
        ME.Running -> Noop
        ME.Editing ->
            case keycode of
                "h" -> FocusShift ( -1,  0,  0 )
                "l" -> FocusShift (  1,  0,  0 )
                "k" -> FocusShift (  0,  1,  0 )
                "j" -> FocusShift (  0, -1,  0 )
                "i" -> FocusShift (  0,  0,  1 )
                "u" -> FocusShift (  0,  0, -1 )
                " " -> ToggleBlock
                "s" -> ToggleStairs
                "b" -> ToggleBridge
                "a" -> PlaceStart
                "z" -> PlaceEnd
                _   -> Noop

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BE.onResize Resize
        , BE.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , BE.onVisibilityChange VisibilityChange
        , BE.onKeyDown (Decode.field "key" Decode.string |> Decode.map KeyDown)
        , BE.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
        , updateDpr DprUpdated
        ]


port updateDpr : (Float -> msg) -> Sub msg


-- View

menuLink : msg -> String -> String -> H.Html msg
menuLink action iconText tooltip =
    H.div [ HA.class "item" ]
        [ H.div [ HA.class "icon", HE.onClick action ]
            [ H.text iconText, H.span [ HA.class "tooltip" ] [ H.text <| " " ++ tooltip ] ]
        ]

view : Model -> Browser.Document Msg
view model =
    let
        alwaysWatch =
            [ HE.on "pointerdown" <| DD.decodePrimary Started
            , HE.on "pointerup" <| DD.decodePrimary Finished
            , HE.on "pointercancel" <| DD.decodePrimary Cancelled
            ]
        watchNow =
            if (model.mode == ME.Editing && model.orbiting) || model.mode == ME.Running then
                (HE.preventDefaultOn "pointermove" <| Decode.map (\m -> ( m, True )) (DD.decodePrimary Moved))
                    :: alwaysWatch
            else alwaysWatch
    in
    { title = "Iso Maze"
    , body =
        [ H.div [ HA.id "menu" ]
            [ menuLink Noop "#" "maze"
            , menuLink Noop "*" "settings"
            , menuLink Noop "?" "help"
            , menuLink ToggleDebug "~" "debug"
            ]
        , H.div
            (watchNow ++
                [ HA.id "three-container"
                , HA.style "width" "100%"
                , HA.style "height" "100vh"
                , HA.style "position" "relative"
                , HA.style "user-select" "none"
                , HA.style "touch-action" "none"
                ]
            )
            [ viewJoystick model ]
        , if model.debugInfo then
            H.div
                [ HA.style "position" "absolute"
                , HA.style "top" "10px"
                , HA.style "right" "10px"
                , HA.style "color" "white"
                , HA.style "background" "rgba(0, 0, 0, 0.4)"
                , HA.style "padding" "5px 10px"
                , HA.style "pointer-events" "none"
                , HA.style "font-family" "monospace"
                , HA.style "white-space" "pre"
                , HA.style "z-index" "10"
                ]
                [ H.text ("FT: " ++ formatMs (avgFrameTime model.frameHistory) ++ "ms\nDPR: " ++ String.fromFloat model.dpr) ]
          else
            H.text ""
        ]
    }

avgFrameTime : List { timestamp : Float, duration : Float } -> Float
avgFrameTime history =
    case history of
        [] ->
            0

        _ ->
            List.sum (List.map .duration history) / toFloat (List.length history)


formatMs : Float -> String
formatMs val =
    let
        rounded =
            toFloat (round (val * 10)) / 10

        s =
            String.fromFloat rounded
    in
    if String.contains "." s then
        s

    else
        s ++ ".0"


viewJoystick : Model -> H.Html Msg
viewJoystick model =
    case ( model.mode, model.pointerStart, model.pointerLast ) of
        ( ME.Running, Just start, Just last ) ->
            let
                dx = last.x - start.x
                dy = last.y - start.y
                dist = sqrt (dx * dx + dy * dy)
                maxDist = 40
                clampedDist = min dist maxDist
                angle = atan2 dy dx
                kx = clampedDist * cos angle
                ky = clampedDist * sin angle
            in
            H.div
                [ HA.style "position" "absolute"
                , HA.style "left" (String.fromFloat start.x ++ "px")
                , HA.style "top" (String.fromFloat start.y ++ "px")
                , HA.style "width" "0"
                , HA.style "height" "0"
                , HA.style "pointer-events" "none"
                , HA.style "z-index" "100"
                ]
                [ H.div
                    [ HA.style "position" "absolute"
                    , HA.style "left" "-40px"
                    , HA.style "top" "-40px"
                    , HA.style "width" "80px"
                    , HA.style "height" "80px"
                    , HA.style "border" "2px solid rgba(255,255,255,0.3)"
                    , HA.style "border-radius" "50%"
                    ]
                    []
                , H.div [ HA.style "position" "absolute", HA.style "left" "-30px", HA.style "top" "0", HA.style "width" "60px", HA.style "height" "2px", HA.style "background" "rgba(255,255,255,0.2)" ] []
                , H.div [ HA.style "position" "absolute", HA.style "left" "0", HA.style "top" "-30px", HA.style "width" "2px", HA.style "height" "60px", HA.style "background" "rgba(255,255,255,0.2)" ] []
                , H.div
                    [ HA.style "position" "absolute"
                    , HA.style "left" (String.fromFloat (kx - 15) ++ "px")
                    , HA.style "top" (String.fromFloat (ky - 15) ++ "px")
                    , HA.style "width" "30px"
                    , HA.style "height" "30px"
                    , HA.style "background" "rgba(255,255,255,0.5)"
                    , HA.style "border-radius" "50%"
                    ]
                    []
                ]

        _ ->
            H.text ""
