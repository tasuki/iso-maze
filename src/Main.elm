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

type PlayerState
    = Idle M.Position
    | Moving
        { from : M.Position
        , to : M.Position
        , dir : M.Direction
        , progress : Float
        }


type alias Vec3 =
    { x : Float, y : Float, z : Float }


type alias SphereState =
    { current : Vec3
    , velocity : Vec3
    }


type alias AnimatorState =
    { spheres : List SphereState
    , timer : Float
    , initialized : Bool
    }


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
    , playerState : PlayerState
    , animator : AnimatorState
    , focus : M.Position
    , keysDown : Set String
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
    | KeyDown String
    | KeyUp String

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
    let
        initialPos =
            M.startPosition defaultMaze

        initialTargets =
            getPlayerTargets (Idle initialPos) defaultMaze

        model =
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
            , playerState = Idle initialPos
            , animator = initAnimator initialTargets
            , focus = ( 0, 0, 1 )
            , keysDown = Set.empty
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
        ( newModel, cmd ) =
            updateModel message model

        targets =
            getPlayerTargets newModel.playerState newModel.maze

        isMoving =
            case newModel.playerState of
                Idle _ ->
                    isAnimatorMoving targets newModel.animator

                Moving _ ->
                    True

        shouldRender =
            case message of
                Resize _ _ ->
                    True

                Tick _ ->
                    isMoving

                Started _ ->
                    True

                Moved _ ->
                    newModel.mode == ME.Editing && newModel.orbiting

                Finished _ ->
                    True

                Cancelled _ ->
                    True

                _ ->
                    True

        sceneDataValue =
            D.sceneData
                { azimuth = newModel.azimuth
                , elevation = newModel.elevation
                , maze = newModel.maze
                , playerSpheres = List.map .current newModel.animator.spheres
                , focus = newModel.focus
                , mode = newModel.mode
                , widthPx = newModel.widthPx
                , heightPx = newModel.heightPx
                }
                60
    in
    if shouldRender then
        ( newModel
        , Cmd.batch [ cmd, D.renderThreeJS sceneDataValue ]
        )

    else
        ( newModel, cmd )


interpolatedPosition : PlayerState -> ( Float, Float, Float )
interpolatedPosition playerState =
    case playerState of
        Idle ( x, y, z ) ->
            ( toFloat x, toFloat y, toFloat z )

        Moving m ->
            let
                ( x1, y1, z1 ) =
                    m.from

                ( x2, y2, z2 ) =
                    m.to

                p =
                    m.progress

                lerp a b t =
                    toFloat a + (toFloat b - toFloat a) * t
            in
            ( lerp x1 x2 p, lerp y1 y2 p, lerp z1 z2 p )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel message model =
    case message of
        UrlChanged url ->
            ( changeRouteTo url model, Cmd.none )

        Resize width height ->
            ( { model | widthPx = width, heightPx = height }, Cmd.none )

        Tick elapsed ->
            let
                dt =
                    Duration.inSeconds elapsed

                newElapsedTime =
                    model.elapsedTime |> Quantity.plus elapsed

                newPlayerState =
                    if model.mode == ME.Running then
                        updatePlayerState dt model.keysDown model.pointerStart model.pointerLast model.maze model.playerState

                    else
                        model.playerState

                targets =
                    getPlayerTargets newPlayerState model.maze

                newAnimator =
                    updateAnimator dt targets model.animator
            in
            ( { model
                | elapsedTime = newElapsedTime
                , playerState = newPlayerState
                , animator = newAnimator
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
                        rotationRate =
                            Angle.degrees 0.5 |> Quantity.per Pixels.pixel

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

        Finished dc ->
            ( { model | pointerStart = Nothing, pointerLast = Nothing, orbiting = False }, Cmd.none )

        Cancelled _ ->
            ( { model | pointerStart = Nothing, pointerLast = Nothing, orbiting = False }, Cmd.none )

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
                pos = case model.playerState of
                    Idle p -> p
                    Moving m -> m.to
                newPos : M.Position
                newPos =
                    M.move pos dir model.maze
                        |> Maybe.withDefault pos
            in
            ( { model | playerState = Idle newPos }, Cmd.none )

        KeyDown key ->
            let
                newKeysDown = Set.insert key model.keysDown
                newModel = { model | keysDown = newKeysDown }
            in
            case (model.mode, key) of
                (_, "e") -> updateModel ToggleMode newModel
                (_, "c") -> updateModel CameraReset newModel
                (ME.Running, "ArrowLeft") -> (newModel, Cmd.none)
                (ME.Running, "ArrowDown") -> (newModel, Cmd.none)
                (ME.Running, "ArrowUp") -> (newModel, Cmd.none)
                (ME.Running, "ArrowRight") -> (newModel, Cmd.none)
                (ME.Editing, _) ->
                    let
                        msg = keydown model.mode key
                    in
                    if msg == Noop then (newModel, Cmd.none)
                    else updateModel msg newModel
                _ -> (newModel, Cmd.none)

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        _ ->
            ( model, Cmd.none )

updatePlayerState : Float -> Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> M.Maze -> PlayerState -> PlayerState
updatePlayerState dt keysDown pointerStart pointerLast maze playerState =
    let
        maybeMove pos progress =
            case getDesiredDirection keysDown pointerStart pointerLast of
                Just dir ->
                    case M.move pos dir maze of
                        Just nextTo ->
                            Moving { from = pos, to = nextTo, dir = dir, progress = progress }
                        Nothing ->
                            Idle pos
                Nothing ->
                    Idle pos
    in
    case playerState of
        Idle pos -> maybeMove pos 0
        Moving m ->
            let
                newProgress = m.progress + (dt / secondsPerStep)
            in
            if newProgress >= 1 then maybeMove m.to (newProgress - 1)
            else Moving { m | progress = newProgress }

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
                startPos =
                    M.startPosition maze

                targets =
                    getPlayerTargets (Idle startPos) maze
            in
            { model
                | maze = maze
                , playerState = Idle startPos
                , animator = initAnimator targets
            }

        Nothing ->
            model

updateMaze : (M.Position -> M.Maze -> M.Maze) -> Model -> ( Model, Cmd Msg )
updateMaze fun model =
    let newMaze = fun model.focus model.maze in
    ( { model | maze = newMaze }
    , pushUrl model.navKey newMaze
    )

pushUrl : Nav.Key -> M.Maze -> Cmd msg
pushUrl navKey maze =
    Nav.pushUrl navKey <| "?" ++ Codec.encode maze


-- Animator Logic

initAnimator : List Vec3 -> AnimatorState
initAnimator targets =
    let
        initSphere target =
            { current = { x = target.x, y = target.y, z = target.z + 10.0 }
            , velocity = { x = 0, y = 0, z = 0 }
            }
    in
    { spheres = List.map initSphere targets
    , timer = 0
    , initialized = True
    }


isAnimatorMoving : List Vec3 -> AnimatorState -> Bool
isAnimatorMoving targets state =
    let
        velThreshold =
            0.1

        posThreshold =
            0.01

        isSphereMoving target s =
            let
                dv2 =
                    s.velocity.x * s.velocity.x + s.velocity.y * s.velocity.y + s.velocity.z * s.velocity.z

                dp2 =
                    (s.current.x - target.x) * (s.current.x - target.x)
                        + (s.current.y - target.y) * (s.current.y - target.y)
                        + (s.current.z - target.z) * (s.current.z - target.z)
            in
            dv2 > velThreshold * velThreshold || dp2 > posThreshold * posThreshold
    in
    List.map2 isSphereMoving targets state.spheres |> List.any identity


updateAnimator : Float -> List Vec3 -> AnimatorState -> AnimatorState
updateAnimator totalDt targets state =
    let
        subSteps =
            5

        dt =
            min totalDt 0.2 / toFloat subSteps

        staggerDelay =
            0.05

        springK =
            400

        damping =
            30

        step currentSpheres currentTimer =
            let
                newTimer =
                    currentTimer + dt

                updateSphere i target s prevCurrent prevTarget =
                    if newTimer < toFloat i * staggerDelay then
                        s

                    else
                        let
                            targetPos =
                                if i == 0 then
                                    target

                                else
                                    let
                                        desiredOffset =
                                            { x = target.x - prevTarget.x
                                            , y = target.y - prevTarget.y
                                            , z = target.z - prevTarget.z
                                            }
                                    in
                                    { x = prevCurrent.x + desiredOffset.x
                                    , y = prevCurrent.y + desiredOffset.y
                                    , z = prevCurrent.z + desiredOffset.z
                                    }

                            diff =
                                { x = targetPos.x - s.current.x
                                , y = targetPos.y - s.current.y
                                , z = targetPos.z - s.current.z
                                }

                            force =
                                { x = diff.x * springK
                                , y = diff.y * springK
                                , z = diff.z * springK
                                }

                            friction =
                                { x = -s.velocity.x * damping
                                , y = -s.velocity.y * damping
                                , z = -s.velocity.z * damping
                                }

                            acceleration =
                                { x = force.x + friction.x
                                , y = force.y + friction.y
                                , z = force.z + friction.z
                                }

                            newVelocity =
                                { x = s.velocity.x + acceleration.x * dt
                                , y = s.velocity.y + acceleration.y * dt
                                , z = s.velocity.z + acceleration.z * dt
                                }

                            newCurrent =
                                { x = s.current.x + newVelocity.x * dt
                                , y = s.current.y + newVelocity.y * dt
                                , z = s.current.z + newVelocity.z * dt
                                }
                        in
                        { current = newCurrent, velocity = newVelocity }

                -- We need to update spheres one by one because each depends on the NEW current of the previous one
                folder ( i, target, s ) ( accSpheres, lastPrevCurrent, lastPrevTarget ) =
                    let
                        newS =
                            updateSphere i target s lastPrevCurrent lastPrevTarget
                    in
                    ( accSpheres ++ [ newS ], newS.current, target )

                ( nextSpheres, _, _ ) =
                    List.foldl folder ( [], { x = 0, y = 0, z = 0 }, { x = 0, y = 0, z = 0 } ) (List.map3 (\i t s -> ( i, t, s )) (List.range 0 (List.length currentSpheres - 1)) targets currentSpheres)
            in
            ( nextSpheres, newTimer )

        runSubSteps n s t =
            if n <= 0 then
                { state | spheres = s, timer = t }

            else
                let
                    ( nextS, nextT ) =
                        step s t
                in
                runSubSteps (n - 1) nextS nextT
    in
    runSubSteps subSteps state.spheres state.timer


getPlayerTargets : PlayerState -> M.Maze -> List Vec3
getPlayerTargets playerState maze =
    let
        ( x, y, z ) =
            interpolatedPosition playerState

        playerPos_ ( px, py, pz ) zOffset =
            let
                getFix ( ix, iy ) =
                    case M.get ( ix, iy ) maze of
                        Just (M.Stairs _ _) ->
                            -5

                        _ ->
                            0

                x1 =
                    floor px

                x2 =
                    ceiling px

                y1 =
                    floor py

                y2 =
                    ceiling py

                fx =
                    px - toFloat x1

                fy =
                    py - toFloat y1

                fix11 =
                    getFix ( x1, y1 )

                fix12 =
                    getFix ( x1, y2 )

                fix21 =
                    getFix ( x2, y1 )

                fix22 =
                    getFix ( x2, y2 )

                fix =
                    if x1 == x2 && y1 == y2 then
                        toFloat fix11

                    else if x1 == x2 then
                        toFloat fix11 * (1 - fy) + toFloat fix12 * fy

                    else if y1 == y2 then
                        toFloat fix11 * (1 - fx) + toFloat fix21 * fx

                    else
                        (toFloat fix11 * (1 - fx) + toFloat fix21 * fx) * (1 - fy)
                            + (toFloat fix12 * (1 - fx) + toFloat fix22 * fx) * fy
            in
            { x = px * 10
            , y = py * 10
            , z = pz * 10 + zOffset + fix
            }

        playerSphere zOffset =
            playerPos_ ( x, y, z ) zOffset
    in
    [ playerSphere 2.0
    , playerSphere 5.5
    , playerSphere 8.5
    ]


-- Subscriptions

keydown : ME.Mode -> String -> Msg
keydown mode keycode =
    case mode of
        ME.Running -> Noop
        ME.Editing ->
            case keycode of
                "e" -> ToggleMode
                "c" -> CameraReset
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
subscriptions model =
    Sub.batch
        [ BE.onResize Resize
        , BE.onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , BE.onVisibilityChange VisibilityChange
        , BE.onKeyDown (Decode.field "key" Decode.string |> Decode.map KeyDown)
        , BE.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp)
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
            if (model.mode == ME.Editing && model.orbiting) || model.mode == ME.Running then
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
                , HA.style "position" "relative"
                , HA.style "user-select" "none"
                , HA.style "touch-action" "none"
                ]
            )
            [ viewJoystick model ]
        ]
    }

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
