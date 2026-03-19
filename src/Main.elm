port module Main exposing (main)

import Angle exposing (Angle)
import Animate
import Browser
import Browser.Dom
import Browser.Events as BE
import Browser.Navigation as Nav
import Campaign
import Codec
import Controls
import DocumentDecoders as DD
import DrawThree as D
import Duration exposing (Duration)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Analyzer
import Json.Decode as Decode
import Maybe.Extra
import Maze as M
import MazeEdit as ME
import Pixels
import Quantity
import SampleMazes as SM
import Set exposing (Set)
import Task
import Url exposing (Url)
import Url.Parser as UP exposing ((</>))

defaultMaze = SM.ziggurat2
secondsPerStep = 0.25
leashDistance = 80.0


type Overlay
    = Help
    | Settings
    | Campaign
    | LevelComplete String

type DebugLevel
    = DebugOff
    | DebugTechnical
    | DebugAnalysis

type Performance
    = Potato
    | Normal
    | Rocket

performanceToString : Performance -> String
performanceToString p =
    case p of
        Potato -> "potato"
        Normal -> "normal"
        Rocket -> "rocket"

performanceFromString : String -> Performance
performanceFromString s =
    case s of
        "potato" -> Potato
        "rocket" -> Rocket
        _ -> Normal

type alias RenderUpdate =
    { duration : Float
    , staticMeshes : Int
    , dynamicMeshes : Int
    , staticDrawCalls : Int
    , staticTriangles : Int
    , dynamicDrawCalls : Int
    , dynamicTriangles : Int
    , geometries : Int
    , textures : Int
    }

type alias Model =
    { navKey : Nav.Key
    , finishedLevels : Set String
    , currentLevel : Maybe Campaign.Level
    , widthPx : Int
    , heightPx : Int
    , elapsedTime : Duration
    , pointerStart : Maybe DD.DocumentCoords
    , pointerLast : Maybe DD.DocumentCoords
    , keysDown : Set String
    , debugLevel : DebugLevel
    , orbiting : Bool
    , elevation : Angle
    , azimuth : Angle
    , mode : ME.Mode
    , maze : M.Maze
    , playerState : M.PlayerState
    , animator : Animate.AnimatorState
    , focus : M.Position
    , dpr : Float
    , renderHistory : List { timestamp : Float, duration : Float }
    , tickHistory : List { timestamp : Float, duration : Float }
    , lastRenderStats : Maybe RenderUpdate
    , staticUpdate : Bool
    , activeOverlay : Maybe Overlay
    , performance : Performance
    , leashEnabled : Bool
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
    | ToggleGreenery
    | ClearTile
    | PlaceStart
    | PlaceEnd
    | SetDebugLevel DebugLevel
    | CycleDebug
    | SetPerformance Performance
    | SetLeashEnabled Bool
    | ResetProgress
    | ShowOverlay Overlay
    | CloseOverlay
    | DprUpdated Float
    | RenderTimeUpdated RenderUpdate
    | GotCompletionResponse (Result Http.Error ())

type alias Flags =
    { dpr : Float
    , finishedLevels : List String
    , performance : String
    , leashEnabled : Bool
    }

main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }

init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        initialPos = M.startPosition defaultMaze
        initialTargets = Animate.getPlayerTargets (M.Idle initialPos) defaultMaze

        model =
            { navKey = navKey
            , finishedLevels = Set.fromList flags.finishedLevels
            , currentLevel = Nothing
            , widthPx = 0
            , heightPx = 0
            , elapsedTime = Quantity.zero
            , pointerStart = Nothing
            , pointerLast = Nothing
            , keysDown = Set.empty
            , debugLevel = DebugOff
            , orbiting = False
            , elevation = Angle.degrees D.initialElevation
            , azimuth = Angle.degrees D.initialAzimuth
            , mode = ME.Running
            , maze = defaultMaze
            , playerState = M.Idle ( 999, 999, 999 )
            , animator = Animate.initAnimator initialTargets
            , focus = M.snapFocus ( 0, 0, 1 ) defaultMaze
            , dpr = flags.dpr
            , renderHistory = []
            , tickHistory = []
            , lastRenderStats = Nothing
            , staticUpdate = True
            , activeOverlay = Nothing
            , performance = performanceFromString flags.performance
            , leashEnabled = flags.leashEnabled
            }

        ( routedModel, routeCmd ) = changeRouteTo url model
    in
    ( routedModel
    , Cmd.batch
        [ routeCmd
        , Task.perform
            (\{ viewport } -> Resize
                (round viewport.width)
                (round viewport.height)
            )
            Browser.Dom.getViewport
        ]
    )


-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        ( preModel, cmd ) = updateModel message model
        targets = Animate.getPlayerTargets preModel.playerState preModel.maze
        wasMoving =
            case model.playerState of
                M.Idle _ -> Animate.isAnimatorMoving (Animate.getPlayerTargets model.playerState model.maze) model.animator
                M.Moving _ -> True

        isMoving =
            case preModel.playerState of
                M.Idle _ -> Animate.isAnimatorMoving targets preModel.animator
                M.Moving _ -> True

        shouldRender =
            preModel.staticUpdate ||
            case message of
                Resize _ _ -> True
                Tick _ -> isMoving || wasMoving || (preModel.mode == ME.Running && preModel.pointerStart /= Nothing)
                Started _ -> preModel.mode == ME.Running
                Moved _ -> (preModel.mode == ME.Editing && preModel.orbiting) || preModel.mode == ME.Running
                Finished _ -> model.orbiting || (model.mode == ME.Running && model.pointerStart /= Nothing)
                KeyDown key ->
                    if preModel.mode == ME.Editing then True
                    else key == "e" || key == "c"
                UrlChanged _ -> True
                _ -> False
    in
    if shouldRender then
        let
            ( s1, s2, s3 ) = preModel.animator.spheres
            sceneDataValue =
                D.sceneData
                    { azimuth = preModel.azimuth
                    , elevation = preModel.elevation
                    , maze = preModel.maze
                    , playerState = preModel.playerState
                    , playerSpheres = ( s1.current, s2.current, s3.current )
                    , animatorTimer = preModel.animator.timer
                    , animatorInitialFall = preModel.animator.initialFall
                    , focus = preModel.focus
                    , mode = preModel.mode
                    , widthPx = preModel.widthPx
                    , heightPx = preModel.heightPx
                    , staticUpdate = preModel.staticUpdate
                    , performance = performanceToString preModel.performance
                    , analysis =
                        if preModel.debugLevel == DebugAnalysis then
                            Just (Analyzer.analyze preModel.maze)
                        else
                            Nothing
                    , joystick =
                        case ( preModel.mode, preModel.pointerStart, preModel.pointerLast ) of
                            ( ME.Running, Just start, Just last ) ->
                                Just { dx = last.x - start.x, dy = last.y - start.y }

                            _ ->
                                Nothing
                    }
        in
        ( { preModel | staticUpdate = False }
        , Cmd.batch [ cmd, D.renderThreeJS sceneDataValue ]
        )
    else ( preModel, cmd )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changeRouteTo url model

        Resize width height ->
            ( { model | widthPx = width, heightPx = height, staticUpdate = True }, Cmd.none )

        Tick elapsed ->
            let
                dt = Duration.inSeconds elapsed
                dtMs = Duration.inMilliseconds elapsed
                newElapsedTime = model.elapsedTime |> Quantity.plus elapsed
                newPlayerState =
                    if model.mode == ME.Running && model.activeOverlay == Nothing then
                        updatePlayerState dt model.keysDown model.pointerStart model.pointerLast model.maze model.playerState
                    else
                        model.playerState

                maybeFinishedLevel =
                    case newPlayerState of
                        M.Idle pos ->
                            if pos == M.endPosition model.maze then
                                Just (model.currentLevel |> Maybe.map .name |> Maybe.withDefault "")
                            else Nothing
                        _ -> Nothing

                (newFinishedLevels, saveCmd) =
                    case (model.activeOverlay, maybeFinishedLevel) of
                        (Nothing, Just name) ->
                            if name /= "" then
                                let updated = Set.insert name model.finishedLevels in
                                ( updated, saveFinishedLevels (Set.toList updated) )
                            else ( model.finishedLevels, Cmd.none )
                        _ ->
                            ( model.finishedLevels, Cmd.none )

                completionCmd =
                    case (model.activeOverlay, maybeFinishedLevel) of
                        (Nothing, Just name) ->
                            Http.post
                                { url = "/completed/" ++
                                    if name /= "" then name
                                    else Codec.encode model.maze
                                , body = Http.emptyBody
                                , expect = Http.expectWhatever GotCompletionResponse
                                }
                        _ -> Cmd.none

                targets = Animate.getPlayerTargets newPlayerState model.maze
                newAnimator = Animate.updateAnimator dt targets model.animator

                currentTime = Duration.inMilliseconds newElapsedTime
                newTickHistory =
                    { timestamp = currentTime, duration = dtMs }
                        :: model.tickHistory
                        |> List.filter (\f -> currentTime - f.timestamp < 1000)
            in
            ( { model
                | elapsedTime = newElapsedTime
                , playerState = newPlayerState
                , animator = newAnimator
                , tickHistory = newTickHistory
                , finishedLevels = newFinishedLevels
                , activeOverlay =
                    case maybeFinishedLevel of
                        Just name -> Just (LevelComplete name)
                        Nothing -> model.activeOverlay
              }
            , Cmd.batch [ saveCmd, completionCmd ]
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
            let
                ( updatedModel, cmd ) =
                    if model.mode == ME.Running && model.activeOverlay == Nothing then
                        ( applyLeash dc model, Cmd.none )
                    else if model.orbiting && model.activeOverlay == Nothing then
                        ( applyOrbit dc model, Cmd.none )
                    else
                        ( model, Cmd.none )
            in
            ( { updatedModel | pointerLast = Just dc }, cmd )

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
                , staticUpdate = True
              }
            , Cmd.none
            )

        FocusShift vector ->
            let newFocus = M.shiftPosition model.focus vector in
            if M.isFocusValid (M.positionTo2d newFocus) model.maze then
                ( { model | focus = newFocus, staticUpdate = True }, Cmd.none )
            else
                ( model, Cmd.none )

        ToggleMode ->
            let
                newMode = if model.mode == ME.Running
                    then ME.Editing
                    else ME.Running

                cmd = if newMode == ME.Editing
                    then pushUrl model.navKey model.maze
                    else Cmd.none

                newFocus = if newMode == ME.Editing
                    then M.snapFocus model.focus model.maze
                    else model.focus
            in
            ( { model | mode = newMode, focus = newFocus, staticUpdate = True }, cmd )

        ToggleBlock -> updateMaze ME.toggleBlock { model | currentLevel = Nothing }
        ToggleStairs -> updateMaze ME.toggleStairs { model | currentLevel = Nothing }
        ToggleBridge -> updateMaze ME.toggleBridge { model | currentLevel = Nothing }
        ToggleGreenery -> updateMaze ME.toggleGreenery { model | currentLevel = Nothing }
        ClearTile -> updateMaze ME.clearTile { model | currentLevel = Nothing }
        PlaceStart -> updateMaze ME.placeStart { model | currentLevel = Nothing }
        PlaceEnd -> updateMaze ME.placeEnd { model | currentLevel = Nothing }

        KeyDown key ->
            case model.activeOverlay of
                Just _ ->
                    if key == "Escape" then
                        ( { model | activeOverlay = Nothing, keysDown = Set.empty }, Cmd.none )
                    else
                        ( model, Cmd.none )

                Nothing ->
                    let
                        newKeysDown = Set.insert key model.keysDown
                        newModel = { model | keysDown = newKeysDown }
                    in
                    case (model.mode, key) of
                        (_, "e") -> updateModel ToggleMode newModel
                        (_, "c") -> updateModel CameraReset newModel
                        (_, "`") -> updateModel CycleDebug newModel
                        (ME.Editing, _) ->
                            let
                                msg = keydown model.mode key
                            in
                            if msg == Noop then (newModel, Cmd.none)
                            else updateModel msg newModel
                        _ -> (newModel, Cmd.none)

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        SetDebugLevel level ->
            ( { model | debugLevel = level, staticUpdate = True }, Cmd.none )

        CycleDebug ->
            let
                newLevel =
                    case model.debugLevel of
                        DebugOff -> DebugTechnical
                        DebugTechnical -> DebugAnalysis
                        DebugAnalysis -> DebugOff
            in
            ( { model | debugLevel = newLevel, staticUpdate = True }, Cmd.none )

        SetPerformance perf ->
            ( { model | performance = perf, staticUpdate = True }, savePerformance (performanceToString perf) )

        SetLeashEnabled enabled ->
            ( { model | leashEnabled = enabled }, saveLeashEnabled enabled )

        ResetProgress ->
            ( { model | finishedLevels = Set.empty }, saveFinishedLevels [] )

        ShowOverlay overlay ->
            if model.activeOverlay == Just overlay then
                ( { model | activeOverlay = Nothing }, Cmd.none )
            else
                ( { model | activeOverlay = Just overlay, orbiting = False, pointerStart = Nothing, keysDown = Set.empty }, Cmd.none )

        CloseOverlay ->
            case model.activeOverlay of
                Just (LevelComplete _) ->
                    -- make LevelComplete overlay uncloseable
                    ( model, Cmd.none )
                _ ->
                    ( { model | activeOverlay = Nothing }, Cmd.none )

        DprUpdated dpr ->
            ( { model | dpr = dpr }, Cmd.none )

        RenderTimeUpdated renderUpdate ->
            let
                currentTime = Duration.inMilliseconds model.elapsedTime
                newEntry = { timestamp = currentTime, duration = renderUpdate.duration }
                withinWindow =
                    newEntry :: model.renderHistory
                        |> List.filter (\r -> currentTime - r.timestamp < 1000)
                finalHistory =
                    if List.length withinWindow < 2 then
                        List.take 2 (newEntry :: model.renderHistory)
                    else
                        withinWindow
            in
            ( { model | renderHistory = finalHistory, lastRenderStats = Just renderUpdate }, Cmd.none )

        _ ->
            ( model, Cmd.none )

updatePlayerState : Float -> Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> M.Maze -> M.PlayerState -> M.PlayerState
updatePlayerState dt keysDown pointerStart pointerLast maze playerState =
    let
        maybeIntent = Controls.getIntent keysDown pointerStart pointerLast
        maybeMove pos progress =
            case maybeIntent of
                Just ((Controls.Intent _ s) as intent) ->
                    case Controls.resolveIntent pos intent maze of
                        Just ( dir, nextTo ) ->
                            M.Moving { from = pos, to = nextTo, dir = dir, progress = progress, speedFactor = s }
                        Nothing ->
                            M.Idle pos
                Nothing ->
                    M.Idle pos

        speedFactor =
            case (playerState, maybeIntent) of
                (_, Just (Controls.Intent _ s)) -> s
                (M.Moving m, Nothing) -> m.speedFactor
                _ -> 1.0
    in
    case playerState of
        M.Idle pos -> maybeMove pos 0
        M.Moving m ->
            let
                oldMaxProgress = if m.to == M.endPosition maze then 4.0 else 1.0
                tryReverse ( revDir, revTo ) =
                    if revTo == m.from && m.progress < oldMaxProgress then
                        { from = m.to, to = m.from, dir = revDir, progress = max 0 (1.0 - m.progress), speedFactor = speedFactor }
                    else
                        m

                activeM = maybeIntent
                    |> Maybe.andThen (\intent -> Controls.resolveIntent m.to intent maze)
                    |> Maybe.map tryReverse
                    |> Maybe.withDefault m
                activeMaxProgress = if activeM.to == M.endPosition maze then 4.0 else 1.0
                newProgress = activeM.progress + (dt * speedFactor / secondsPerStep)
            in
            if newProgress >= activeMaxProgress then maybeMove activeM.to (newProgress - activeMaxProgress)
            else M.Moving { activeM | progress = newProgress, speedFactor = speedFactor }

type Route
    = Home (Maybe M.Maze)
    | Level String

routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Level (UP.s "level" </> UP.string)
        ]

changeRouteTo : Url.Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        route = UP.parse routeParser url |> Maybe.withDefault (Home (Maybe.andThen Codec.decode url.query))
    in
    case route of
        Home maybeMaze ->
            if url.path == "/" && url.query == Nothing then
                case Campaign.getNextUnsolvedLevel model.finishedLevels of
                    Just nextLevelName ->
                        ( model, Nav.replaceUrl model.navKey ("/level/" ++ nextLevelName) )
                    Nothing ->
                        ( loadMaze (maybeMaze |> Maybe.withDefault defaultMaze) Nothing model, Cmd.none )
            else
                ( loadMaze (maybeMaze |> Maybe.withDefault defaultMaze) Nothing model, Cmd.none )

        Level name ->
            case List.filter (\m -> m.name == name) Campaign.mazeDefs |> List.head of
                Just def ->
                    ( loadMaze (Codec.decode def.maze |> Maybe.withDefault M.emptyMaze) (Just name) model, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )


loadMaze : M.Maze -> Maybe String -> Model -> Model
loadMaze maze maybeName model =
    let
        startPos = M.startPosition maze
        targets = Animate.getPlayerTargets (M.Idle startPos) maze
        ( oldX, oldY ) = case model.playerState of
            M.Idle ( x, y, _ ) -> ( x, y )
            M.Moving m ->
                let ( x, y, _ ) = m.from in ( x, y )
        ( newX, newY, _ ) = startPos
        shouldFall = oldX /= newX || oldY /= newY || Maybe.Extra.isJust maybeName
        newAnimator =
            if shouldFall then Animate.initAnimator targets
            else Animate.initAnimatorAt targets
    in
    { model
        | maze = maze
        , playerState = M.Idle startPos
        , animator = newAnimator
        , staticUpdate = True
        , currentLevel = maybeName |> Maybe.andThen Campaign.getLevel
        , activeOverlay = Nothing
    }

applyLeash : DD.DocumentCoords -> Model -> Model
applyLeash dc model =
    case ( model.leashEnabled, model.pointerStart ) of
        ( True, Just start ) ->
            let
                dx = dc.x - start.x
                dy = dc.y - start.y
                dist = sqrt (dx * dx + dy * dy)
            in
            if dist > leashDistance then
                let
                    angle = atan2 dy dx
                    newStart = { x = dc.x - leashDistance * cos angle, y = dc.y - leashDistance * sin angle }
                in
                { model | pointerStart = Just newStart }
            else model
        _ ->
            model

applyOrbit : DD.DocumentCoords -> Model -> Model
applyOrbit dc model =
    case model.pointerLast of
        Just lastDc ->
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
            { model | azimuth = newAzimuth, elevation = newElevation, staticUpdate = True }
        Nothing ->
            model

updateMaze : (M.Position -> M.Maze -> M.Maze) -> Model -> ( Model, Cmd Msg )
updateMaze fun model =
    let
        newMazeRaw = fun model.focus model.maze
        ( newMaze, ( dx, dy ) ) = M.normalize newMazeRaw

        shiftSpheres ( s1, s2, s3 ) =
            let
                shiftS s =
                    { s
                    | current = { x = s.current.x + toFloat dx * 10, y = s.current.y + toFloat dy * 10, z = s.current.z }
                    , velocity = s.velocity
                    }
            in
            ( shiftS s1, shiftS s2, shiftS s3 )

        newPlayerState =
            case model.playerState of
                M.Idle ( x, y, z ) -> M.Idle ( x + dx, y + dy, z )
                M.Moving m ->
                    let
                        ( fx, fy, fz ) = m.from
                        ( tx, ty, tz ) = m.to
                    in
                    M.Moving { m | from = ( fx + dx, fy + dy, fz ), to = ( tx + dx, ty + dy, tz ) }

        newAnimator = model.animator
        updatedAnimator = { newAnimator | spheres = shiftSpheres newAnimator.spheres }
    in
    ( { model
        | maze = newMaze
        , focus = M.snapFocus (M.shiftPosition model.focus ( dx, dy, 0 )) newMaze
        , playerState = newPlayerState
        , animator = updatedAnimator
        , staticUpdate = True
      }
    , pushUrl model.navKey newMaze
    )

pushUrl : Nav.Key -> M.Maze -> Cmd msg
pushUrl navKey maze =
    Nav.pushUrl navKey <| "/?" ++ Codec.encode maze


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
                "g" -> ToggleGreenery
                "q" -> ClearTile
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
        , updateRenderTime RenderTimeUpdated
        ]


port updateDpr : (Float -> msg) -> Sub msg
port updateRenderTime : (RenderUpdate -> msg) -> Sub msg
port saveFinishedLevels : List String -> Cmd msg
port savePerformance : String -> Cmd msg
port saveLeashEnabled : Bool -> Cmd msg


-- View

menuLink : msg -> String -> H.Html msg
menuLink action iconText =
    H.div [ HA.class "item" ]
        [ H.div [ HA.class "icon overlay", HE.onClick action ] [ H.text iconText ]
        ]

viewOverlay : Model -> Overlay -> H.Html Msg
viewOverlay model overlay =
    let
        content =
            case overlay of
                Help ->
                    [ H.text helpText ]

                Settings ->
                    [ H.div [ HA.class "modal-row" ]
                        [ H.div [ HA.class "icon", HE.onClick ResetProgress ] [ H.text "⚠️⏮️⚠️" ]
                        ]
                    , H.div [ HA.class "modal-row" ]
                        [ H.div
                            [ HA.class ("icon" ++ if model.performance == Potato then " active" else "")
                            , HE.onClick (SetPerformance Potato)
                            ]
                            [ H.text "🥔" ]
                        , H.div
                            [ HA.class ("icon" ++ if model.performance == Normal then " active" else "")
                            , HE.onClick (SetPerformance Normal)
                            ]
                            [ H.text "💻" ]
                        , H.div
                            [ HA.class ("icon" ++ if model.performance == Rocket then " active" else "")
                            , HE.onClick (SetPerformance Rocket)
                            ]
                            [ H.text "🚀" ]
                        ]
                    , H.div [ HA.class "modal-row" ]
                        [ H.div
                            [ HA.class ("icon" ++ if not model.leashEnabled then " active" else "")
                            , HE.onClick (SetLeashEnabled False)
                            ]
                            [ H.text "🎯📌" ]
                        , H.div
                            [ HA.class ("icon" ++ if model.leashEnabled then " active" else "")
                            , HE.onClick (SetLeashEnabled True)
                            ]
                            [ H.text "🎯🏃" ]
                        ]
                    , H.div [ HA.class "modal-row" ]
                        [ H.div
                            [ HA.class ("icon" ++ if model.debugLevel == DebugOff then " active" else "")
                            , HE.onClick (SetDebugLevel DebugOff)
                            ]
                            [ H.text "🧪❌" ]
                        , H.div
                            [ HA.class ("icon" ++ if model.debugLevel /= DebugOff then " active" else "")
                            , HE.onClick (SetDebugLevel DebugTechnical)
                            ]
                            [ H.text "🧪✔️" ]
                        ]
                    ]

                Campaign ->
                    [ H.div [ HA.class "campaign-grid" ]
                        (Campaign.levels model.finishedLevels |> List.map viewCampaignLevel)
                    ]

                LevelComplete name ->
                    [ H.div [ HA.class "modal-row center" ] [ H.text "🏆👑😎" ]
                    , H.div [ HA.class "modal-row" ]
                        [ H.a [ HA.class "icon", HA.href ("/level/" ++ name) ] [ H.text "🔄" ]
                        , case Campaign.getNextUnsolvedLevel model.finishedLevels of
                            Just nextName ->
                                H.a [ HA.class "icon", HA.href ("/level/" ++ nextName) ] [ H.text "🚀" ]
                            Nothing ->
                                H.a [ HA.class "icon", HE.onClick (ShowOverlay Campaign) ] [ H.text "🚀" ]
                        ]
                    ]
    in
    H.div [ HA.class "modal-backdrop" ]
        [ H.div [ HA.class "modal-dimmer", HE.onClick CloseOverlay ] []
        , H.div [ HA.class "modal-content overlay" ] content ]

viewCampaignLevel : Campaign.Level -> H.Html Msg
viewCampaignLevel level =
    H.a
        [ HA.class ("campaign-level" ++ (if level.finished then " finished" else ""))
        , HA.href ("/level/" ++ level.name)
        ]
        [ H.text level.emoji ]

helpText : String
helpText =
    """👋 ⛄🔎🎩

1️⃣ 🌬️⛄
2️⃣ 💨🎩🪽
3️⃣ ⛄❓🎩
4️⃣ 🧭⛄🕹️
  ✳️ 📱👆↕️↔️
  ✳️ 💻🖱️↕️↔️
  ✳️ 💻⌨️🔼🔽◀️▶️
5️⃣ ⛄➡️🎩  🏆👑😎"""

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
        ( menuEmoji, title ) = case model.currentLevel of
            Just l -> ( l.emoji, l.emoji ++ " " ++ l.name ++ " – maze" )
            Nothing -> ( "🚀", "🚀 – maze" )
    in
    { title = title
    , body =
        [ H.div [ HA.id "menu" ]
            [ menuLink (ShowOverlay Campaign) menuEmoji
            , menuLink (ShowOverlay Settings) "🔧"
            , menuLink (ShowOverlay Help) "💡"
            ]
        , H.div (HA.id "three-container" :: watchNow)
            [ viewJoystick model ]
        , case model.activeOverlay of
            Just overlay -> viewOverlay model overlay
            Nothing -> H.text ""
        , case model.debugLevel of
            DebugOff ->
                H.text ""

            DebugTechnical ->
                H.div [ HA.id "debug-info", HA.class "overlay" ]
                    [ H.text ("FPS: " ++ formatMs (fpsFromPeriod (avgDuration model.tickHistory)) ++ "\nFT: " ++ formatMs (avgDuration model.tickHistory) ++ "ms\nRT: " ++ formatMs (avgDuration model.renderHistory) ++ "ms\nDPR: " ++ String.fromFloat model.dpr)
                    , case model.lastRenderStats of
                        Just stats ->
                            H.div [ HA.style "font-size" "10px" ]
                                [ H.br [] []
                                , H.text ("Static Meshes: " ++ String.fromInt stats.staticMeshes)
                                , H.text ("\nDynamic Meshes: " ++ String.fromInt stats.dynamicMeshes)
                                , H.text ("\nStatic DC: " ++ String.fromInt stats.staticDrawCalls ++ " (" ++ String.fromInt stats.staticTriangles ++ " tris)")
                                , H.text ("\nDynamic DC: " ++ String.fromInt stats.dynamicDrawCalls ++ " (" ++ String.fromInt stats.dynamicTriangles ++ " tris)")
                                , H.text ("\nGeometries: " ++ String.fromInt stats.geometries)
                                , H.text ("\nTextures: " ++ String.fromInt stats.textures)
                                ]

                        Nothing ->
                            H.text ""
                    ]

            DebugAnalysis ->
                viewAnalyzer (Analyzer.analyze model.maze)
        ]
    }

fpsFromPeriod : Float -> Float
fpsFromPeriod ms =
    if ms <= 0 then 0 else 1000 / ms

avgDuration : List { timestamp : Float, duration : Float } -> Float
avgDuration history =
    case history of
        [] -> 0
        _ -> List.sum (List.map .duration history) / toFloat (List.length history)

formatMs : Float -> String
formatMs val =
    let
        rounded = toFloat (round (val * 10)) / 10
        s = String.fromFloat rounded
    in
    if String.contains "." s then s
    else s ++ ".0"

viewAnalyzer : Analyzer.Analysis -> H.Html Msg
viewAnalyzer a =
    H.div [ HA.id "debug-info", HA.class "overlay" ]
        [ H.text ("--- ANALYSIS ---")
        , H.br [] []
        , H.text ("Reachable: " ++ (if a.reachable then "✔️" else "❌"))
        , H.br [] []
        , H.text ("Occluding: " ++ String.fromInt (Set.size a.occluding))
        , H.br [] []
        , H.text ("Unreachable: " ++ String.fromInt (Set.size a.unreachable))
        , H.br [] []
        , H.text ("Hanging: " ++ String.fromInt (Set.size a.hanging))
        , H.br [] []
        , H.text ("Total Cells: " ++ String.fromInt a.totalCells)
        , H.br [] []
        , H.text ("Shortest Path: " ++ (a.shortestPathLength |> Maybe.map String.fromInt |> Maybe.withDefault "N/A"))
        , H.br [] []
        , H.text ("Sol. Density: " ++ formatFloat a.solutionDensity)
        , H.br [] []
        , H.text ("Stairs: " ++ formatFloat a.stairsProportion)
        , H.br [] []
        , H.text ("Bridges: " ++ formatFloat a.bridgesProportion)
        , H.br [] []
        , H.text ("River Factor: " ++ formatFloat a.riverFactor)
        , H.br [] []
        , H.text ("Loop Count: " ++ String.fromInt a.loopCount)
        , H.br [] []
        , H.text ("Greenery: " ++ String.fromInt a.greenery)
        , H.br [] []
        , H.text ("Holes: " ++ String.fromInt a.holes)
        , H.br [] []
        , H.text ("Squares: " ++ String.fromInt a.squares)
        ]


formatFloat : Float -> String
formatFloat val =
    let
        rounded = toFloat (round (val * 100)) / 100
        s = String.fromFloat rounded
    in
    if String.contains "." s then s
    else s ++ ".0"


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
                [ HA.class "joystick-container"
                , HA.style "left" (String.fromFloat start.x ++ "px")
                , HA.style "top" (String.fromFloat start.y ++ "px")
                ]
                [ H.div
                    [ HA.classList
                        [ ( "joystick-base", True )
                        , ( "too-far", dist > (leashDistance - 5) )
                        ]
                    ] []
                , H.div [ HA.class "joystick-crosshair-1" ] []
                , H.div [ HA.class "joystick-crosshair-2" ] []
                , H.div
                    [ HA.class "joystick-knob"
                    , HA.style "left" (String.fromFloat (kx - 15) ++ "px")
                    , HA.style "top" (String.fromFloat (ky - 15) ++ "px")
                    ]
                    []
                ]

        _ ->
            H.text ""
