module Controls exposing (..)

import Angle exposing (Angle)
import DocumentDecoders as DD
import Duration exposing (Duration)
import Maybe.Extra
import Maze as M
import Pixels
import Quantity
import Set exposing (Set)


-- CONSTANTS

secondsPerStep = 0.2
joystickDeadzone = 20.0
joystickMaxDist = 80.0
leashDistance = 80.0


-- HIGH-LEVEL CONTROLS

applyLeash : Bool -> Maybe DD.DocumentCoords -> DD.DocumentCoords -> Maybe DD.DocumentCoords
applyLeash leashEnabled pointerStart dc =
    case ( leashEnabled, pointerStart ) of
        ( True, Just start ) ->
            let
                dx = dc.x - start.x
                dy = dc.y - start.y
                dist = sqrt (dx*dx + dy*dy)
            in
            if dist > leashDistance then
                let angle = atan2 dy dx in
                Just { x = dc.x - leashDistance * cos angle, y = dc.y - leashDistance * sin angle }
            else Nothing
        _ ->
            Nothing

applyOrbit : Angle -> Angle -> Maybe DD.DocumentCoords -> DD.DocumentCoords -> Maybe (Angle, Angle)
applyOrbit azimuth elevation pointerLast dc =
    pointerLast |> Maybe.map (\lastDc ->
        let
            rotationRate = Angle.degrees 0.5 |> Quantity.per Pixels.pixel
            newAzimuth =
                azimuth
                    |> Quantity.minus (dc.x - lastDc.x |> Pixels.pixels |> Quantity.at rotationRate)
            newElevation =
                elevation
                    |> Quantity.plus (dc.y - lastDc.y |> Pixels.pixels |> Quantity.at rotationRate)
                    |> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
        in
        (newAzimuth, newElevation)
    )


-- INTENT ANALYSIS

type alias IntentInfo =
    { intent : Maybe M.MovementIntent
    , primaryDir : Maybe M.Direction
    , secondaryDir : Maybe M.Direction
    , primarySpeed : Float
    , secondarySpeed : Float
    , isLong : Bool
    , shouldStop : Bool
    , interactionStart : Maybe Duration
    }

analyzeIntent : Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe Duration -> Duration -> IntentInfo
analyzeIntent keysDown pointerStart pointerLast interactionStart currentTime =
    let
        maybeIntent = getIntent keysDown pointerStart pointerLast
        intentDuration = interactionStart
            |> Maybe.map (\i -> currentTime |> Duration.inSeconds |> (\curr -> curr - (Duration.inSeconds i)))
            |> Maybe.withDefault 0.0

        intentSummary =
            case maybeIntent of
                Just (M.Intent axes) ->
                    let
                        ne = if axes.ne_sw > 0 then axes.ne_sw else 0
                        sw = if axes.ne_sw < 0 then -axes.ne_sw else 0
                        se = if axes.nw_se > 0 then axes.nw_se else 0
                        nw = if axes.nw_se < 0 then -axes.nw_se else 0

                        dirs =
                            [ ( M.NE, ne ), ( M.SW, sw ), ( M.SE, se ), ( M.NW, nw ) ]
                                |> List.filter (\( _, s ) -> s > 0)
                                |> List.sortBy (\( _, s ) -> -s)
                    in
                    case dirs of
                        ( d1, s1 ) :: ( d2, s2 ) :: _ -> { primary = Just d1, secondary = Just d2, pSpeed = s1, sSpeed = s2 }
                        [ ( d1, s1 ) ] -> { primary = Just d1, secondary = Nothing, pSpeed = s1, sSpeed = 0 }
                        _ -> { primary = Nothing, secondary = Nothing, pSpeed = 0, sSpeed = 0 }
                Nothing -> { primary = Nothing, secondary = Nothing, pSpeed = 0, sSpeed = 0 }
    in
    { intent = maybeIntent
    , primaryDir = intentSummary.primary
    , secondaryDir = intentSummary.secondary
    , primarySpeed = intentSummary.pSpeed
    , secondarySpeed = intentSummary.sSpeed
    , isLong = intentDuration >= 0.4
    , shouldStop = maybeIntent == Nothing && (pointerStart /= Nothing || (Set.member " " keysDown))
    , interactionStart = interactionStart
    }

getIntent : Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe M.MovementIntent
getIntent keysDown pointerStart pointerLast =
    let
        joyIntent = getIntentFromJoystick pointerStart pointerLast
        kbdIntent = getIntentFromKeyboard keysDown
    in
    case joyIntent of
        Just _ -> joyIntent
        Nothing -> kbdIntent

getIntentFromKeyboard : Set String -> Maybe M.MovementIntent
getIntentFromKeyboard keys =
    let
        up = Set.member "ArrowUp" keys
        down = Set.member "ArrowDown" keys
        left = Set.member "ArrowLeft" keys
        right = Set.member "ArrowRight" keys

        -- Up (NW), Down (SE), Left (SW), Right (NE)
        ne_sw = (if right then 1.0 else 0.0) + (if left then -1.0 else 0.0)
        nw_se = (if down then 1.0 else 0.0) + (if up then -1.0 else 0.0)
    in
    if ne_sw == 0 && nw_se == 0 then Nothing
    else Just (M.Intent { ne_sw = ne_sw, nw_se = nw_se })

getIntentFromJoystick : Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe M.MovementIntent
getIntentFromJoystick pointerStart pointerLast =
    case ( pointerStart, pointerLast ) of
        ( Just start, Just last ) ->
            let
                dx = last.x - start.x
                dy = last.y - start.y
                dist = sqrt (dx * dx + dy * dy)
            in
            if dist > joystickDeadzone then
                let
                    -- ne_sw axis is NE-SW. Vector NE is (1, -1). Vector SW is (-1, 1).
                    -- nw_se axis is NW-SE. Vector SE is (1, 1). Vector NW is (-1, -1).
                    ne_sw = (dx - dy) / joystickMaxDist
                    nw_se = (dx + dy) / joystickMaxDist
                in
                Just (M.Intent { ne_sw = clamp -1 1 ne_sw, nw_se = clamp -1 1 nw_se })
            else Nothing
        _ -> Nothing


-- MOVEMENT STATE MACHINE

updateIdle : M.Position -> IntentInfo -> M.Maze -> M.PlayerState
updateIdle pos intent maze =
    let
        exits = M.getExits pos maze
        chosenDir =
            intent.primaryDir |> Maybe.andThen (\p ->
                if List.member p exits then Just p
                else
                    intent.secondaryDir |> Maybe.andThen (\s ->
                        if List.member s exits then Just s else Nothing
                    )
            )
    in
    case chosenDir of
        Just d ->
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = 0
                    , speedFactor = getSpeedForDir d intent.intent
                    , queuedIntent = M.QueuedNone
                    , interactionStart = intent.interactionStart
                    }
                Nothing -> M.Idle pos
        Nothing -> M.Idle pos

getSpeedForDir : M.Direction -> Maybe M.MovementIntent -> Float
getSpeedForDir dir intent =
    case intent of
        Just (M.Intent axes) ->
            let
                ( axisSpeed, _ ) =
                    case dir of
                        M.NE -> ( axes.ne_sw, axes.ne_sw < 0 )
                        M.SW -> ( -axes.ne_sw, axes.ne_sw > 0 )
                        M.SE -> ( axes.nw_se, axes.nw_se < 0 )
                        M.NW -> ( -axes.nw_se, axes.nw_se > 0 )
            in
            if axisSpeed > 0 then clamp 0 1 axisSpeed else 0.0
        Nothing -> 1.0

updateMoving : Float -> M.MovingData -> IntentInfo -> Bool -> M.Maze -> M.PlayerState
updateMoving dt m intent isRelease maze =
    let
        isOpposite = intent.primaryDir == Just (M.oppositeDirection m.dir) || intent.secondaryDir == Just (M.oppositeDirection m.dir)

        newQueuedIntent =
            if intent.isLong then M.QueuedNone
            else if intent.shouldStop then M.QueuedStop
            else
                let qDir = intent.primaryDir |> Maybe.Extra.or intent.secondaryDir in
                case qDir of
                    Just d ->
                        if d == M.oppositeDirection m.dir then M.QueuedStop
                        else if d == m.dir && intent.interactionStart == m.interactionStart then m.queuedIntent
                        else M.QueuedTurn d
                    Nothing -> m.queuedIntent

        speed = if isRelease then 1.0 else getSpeedForDir m.dir intent.intent

        activeM =
            if isOpposite && intent.isLong && not isRelease then
                let
                    oppDir = M.oppositeDirection m.dir
                    oppSpeed = getSpeedForDir oppDir intent.intent
                in
                if oppSpeed > 0 then
                    { from = m.to
                    , to = m.from
                    , dir = oppDir
                    , progress = max 0 (1.0 - m.progress)
                    , speedFactor = oppSpeed
                    , queuedIntent = M.QueuedNone
                    , interactionStart = intent.interactionStart
                    }
                else
                    { m | speedFactor = speed, queuedIntent = newQueuedIntent }
            else
                { m | speedFactor = speed, queuedIntent = newQueuedIntent }

        maxProgress = if activeM.to == M.endPosition maze then 4.0 else 1.0
        newProgress = activeM.progress + (dt * activeM.speedFactor / secondsPerStep)
    in
    if newProgress >= maxProgress then
        let pos = activeM.to in
        if pos == M.endPosition maze || activeM.queuedIntent == M.QueuedStop then M.Idle pos
        else nextTile pos (newProgress - maxProgress) activeM.queuedIntent activeM.dir intent maze (if isRelease then 1.0 else getSpeedForDir activeM.dir intent.intent)
    else
        M.Moving { activeM | progress = newProgress }

nextTile : M.Position -> Float -> M.QueuedIntent -> M.Direction -> IntentInfo -> M.Maze -> Float -> M.PlayerState
nextTile pos progress queuedIntent currentDir intent maze speedFactor =
    let
        exits = M.getExits pos maze
        isJunction = M.isJunction pos maze

        chosenDir =
            intent.primaryDir |> Maybe.andThen (\p ->
                if List.member p exits then Just p
                else
                    intent.secondaryDir |> Maybe.andThen (\s ->
                        if List.member s exits then Just s else Nothing
                    )
            )

        maybeMove d q iStart =
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = progress
                    , speedFactor = getSpeedForDir d intent.intent
                    , queuedIntent = q
                    , interactionStart = iStart
                    }
                Nothing -> M.Idle pos

        forwardExits = List.filter (\d -> d /= M.oppositeDirection currentDir) exits
    in
    case chosenDir of
        Just d -> maybeMove d M.QueuedNone intent.interactionStart
        Nothing ->
            case queuedIntent of
                M.QueuedTurn d ->
                    if isJunction then
                        if List.member d exits then maybeMove d M.QueuedNone intent.interactionStart
                        else M.Idle pos
                    else
                        continueInPath pos progress currentDir forwardExits maze speedFactor queuedIntent intent.interactionStart

                M.QueuedStop -> M.Idle pos
                M.QueuedNone ->
                    if not isJunction
                        then continueInPath pos progress currentDir forwardExits maze speedFactor M.QueuedNone intent.interactionStart
                        else M.Idle pos

continueInPath : M.Position -> Float -> M.Direction -> List M.Direction -> M.Maze -> Float -> M.QueuedIntent -> Maybe Duration -> M.PlayerState
continueInPath pos progress currentDir forwardExits maze speedFactor q iStart =
    let
        nextDir =
            if List.member currentDir forwardExits then Just currentDir
            else
                case forwardExits of
                    [ d ] -> Just d
                    _ -> Nothing
    in
    case nextDir of
        Just d ->
            let
                newQ =
                    case q of
                        M.QueuedTurn qd ->
                            if qd == d then M.QueuedNone
                            else q
                        _ -> q
            in
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = progress
                    , speedFactor = speedFactor
                    , queuedIntent = newQ
                    , interactionStart = iStart
                    }
                Nothing -> M.Idle pos
        Nothing -> M.Idle pos


-- LOW-LEVEL HELPERS

isArrow : String -> Bool
isArrow k =
    k == "ArrowUp" || k == "ArrowDown" || k == "ArrowLeft" || k == "ArrowRight"
