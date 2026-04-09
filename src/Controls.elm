module Controls exposing (..)

import Angle exposing (Angle)
import DocumentDecoders as DD
import Duration exposing (Duration)
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
        ( primary, secondary ) =
            case maybeIntent of
                Just (M.Intent axes) ->
                    let
                        ne_sw = axes.ne_sw
                        nw_se = axes.nw_se
                        pDir =
                            if abs ne_sw >= abs nw_se then
                                if ne_sw > 0 then Just M.NE else if ne_sw < 0 then Just M.SW else Nothing
                            else
                                if nw_se > 0 then Just M.SE else if nw_se < 0 then Just M.NW else Nothing
                        sDir =
                            if abs ne_sw >= abs nw_se then
                                if nw_se > 0 then Just M.SE else if nw_se < 0 then Just M.NW else Nothing
                            else
                                if ne_sw > 0 then Just M.NE else if ne_sw < 0 then Just M.SW else Nothing
                    in
                    ( pDir, sDir )
                Nothing -> ( Nothing, Nothing )
    in
    { intent = maybeIntent
    , primaryDir = primary
    , secondaryDir = secondary
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
                    -- In our isometric view:
                    -- Right (dx > 0) is NE (+ne_sw, -nw_se) if we rotate 45 deg?
                    -- Let's derive from the existing directionToAngle:
                    -- SE: pi/4 (1, 1) in screen coords? No, wait.
                    -- Looking at existing code:
                    -- SE: 1*pi/4, SW: 3*pi/4, NW: 5*pi/4, NE: 7*pi/4
                    -- Screen coords: X is right, Y is down.
                    -- atan2 dy dx:
                    -- SE (dx=1, dy=1) -> pi/4
                    -- SW (dx=-1, dy=1) -> 3*pi/4
                    -- NW (dx=-1, dy=-1) -> -3*pi/4 (or 5*pi/4)
                    -- NE (dx=1, dy=-1) -> -pi/4 (or 7*pi/4)

                    -- Projecting (dx, dy) onto these axes:
                    -- Axis SE: (1, 1)/sqrt(2)
                    -- Axis SW: (-1, 1)/sqrt(2)
                    -- Axis NW: (-1, -1)/sqrt(2)
                    -- Axis NE: (1, -1)/sqrt(2)

                    -- ne_sw axis is NE-SW. Vector NE is (1, -1). Vector SW is (-1, 1).
                    -- nw_se axis is NW-SE. Vector SE is (1, 1). Vector NW is (-1, -1).

                    ne_sw_raw = dx - dy
                    nw_se_raw = dx + dy

                    speedFactor = 1.0 / joystickMaxDist
                    -- We want 100% speed when the joystick is at joystickMaxDist along an axis.
                    -- If we tilt 45 degrees between axes, we can have 100% in both.
                in
                Just (M.Intent { ne_sw = ne_sw_raw * speedFactor, nw_se = nw_se_raw * speedFactor })
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
                else intent.secondaryDir |> Maybe.andThen (\s ->
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
            case dir of
                M.NE -> max 0 axes.ne_sw
                M.SW -> max 0 -axes.ne_sw
                M.SE -> max 0 axes.nw_se
                M.NW -> max 0 -axes.nw_se
        Nothing -> 1.0

updateMoving : Float -> M.MovingData -> IntentInfo -> Bool -> M.Maze -> M.PlayerState
updateMoving dt m intent isRelease maze =
    let
        isOpposite = intent.primaryDir == Just (M.oppositeDirection m.dir) || intent.secondaryDir == Just (M.oppositeDirection m.dir)
        isCurrentInteraction = intent.interactionStart == m.interactionStart

        newQueuedIntent =
            if intent.isLong then M.QueuedNone
            else if intent.shouldStop then M.QueuedStop
            else
                let
                    qDir =
                        if intent.primaryDir /= Just m.dir && intent.primaryDir /= Nothing then intent.primaryDir
                        else if intent.secondaryDir /= Just m.dir && intent.secondaryDir /= Nothing then intent.secondaryDir
                        else Nothing
                in
                case qDir of
                    Just d ->
                        if d == M.oppositeDirection m.dir then M.QueuedStop
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
                    , queuedIntent = newQueuedIntent
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
        forwardLongExits = List.filter (\d -> d /= M.oppositeDirection currentDir && hasPath 4 pos d maze) exits
        effectiveJunction = isJunction && List.length forwardLongExits /= 1

        chosenDir =
            intent.primaryDir |> Maybe.andThen (\p ->
                if List.member p exits then Just p
                else intent.secondaryDir |> Maybe.andThen (\s ->
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
                    , speedFactor = if iStart == Nothing then 1.0 else getSpeedForDir d intent.intent
                    , queuedIntent = q
                    , interactionStart = iStart
                    }
                Nothing -> M.Idle pos

        forwardExits =
            if isJunction then forwardLongExits
            else List.filter (\d -> d /= M.oppositeDirection currentDir) exits
    in
    case chosenDir of
        Just d -> maybeMove d M.QueuedNone intent.interactionStart
        Nothing ->
            case queuedIntent of
                M.QueuedTurn d ->
                    if effectiveJunction then
                        if List.member d exits && hasPath 4 pos d maze
                            then maybeMove d M.QueuedNone Nothing
                            else M.Idle pos
                    else if isJunction then
                        continueInPath pos progress currentDir forwardExits maze speedFactor M.QueuedNone Nothing
                    else
                        continueInPath pos progress currentDir forwardExits maze speedFactor queuedIntent Nothing

                M.QueuedStop -> M.Idle pos
                M.QueuedNone ->
                    if intent.intent /= Nothing || not effectiveJunction
                        then continueInPath pos progress currentDir forwardExits maze speedFactor M.QueuedNone intent.interactionStart
                        else M.Idle pos

continueInPath : M.Position -> Float -> M.Direction -> List M.Direction -> M.Maze -> Float -> M.QueuedIntent -> Maybe Duration -> M.PlayerState
continueInPath pos progress currentDir forwardExits maze speedFactor q iStart =
    let
        nextDir =
            if List.member currentDir forwardExits then
                Just currentDir
            else
                case forwardExits of
                    [ d ] -> Just d
                    _ -> Nothing
    in
    case nextDir of
        Just d ->
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = progress
                    , speedFactor = speedFactor
                    , queuedIntent = q
                    , interactionStart = iStart
                    }
                Nothing -> M.Idle pos
        Nothing -> M.Idle pos


-- LOW-LEVEL HELPERS

hasPath : Int -> M.Position -> M.Direction -> M.Maze -> Bool
hasPath n pos dir maze =
    case M.move pos dir maze of
        Nothing -> False
        Just nextPos ->
            if n <= 1 then True
            else
                let
                    exits = M.getExits nextPos maze
                    backDir = M.oppositeDirection dir
                    forwardExits = List.filter (\d -> d /= backDir) exits
                in
                List.any (\d -> hasPath (n - 1) nextPos d maze) forwardExits


isArrow : String -> Bool
isArrow k =
    k == "ArrowUp" || k == "ArrowDown" || k == "ArrowLeft" || k == "ArrowRight"
