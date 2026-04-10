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
    , dir : Maybe M.Direction
    , speed : Float
    , isLong : Bool
    , shouldStop : Bool
    , interactionStart : Maybe Duration
    }

analyzeIntent : Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe Duration -> Duration -> IntentInfo
analyzeIntent keysDown pointerStart pointerLast interactionStart currentTime =
    let
        maybeIntent = getIntent keysDown pointerStart pointerLast
        intentDuration = interactionStart
            |> Maybe.map (\i -> currentTime |> Quantity.minus i |> Duration.inSeconds)
            |> Maybe.withDefault 0.0
    in
    { intent = maybeIntent
    , dir = maybeIntent |> Maybe.map (\(M.Intent a _) -> resolveDirection a)
    , speed = maybeIntent |> Maybe.map (\(M.Intent a s) -> resolveSpeed (resolveDirection a) s) |> Maybe.withDefault 1.0
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
        up = if Set.member "ArrowUp" keys then -1.0 else 0.0
        down = if Set.member "ArrowDown" keys then 1.0 else 0.0
        left = if Set.member "ArrowLeft" keys then -1.0 else 0.0
        right = if Set.member "ArrowRight" keys then 1.0 else 0.0
        dx = left + right
        dy = up + down
    in
    if dx == 0 && dy == 0 then Nothing
    else Just (M.Intent (atan2 dy dx - 0.0001) { nwse = 1.0, nesw = 1.0 })

getIntentFromJoystick : Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe M.MovementIntent
getIntentFromJoystick pointerStart pointerLast =
    case ( pointerStart, pointerLast ) of
        ( Just start, Just last ) ->
            let
                dx = last.x - start.x
                dy = last.y - start.y
                dist = sqrt (dx * dx + dy * dy)
                nwse = abs (dx + dy) / (sqrt 2 * joystickMaxDist) |> min 1.0
                nesw = abs (dx - dy) / (sqrt 2 * joystickMaxDist) |> min 1.0
            in
            if dist > joystickDeadzone
                then Just (M.Intent (atan2 dy dx) { nwse = nwse, nesw = nesw })
                else Nothing
        _ -> Nothing


-- MOVEMENT STATE MACHINE

updateIdle : M.Position -> IntentInfo -> M.Maze -> M.PlayerState
updateIdle pos intent maze =
    let
        exits = M.getExits pos maze
        chosenDir =
            Maybe.andThen (\(M.Intent a speeds) ->
                if intent.isLong then findBestExit a exits
                else intent.dir |> Maybe.andThen (\d -> if List.member d exits then Just d else Nothing)
            ) intent.intent

        speedFactor =
            case ( intent.intent, chosenDir ) of
                ( Just (M.Intent _ speeds), Just d ) -> resolveSpeed d speeds
                _ -> intent.speed
    in
    case chosenDir of
        Just d ->
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = 0
                    , speedFactor = speedFactor
                    , queuedIntent = M.QueuedNone
                    , interactionStart = intent.interactionStart
                    }
                Nothing -> M.Idle pos
        Nothing -> M.Idle pos

updateMoving : Float -> M.MovingData -> IntentInfo -> Bool -> M.Maze -> M.PlayerState
updateMoving dt m intent isRelease maze =
    let
        isOpposite = intent.dir == Just (M.oppositeDirection m.dir)
        isCurrentInteraction = intent.interactionStart == m.interactionStart

        newQueuedIntent =
            if intent.isLong then M.QueuedNone
            else if intent.shouldStop then M.QueuedStop
            else case intent.dir of
                Just d ->
                    if isOpposite then M.QueuedStop
                    else if isCurrentInteraction && d == m.dir then m.queuedIntent
                    else M.QueuedTurn d
                Nothing -> m.queuedIntent

        activeM =
            if isOpposite && intent.isLong then
                { from = m.to
                , to = m.from
                , dir = M.oppositeDirection m.dir
                , progress = max 0 (1.0 - m.progress)
                , speedFactor = m.speedFactor
                , queuedIntent = newQueuedIntent
                , interactionStart = intent.interactionStart
                }
            else
                { m | queuedIntent = newQueuedIntent }

        speed =
            if isRelease then 1.0
            else
                case intent.intent of
                    Just (M.Intent _ s) -> resolveSpeed activeM.dir s
                    Nothing -> 1.0

        maxProgress = if activeM.to == M.endPosition maze then 4.0 else 1.0
        newProgress = activeM.progress + (dt * speed / secondsPerStep)
    in
    if newProgress >= maxProgress then
        let pos = activeM.to in
        if pos == M.endPosition maze || activeM.queuedIntent == M.QueuedStop then M.Idle pos
        else nextTile pos (newProgress - maxProgress) activeM.queuedIntent activeM.dir intent maze (if isRelease then 1.0 else speed)
    else
        M.Moving { activeM | progress = newProgress, speedFactor = speed }

nextTile : M.Position -> Float -> M.QueuedIntent -> M.Direction -> IntentInfo -> M.Maze -> Float -> M.PlayerState
nextTile pos progress queuedIntent currentDir intent maze speedFactor =
    let
        exits = M.getExits pos maze
        isJunction = M.isJunction pos maze
        forwardLongExits = List.filter (\d -> d /= M.oppositeDirection currentDir && hasPath 4 pos d maze) exits
        effectiveJunction = isJunction && List.length forwardLongExits /= 1

        chosenDir =
            Maybe.andThen (\(M.Intent a _) ->
                if intent.isLong then findBestExit a exits
                else intent.dir |> Maybe.andThen (\d -> if List.member d exits then Just d else Nothing)
            ) intent.intent

        maybeMove d q iStart =
            let
                moveSpeed =
                    case intent.intent of
                        Just (M.Intent _ s) -> resolveSpeed d s
                        Nothing -> speedFactor
            in
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = progress
                    , speedFactor = moveSpeed
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
                        continueInPath pos progress currentDir forwardExits maze speedFactor M.QueuedNone Nothing intent
                    else
                        continueInPath pos progress currentDir forwardExits maze speedFactor queuedIntent Nothing intent

                M.QueuedStop -> M.Idle pos
                M.QueuedNone ->
                    if intent.intent /= Nothing || not effectiveJunction
                        then continueInPath pos progress currentDir forwardExits maze speedFactor M.QueuedNone intent.interactionStart intent
                        else M.Idle pos

continueInPath : M.Position -> Float -> M.Direction -> List M.Direction -> M.Maze -> Float -> M.QueuedIntent -> Maybe Duration -> IntentInfo -> M.PlayerState
continueInPath pos progress currentDir forwardExits maze speedFactor q iStart intent =
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
            let
                speed =
                    case intent.intent of
                        Just (M.Intent _ s) -> resolveSpeed d s
                        Nothing -> speedFactor
            in
            case M.move pos d maze of
                Just nextTo -> M.Moving
                    { from = pos
                    , to = nextTo
                    , dir = d
                    , progress = progress
                    , speedFactor = speed
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


resolveSpeed : M.Direction -> M.Speeds -> Float
resolveSpeed dir speeds =
    case dir of
        M.SE -> speeds.nwse
        M.NW -> speeds.nwse
        M.SW -> speeds.nesw
        M.NE -> speeds.nesw


resolveDirection : Float -> M.Direction
resolveDirection angle =
    let
        diff d = angleDiff angle (directionToAngle d)
    in
    M.allDirections
        |> List.map (\d -> ( d, diff d ))
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault M.SE

findBestExit : Float -> List M.Direction -> Maybe M.Direction
findBestExit angle dirs =
    let
        diff d = angleDiff angle (directionToAngle d)
    in
    dirs
        |> List.filter (\d -> diff d < 1.3)
        |> List.sortBy diff
        |> List.head

directionToAngle : M.Direction -> Float
directionToAngle dir =
    case dir of
        M.SE -> 1*pi/4
        M.SW -> 3*pi/4
        M.NW -> 5*pi/4
        M.NE -> 7*pi/4

angleDiff : Float -> Float -> Float
angleDiff a b =
    acos (cos (a - b))

isArrow : String -> Bool
isArrow k =
    k == "ArrowUp" || k == "ArrowDown" || k == "ArrowLeft" || k == "ArrowRight"
