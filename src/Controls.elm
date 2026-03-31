module Controls exposing (..)

import Maze as M
import Set exposing (Set)
import DocumentDecoders as DD

joystickDeadzone = 20
joystickMaxDist = 80

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
    else Just (M.Intent (atan2 dy dx) 1.0)

getIntentFromJoystick : Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe M.MovementIntent
getIntentFromJoystick pointerStart pointerLast =
    case (pointerStart, pointerLast) of
        (Just start, Just last) ->
            let
                dx = last.x - start.x
                dy = last.y - start.y
                dist = sqrt (dx*dx + dy*dy)
                speedFactor = min 1 (dist / joystickMaxDist)
            in
            if dist > joystickDeadzone
                then Just (M.Intent (atan2 dy dx) speedFactor)
                else Nothing
        _ -> Nothing

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


resolveIntent : M.Position -> M.MovementIntent -> M.Maze -> Maybe ( M.Direction, M.Position )
resolveIntent pos (M.Intent angle _) maze =
    let
        diff d = angleDiff angle (directionToAngle d)

        validMoves = List.filterMap
            (\d ->
                -- pi/2 radians picks direction up to 90° away
                -- 1.5 radians is about 86 angular degrees
                if diff d < 1.5
                    then M.move pos d maze |> Maybe.map (Tuple.pair d)
                    else Nothing
            )
            M.allDirections

        sortByDiff ( d1, _ ) ( d2, _ ) =
            let
                diff1 = diff d1
                diff2 = diff d2
            in
            if diff1 < diff2 then LT
            else if diff1 > diff2 then GT
            else EQ
    in
    validMoves
        |> List.sortWith sortByDiff
        |> List.head

directionToAngle : M.Direction -> Float
directionToAngle dir =
    case dir of
        M.NE -> -pi/4
        M.NW -> -3*pi/4
        M.SE -> pi/4
        M.SW -> 3*pi/4

angleDiff : Float -> Float -> Float
angleDiff a b =
    let diff = abs (a - b) in
    if diff > pi then 2*pi - diff else diff
