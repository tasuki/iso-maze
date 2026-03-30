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
        up = Set.member "ArrowUp" keys
        down = Set.member "ArrowDown" keys
        left = Set.member "ArrowLeft" keys
        right = Set.member "ArrowRight" keys
        eps = 0.01
    in
    case ( ( up, down ), ( left, right ) ) of
        ( ( True, False ), ( True, False ) ) -> Just (M.Intent (directionToAngle M.NW) 1.0)
        ( ( True, False ), ( False, True ) ) -> Just (M.Intent (directionToAngle M.NE) 1.0)
        ( ( False, True ), ( True, False ) ) -> Just (M.Intent (directionToAngle M.SW) 1.0)
        ( ( False, True ), ( False, True ) ) -> Just (M.Intent (directionToAngle M.SE) 1.0)
        ( ( True, False ), _ ) -> Just (M.Intent (-pi/2 - eps) 1.0)
        ( ( False, True ), _ ) -> Just (M.Intent (pi/2 - eps) 1.0)
        ( _, ( True, False ) ) -> Just (M.Intent (pi - eps) 1.0)
        ( _, ( False, True ) ) -> Just (M.Intent (0 - eps) 1.0)
        _ -> Nothing

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
