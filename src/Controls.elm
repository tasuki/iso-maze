module Controls exposing (..)

import Maze as M
import Set exposing (Set)
import DocumentDecoders as DD

type MovementIntent = Intent Float

getIntent : Set String -> Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe MovementIntent
getIntent keysDown pointerStart pointerLast =
    let
        kbdIntent = getIntentFromKeyboard keysDown
        joyIntent = getIntentFromJoystick pointerStart pointerLast
    in
    case kbdIntent of
        Just _ -> kbdIntent
        Nothing -> joyIntent

getIntentFromKeyboard : Set String -> Maybe MovementIntent
getIntentFromKeyboard keys =
    let
        up = Set.member "ArrowUp" keys
        down = Set.member "ArrowDown" keys
        left = Set.member "ArrowLeft" keys
        right = Set.member "ArrowRight" keys
        eps = 0.01
    in
    case ( ( up, down ), ( left, right ) ) of
        ( ( True, False ), ( True, False ) ) -> Just (Intent (directionToAngle M.NW))
        ( ( True, False ), ( False, True ) ) -> Just (Intent (directionToAngle M.NE))
        ( ( False, True ), ( True, False ) ) -> Just (Intent (directionToAngle M.SW))
        ( ( False, True ), ( False, True ) ) -> Just (Intent (directionToAngle M.SE))
        ( ( True, False ), _ ) -> Just (Intent (-pi/2 - eps))
        ( ( False, True ), _ ) -> Just (Intent (pi/2 - eps))
        ( _, ( True, False ) ) -> Just (Intent (pi - eps))
        ( _, ( False, True ) ) -> Just (Intent (0 - eps))
        _ -> Nothing

getIntentFromJoystick : Maybe DD.DocumentCoords -> Maybe DD.DocumentCoords -> Maybe MovementIntent
getIntentFromJoystick pointerStart pointerLast =
    case (pointerStart, pointerLast) of
        (Just start, Just last) ->
            let
                dx = last.x - start.x
                dy = last.y - start.y
                dist = sqrt (dx*dx + dy*dy)
                deadzone = 10
            in
            if dist > deadzone then
                Just (Intent (atan2 dy dx))
            else
                Nothing
        _ -> Nothing

resolveIntent : M.Position -> Maybe M.Direction -> MovementIntent -> M.Maze -> Maybe ( M.Direction, M.Position )
resolveIntent pos maybePrevDir (Intent angle) maze =
    let
        allDirs = [ M.NW, M.NE, M.SW, M.SE ]
        validMoves = List.filterMap (\d -> M.move pos d maze |> Maybe.map (Tuple.pair d)) allDirs

        diff d = angleDiff angle (directionToAngle d)

        sortByDiff ( d1, _ ) ( d2, _ ) =
            let
                diff1 = diff d1
                diff2 = diff d2
            in
            if diff1 < diff2 then LT
            else if diff1 > diff2 then GT
            else if Just d1 == maybePrevDir then LT
            else if Just d2 == maybePrevDir then GT
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
    let
        diff = abs (a - b)
    in
    if diff > pi then 2*pi - diff else diff
