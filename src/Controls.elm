module Controls exposing (..)

import Maze as M
import Set exposing (Set)
import DocumentDecoders as DD

type MovementIntent
    = North (Maybe Float)
    | South (Maybe Float)
    | East (Maybe Float)
    | West (Maybe Float)
    | NW_Strict
    | NE_Strict
    | SW_Strict
    | SE_Strict

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
        ( ( True, False ), ( True, False ) ) -> Just NW_Strict
        ( ( True, False ), ( False, True ) ) -> Just NE_Strict
        ( ( False, True ), ( True, False ) ) -> Just SW_Strict
        ( ( False, True ), ( False, True ) ) -> Just SE_Strict
        ( ( True, False ), _ ) -> Just (North (Just (-pi/2 - eps)))
        ( ( False, True ), _ ) -> Just (South (Just (pi/2 - eps)))
        ( _, ( True, False ) ) -> Just (West (Just (pi - eps)))
        ( _, ( False, True ) ) -> Just (East (Just (0 - eps)))
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
                let
                    angle = atan2 dy dx
                    -- PI/8 = 0.3927
                    -- sectors of 45 deg centered on cardinals and diagonals
                in
                if angle >= -pi/8 && angle < pi/8 then Just (East (Just angle))
                else if angle >= pi/8 && angle < 3*pi/8 then Just SE_Strict
                else if angle >= 3*pi/8 && angle < 5*pi/8 then Just (South (Just angle))
                else if angle >= 5*pi/8 && angle < 7*pi/8 then Just SW_Strict
                else if angle >= 7*pi/8 || angle < -7*pi/8 then Just (West (Just angle))
                else if angle >= -7*pi/8 && angle < -5*pi/8 then Just NW_Strict
                else if angle >= -5*pi/8 && angle < -3*pi/8 then Just (North (Just angle))
                else Just NE_Strict
            else
                Nothing
        _ -> Nothing

resolveIntent : M.Position -> Maybe M.Direction -> MovementIntent -> M.Maze -> Maybe M.Direction
resolveIntent pos maybePrevDir intent maze =
    case intent of
        NW_Strict -> M.move pos M.NW maze |> Maybe.map (always M.NW)
        NE_Strict -> M.move pos M.NE maze |> Maybe.map (always M.NE)
        SW_Strict -> M.move pos M.SW maze |> Maybe.map (always M.SW)
        SE_Strict -> M.move pos M.SE maze |> Maybe.map (always M.SE)

        North maybeAngle -> resolveCardinal pos maybePrevDir maybeAngle M.NW M.NE maze
        South maybeAngle -> resolveCardinal pos maybePrevDir maybeAngle M.SW M.SE maze
        East maybeAngle -> resolveCardinal pos maybePrevDir maybeAngle M.NE M.SE maze
        West maybeAngle -> resolveCardinal pos maybePrevDir maybeAngle M.NW M.SW maze

resolveCardinal : M.Position -> Maybe M.Direction -> Maybe Float -> M.Direction -> M.Direction -> M.Maze -> Maybe M.Direction
resolveCardinal pos maybePrevDir maybeAngle d1 d2 maze =
    let
        m1 = M.move pos d1 maze
        m2 = M.move pos d2 maze
    in
    case (m1, m2) of
        (Just _, Just _) ->
            -- Both available! Tie-break.
            case maybeAngle of
                Just angle ->
                    -- Joystick: pick closest
                    if angleDiff angle (directionToAngle d1) < angleDiff angle (directionToAngle d2) then
                        Just d1
                    else
                        Just d2
                Nothing ->
                    -- Keyboard: continue current if it's one of them
                    if maybePrevDir == Just d1 then Just d1
                    else if maybePrevDir == Just d2 then Just d2
                    else Just d1 -- Default to d1 (arbitrary)

        (Just _, Nothing) -> Just d1
        (Nothing, Just _) -> Just d2
        (Nothing, Nothing) -> Nothing

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
