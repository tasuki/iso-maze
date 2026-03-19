module Animate exposing (..)

import Maze as M

type alias Vec3 =
    { x : Float, y : Float, z : Float }

type alias SphereState =
    { current : Vec3
    , velocity : Vec3
    }

type alias Triple a = ( a, a, a )

type alias AnimatorState =
    { spheres : Triple SphereState
    , timer : Float
    , initialFall : Bool
    }

fallDuration : Float
fallDuration = 2

initAnimator : Triple Vec3 -> AnimatorState
initAnimator ( t1, t2, t3 ) =
    let
        initSphere target =
            { current = { x = target.x, y = target.y, z = target.z + 30 }
            , velocity = { x = 0, y = 0, z = 0 }
            }
    in
    { spheres = ( initSphere t1, initSphere t2, initSphere t3 )
    , timer = 0
    , initialFall = True
    }

initAnimatorAt : Triple Vec3 -> AnimatorState
initAnimatorAt ( t1, t2, t3 ) =
    let
        initSphere target =
            { current = target
            , velocity = { x = 0, y = 0, z = 0 }
            }
    in
    { spheres = ( initSphere t1, initSphere t2, initSphere t3 )
    , timer = 0
    , initialFall = False
    }

isAnimatorMoving : Triple Vec3 -> AnimatorState -> Bool
isAnimatorMoving ( t1, t2, t3 ) state =
    let
        velThreshold = 10
        posThreshold = 1
        isSphereMoving target s =
            let
                dv2 = s.velocity.x * s.velocity.x
                    + s.velocity.y * s.velocity.y
                    + s.velocity.z * s.velocity.z
                dp2 = (s.current.x - target.x) * (s.current.x - target.x)
                    + (s.current.y - target.y) * (s.current.y - target.y)
                    + (s.current.z - target.z) * (s.current.z - target.z)
            in
            dv2 > velThreshold * velThreshold || dp2 > posThreshold * posThreshold
        ( s1, s2, s3 ) = state.spheres
    in
    (state.initialFall && state.timer <= fallDuration)
    || isSphereMoving t1 s1 || isSphereMoving t2 s2 || isSphereMoving t3 s3

updateAnimator : Float -> Triple Vec3 -> AnimatorState -> AnimatorState
updateAnimator totalDt ( t1, t2, t3 ) state =
    let
        subSteps = 5
        dt = min totalDt 0.2 / toFloat subSteps
        staggerDelay = 0.1

        ( bottomSphere, _, _ ) = state.spheres
        isIntroFalling = bottomSphere.current.z > t1.z + 10

        springK = if isIntroFalling then 40 else 600
        damping = if isIntroFalling then 15 else 30

        updateSphere curT i target s prevC prevT =
            if curT < toFloat i * staggerDelay then s
            else
                let
                    targetPos =
                        if i == 0 then target
                        else
                            { x = prevC.x + (target.x - prevT.x)
                            , y = prevC.y + (target.y - prevT.y)
                            , z = prevC.z + (target.z - prevT.z)
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
                    accel =
                        { x = force.x + friction.x
                        , y = force.y + friction.y
                        , z = force.z + friction.z
                        }
                    newV =
                        { x = s.velocity.x + accel.x * dt
                        , y = s.velocity.y + accel.y * dt
                        , z = s.velocity.z + accel.z * dt
                        }
                    newC =
                        { x = s.current.x + newV.x * dt
                        , y = s.current.y + newV.y * dt
                        , z = s.current.z + newV.z * dt
                        }
                in { current = newC, velocity = newV }
        step ( s1, s2, s3 ) currentTimer =
            let
                newTimer = currentTimer + dt
                ns1 = updateSphere newTimer 0 t1 s1 { x = 0, y = 0, z = 0 } { x = 0, y = 0, z = 0 }
                ns2 = updateSphere newTimer 1 t2 s2 ns1.current t1
                ns3 = updateSphere newTimer 2 t3 s3 ns2.current t2
            in
            ( ( ns1, ns2, ns3 ), newTimer )
        runSubSteps n ( s, t ) =
            if n <= 0 then ( s, t )
            else runSubSteps (n - 1) (step s t)

        ( finalSpheres, finalTimer ) = runSubSteps subSteps ( state.spheres, state.timer )
    in
    { state
    | spheres = finalSpheres
    , timer = finalTimer
    , initialFall = state.initialFall && finalTimer <= fallDuration
    }


type alias HatTransform =
    { x : Float
    , y : Float
    , z : Float
    , squash : Float
    }

computeHatTransform : M.Maze -> M.PlayerState -> Vec3 -> Float -> Bool -> HatTransform
computeHatTransform maze playerState head timer initialFall =
    let
        goal = M.endPosition maze
        ( gx, gy, gz ) = goal
        goalX = toFloat gx * 10
        goalY = toFloat gy * 10
        fz = toFloat gz * 10
    in
    if initialFall && timer <= fallDuration then
        let
            startPos = M.startPosition maze
            ( _, _, startHead ) = getPlayerTargets (M.Idle startPos) maze
            t = clamp 0 1 (timer / fallDuration)
            jumpHeight = 40.0
            startZ = startHead.z + 30.0 + 1.4
            targetZ = fz
            currentZ = startZ * (1.0 - t) + targetZ * t + jumpHeight * 4.0 * t * (1.0 - t)
        in
        { x = startHead.x * (1.0 - t) + goalX * t
        , y = startHead.y * (1.0 - t) + goalY * t
        , z = currentZ
        , squash = 0
        }
    else
        let
            jumpHeight = 18.0
            goal2d = M.positionTo2d goal
            ( from, to, progress ) =
                case playerState of
                    M.Idle p -> ( p, p, 1.0 )
                    M.Moving m -> ( m.from, m.to, m.progress )

            canMoveTo fromPos targetPos =
                List.any (\dir -> M.move fromPos dir maze == Just targetPos) [ M.SE, M.SW, M.NE, M.NW ]

            ( currentBaseZ, squashFactor ) =
                if M.positionTo2d to == goal2d then
                    if M.positionTo2d from == goal2d then
                        -- Already at goal
                        ( head.z + 1.4, 0 )
                    else
                        -- Moving to goal
                        let
                            -- Jump up fast
                            t = clamp 0 0.333 (progress * 0.2)
                            hJump = 6.75 * jumpHeight * t * (1.0 - t) ^ 2

                            -- Descend slowly
                            descendProgress = clamp 0 1 ((progress - 1.0) / 3.0)
                            targetZ = head.z + 1.4
                            currentZ =
                                if progress < 1.0 then fz + hJump
                                else
                                    let startDescendZ = fz + jumpHeight
                                    in startDescendZ + (targetZ - startDescendZ) * descendProgress

                            -- Squash should dissipate as it jumps
                            squash = clamp 0 1 (1.0 - progress * 5.0)
                        in
                        ( currentZ, squash )
                else
                    let
                        squash =
                            if canMoveTo to goal && progress >= 1.0 then 1.0
                            else if canMoveTo to goal && progress < 1.0 then progress
                            else if canMoveTo from goal && progress < 1.0 then 1.0 - progress
                            else 0
                    in
                    ( fz, squash )
        in
        { x = goalX, y = goalY, z = currentBaseZ, squash = squashFactor }


interpolatedPosition : M.PlayerState -> (Float, Float, Float)
interpolatedPosition playerState =
    case playerState of
        M.Idle ( x, y, z ) -> ( toFloat x, toFloat y, toFloat z )
        M.Moving m ->
            let
                ( x1, y1, z1 ) = m.from
                ( x2, y2, z2 ) = m.to
                p = clamp 0 1 m.progress
                lerp a b t = toFloat a + (toFloat b - toFloat a) * t
            in
            ( lerp x1 x2 p, lerp y1 y2 p, lerp z1 z2 p )


type alias InterpolateHelper = M.Maze -> ( Int, Int ) -> Int -> Float

interpolate : InterpolateHelper -> M.Maze -> Triple Float -> Float
interpolate fun maze ( x, y, z ) =
    let
        iz = round z
        x1 = floor x
        x2 = ceiling x
        y1 = floor y
        y2 = ceiling y
        fx = x - toFloat x1
        fy = y - toFloat y1

        fix11 = fun maze ( x1, y1 ) iz
        fix12 = fun maze ( x1, y2 ) iz
        fix21 = fun maze ( x2, y1 ) iz
        fix22 = fun maze ( x2, y2 ) iz
    in
    if x1 == x2 && y1 == y2 then
        fix11
    else if x1 == x2 then
        fix11 * (1 - fy) + fix12 * fy
    else if y1 == y2 then
        fix11 * (1 - fx) + fix21 * fx
    else
        (fix11 * (1 - fx) + fix21 * fx) * (1 - fy) +
        (fix12 * (1 - fx) + fix22 * fx) * fy

fixHelper : InterpolateHelper
fixHelper maze ( ix, iy ) iz =
    case M.get ( ix, iy ) maze of
        Just (M.Stairs _ _) -> -5
        Just (M.Bridge ( _, _, bz ) ) ->
            if iz == bz then 1 else 0
        _ -> 0

getFix : M.Maze -> Triple Float -> Float
getFix = interpolate fixHelper

isUnderBridge : M.Maze -> M.Position -> Bool
isUnderBridge maze ( x, y, z ) =
    case M.get ( x, y ) maze of
        Just (M.Bridge ( _, _, bz ) ) ->
            z == bz - 1
        _ ->
            False


getPlayerTargets : M.PlayerState -> M.Maze -> Triple Vec3
getPlayerTargets playerState maze =
    let
        ( x, y, z ) = interpolatedPosition playerState
        fix = getFix maze ( x, y, z )

        squish =
            case playerState of
                M.Idle pos ->
                    if isUnderBridge maze pos then 1.0 else 0.0

                M.Moving m ->
                    let
                        fromUnder = isUnderBridge maze m.from
                        toUnder = isUnderBridge maze m.to
                        p = clamp 0 1 m.progress
                    in
                    if fromUnder && toUnder then
                        1.0
                    else if toUnder then
                        -- Entering bridge: aggressive rise, fluid arrival.
                        1.0 - (1.0 - p) ^ 12
                    else if fromUnder then
                        -- Exiting bridge: stay squished longer, fluid departure.
                        1.0 - p ^ 12
                    else
                        0.0

        lerp a b t = a + (b - a) * t
        z1 = lerp 2.0 1.0 squish
        z2 = lerp 5.5 3.0 squish
        z3 = lerp 8.5 5.5 squish

        playerPos ( px, py, pz ) zOffset =
            { x = px * 10
            , y = py * 10
            , z = pz * 10 + zOffset + fix
            }
        playerSphere zOffset = playerPos ( x, y, z ) zOffset
    in
    ( playerSphere z1, playerSphere z2, playerSphere z3 )
