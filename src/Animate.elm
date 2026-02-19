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
    , initialized : Bool
    }

initAnimator : Triple Vec3 -> AnimatorState
initAnimator ( t1, t2, t3 ) =
    let
        initSphere target =
            { current = { x = target.x, y = target.y, z = target.z + 10.0 }
            , velocity = { x = 0, y = 0, z = 0 }
            }
    in
    { spheres = ( initSphere t1, initSphere t2, initSphere t3 )
    , timer = 0
    , initialized = True
    }

isAnimatorMoving : Triple Vec3 -> AnimatorState -> Bool
isAnimatorMoving ( t1, t2, t3 ) state =
    let
        velThreshold = 0.1
        posThreshold = 0.01
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
    isSphereMoving t1 s1 || isSphereMoving t2 s2 || isSphereMoving t3 s3

updateAnimator : Float -> Triple Vec3 -> AnimatorState -> AnimatorState
updateAnimator totalDt ( t1, t2, t3 ) state =
    let
        subSteps = 5
        dt = min totalDt 0.2 / toFloat subSteps
        staggerDelay = 0.05
        springK = 400
        damping = 30

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
            if n <= 0 then { state | spheres = s, timer = t }
            else runSubSteps (n - 1) (step s t)
    in
    runSubSteps subSteps ( state.spheres, state.timer )

interpolatedPosition : M.PlayerState -> (Float, Float, Float)
interpolatedPosition playerState =
    case playerState of
        M.Idle ( x, y, z ) -> ( toFloat x, toFloat y, toFloat z )
        M.Moving m ->
            let
                ( x1, y1, z1 ) = m.from
                ( x2, y2, z2 ) = m.to
                p = m.progress
                lerp a b t = toFloat a + (toFloat b - toFloat a) * t
            in
            ( lerp x1 x2 p, lerp y1 y2 p, lerp z1 z2 p )

getPlayerTargets : M.PlayerState -> M.Maze -> Triple Vec3
getPlayerTargets playerState maze =
    let
        ( x, y, z ) = interpolatedPosition playerState

        playerPos ( px, py, pz ) zOffset =
            let
                getFix ( ix, iy ) =
                    case M.get ( ix, iy ) maze of
                        Just (M.Stairs _ _) -> -5
                        _ -> 0

                x1 = floor px
                x2 = ceiling px
                y1 = floor py
                y2 = ceiling py
                fx = px - toFloat x1
                fy = py - toFloat y1

                fix11 = getFix ( x1, y1 )
                fix12 = getFix ( x1, y2 )
                fix21 = getFix ( x2, y1 )
                fix22 = getFix ( x2, y2 )

                fix =
                    if x1 == x2 && y1 == y2 then
                        toFloat fix11
                    else if x1 == x2 then
                        toFloat fix11 * (1 - fy) + toFloat fix12 * fy
                    else if y1 == y2 then
                        toFloat fix11 * (1 - fx) + toFloat fix21 * fx
                    else
                        (toFloat fix11 * (1 - fx) + toFloat fix21 * fx) * (1 - fy) +
                        (toFloat fix12 * (1 - fx) + toFloat fix22 * fx) * fy
            in
            { x = px * 10
            , y = py * 10
            , z = pz * 10 + zOffset + fix
            }
        playerSphere zOffset = playerPos ( x, y, z ) zOffset
    in
    ( playerSphere 2.0, playerSphere 5.5, playerSphere 8.5 )
