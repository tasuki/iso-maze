module Animate exposing (..)

import Maze as M

type alias Vec3 =
    { x : Float, y : Float, z : Float }

type alias SphereState =
    { current : Vec3
    , velocity : Vec3
    }

type alias AnimatorState =
    { spheres : List SphereState
    , timer : Float
    , initialized : Bool
    }


initAnimator : List Vec3 -> AnimatorState
initAnimator targets =
    let
        initSphere target =
            { current = { x = target.x, y = target.y, z = target.z + 10.0 }
            , velocity = { x = 0, y = 0, z = 0 }
            }
    in
    { spheres = List.map initSphere targets
    , timer = 0
    , initialized = True
    }

isAnimatorMoving : List Vec3 -> AnimatorState -> Bool
isAnimatorMoving targets state =
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
    in
    List.map2 isSphereMoving targets state.spheres |> List.any identity

updateAnimator : Float -> List Vec3 -> AnimatorState -> AnimatorState
updateAnimator totalDt targets state =
    let
        subSteps = 5
        dt = min totalDt 0.2 / toFloat subSteps
        staggerDelay = 0.05
        springK = 400
        damping = 30

        step currentSpheres currentTimer =
            let
                newTimer = currentTimer + dt
                updateSphere i target s prevCurrent prevTarget =
                    if newTimer < toFloat i * staggerDelay then
                        s
                    else
                        let
                            targetPos =
                                if i == 0 then target
                                else
                                    let
                                        desiredOffset =
                                            { x = target.x - prevTarget.x
                                            , y = target.y - prevTarget.y
                                            , z = target.z - prevTarget.z
                                            }
                                    in
                                    { x = prevCurrent.x + desiredOffset.x
                                    , y = prevCurrent.y + desiredOffset.y
                                    , z = prevCurrent.z + desiredOffset.z
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

                            acceleration =
                                { x = force.x + friction.x
                                , y = force.y + friction.y
                                , z = force.z + friction.z
                                }

                            newVelocity =
                                { x = s.velocity.x + acceleration.x * dt
                                , y = s.velocity.y + acceleration.y * dt
                                , z = s.velocity.z + acceleration.z * dt
                                }

                            newCurrent =
                                { x = s.current.x + newVelocity.x * dt
                                , y = s.current.y + newVelocity.y * dt
                                , z = s.current.z + newVelocity.z * dt
                                }
                        in
                        { current = newCurrent, velocity = newVelocity }

                folder ( i, target, s ) ( accSpheres, lastPrevCurrent, lastPrevTarget ) =
                    let
                        newS = updateSphere i target s lastPrevCurrent lastPrevTarget
                    in
                    ( accSpheres ++ [ newS ], newS.current, target )

                ( nextSpheres, _, _ ) =
                    List.foldl folder ( [], { x = 0, y = 0, z = 0 }, { x = 0, y = 0, z = 0 } )
                        (List.map3
                            (\i t s -> ( i, t, s ))
                            (List.range 0 (List.length currentSpheres - 1))
                            targets
                            currentSpheres
                        )
            in
            ( nextSpheres, newTimer )

        runSubSteps n (s, t) =
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

getPlayerTargets : M.PlayerState -> M.Maze -> List Vec3
getPlayerTargets playerState maze =
    let
        ( x, y, z ) = interpolatedPosition playerState

        playerPos_ ( px, py, pz ) zOffset =
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
                        (toFloat fix11 * (1 - fx) + toFloat fix21 * fx) * (1 - fy)
                            + (toFloat fix12 * (1 - fx) + toFloat fix22 * fx) * fy
            in
            { x = px * 10
            , y = py * 10
            , z = pz * 10 + zOffset + fix
            }

        playerSphere zOffset = playerPos_ ( x, y, z ) zOffset
    in
    [ playerSphere 2.0
    , playerSphere 5.5
    , playerSphere 8.5
    ]
