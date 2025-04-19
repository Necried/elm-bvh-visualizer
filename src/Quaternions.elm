module Quaternions exposing (..)

import Angle exposing (Angle, sin)
import Base exposing (..)
import Length
import Quantity
import Vector3d exposing (Vector3d)


type alias Quaternion =
    { reals : Float
    , imaginaries : Vec
    }


quaternion : Float -> Float -> Float -> Float -> Quaternion
quaternion w x y z =
    { reals = w, imaginaries = Vector3d.unitless x y z }


toList : Quaternion -> List Float
toList q =
    let
        v =
            Vector3d.toRecord Quantity.toFloat q.imaginaries
    in
    [ q.reals, v.x, v.y, v.z ]


fromList : List Float -> Maybe Quaternion
fromList fls =
    case fls of
        [ w, x, y, z ] ->
            Just <| quaternion w x y z

        _ ->
            Nothing



-- WARNING: This loses units on the vector!
-- This is used in applying rotations so the units are safely handled


vecToQuaternion : VecMeters -> Quaternion
vecToQuaternion vec =
    quaternion 0
        (Length.inMeters <| Vector3d.xComponent vec)
        (Length.inMeters <| Vector3d.yComponent vec)
        (Length.inMeters <| Vector3d.zComponent vec)



-- WARNING: This assumes a quaternion representing position in Meters!
-- This is used in applying rotations so the units are safely handled


quaternionToVec : Quaternion -> VecMeters
quaternionToVec q =
    Vector3d.fromMeters
        { x = Quantity.toFloat <| Vector3d.xComponent q.imaginaries
        , y = Quantity.toFloat <| Vector3d.yComponent q.imaginaries
        , z = Quantity.toFloat <| Vector3d.zComponent q.imaginaries
        }


identity : Quaternion
identity =
    quaternion 1 0 0 0


scalarMul : Float -> Quaternion -> Quaternion
scalarMul m { reals, imaginaries } =
    { reals = m * reals, imaginaries = Vector3d.multiplyBy m imaginaries }


plus : Quaternion -> Quaternion -> Quaternion
plus q0 q1 =
    { reals = q0.reals + q1.reals, imaginaries = Vector3d.plus q0.imaginaries q1.imaginaries }


mul : Quaternion -> Quaternion -> Quaternion
mul q1 q2 =
    { reals = q1.reals * q2.reals - Quantity.toFloat (normalizeProductUnits (q1.imaginaries |> Vector3d.dot q2.imaginaries))
    , imaginaries =
        Vector3d.scaleBy q2.reals q1.imaginaries
            |> Vector3d.plus (Vector3d.scaleBy q1.reals q2.imaginaries)
            |> Vector3d.plus (normalizeVectorUnits (Vector3d.cross q1.imaginaries q2.imaginaries))
    }


vecMul : Quaternion -> VecMeters -> VecMeters
vecMul q v =
    let
        qNorm =
            normalized q

        vQuaternion =
            vecToQuaternion v

        qConjugate =
            conjugate qNorm

        --
        qvq =
            mul (mul qConjugate vQuaternion) qNorm
    in
    quaternionToVec qvq


negate : Quaternion -> Quaternion
negate { reals, imaginaries } =
    { reals = -reals, imaginaries = Vector3d.reverse imaginaries }


conjugate : Quaternion -> Quaternion
conjugate { reals, imaginaries } =
    { reals = reals, imaginaries = Vector3d.reverse imaginaries }


magnitude : Quaternion -> Float
magnitude q =
    let
        v =
            Vector3d.toRecord Quantity.toFloat q.imaginaries
    in
    (q.reals ^ 2.0 + v.x ^ 2.0 + v.y ^ 2.0 + v.z ^ 2.0) ^ 0.5


normalized : Quaternion -> Quaternion
normalized q =
    let
        lengths =
            magnitude q
    in
    toList q
        |> List.map (\n -> n / lengths)
        |> fromList
        |> Maybe.withDefault identity



-- Convert quaternion to euler angle. Assumes xyz order (roll-pitch-yaw)


euler : Quaternion -> Vector3d Angle.Radians WorldCoordinates
euler q =
    let
        nq =
            normalized q

        q0 =
            nq.reals

        q1 =
            Quantity.toFloat <| Vector3d.xComponent nq.imaginaries

        q2 =
            Quantity.toFloat <| Vector3d.yComponent nq.imaginaries

        q3 =
            Quantity.toFloat <| Vector3d.zComponent nq.imaginaries

        xAngle =
            atan2 (2 * (q0 * q1 + q2 * q3)) (1 - 2 * (q1 * q1 + q2 * q2))

        yAngle =
            asin ((2 * (q0 * q2 - q3 * q1)) |> clamp -1 1)

        zAngle =
            atan2 (2 * (q0 * q3 + q1 * q2)) (1 - 2 * (q2 * q2 + q3 * q3))
    in
    Vector3d.xyz (Angle.radians xAngle) (Angle.radians yAngle) (Angle.radians zAngle)


angleAxis : Quaternion -> ( Angle, Vec )
angleAxis q =
    let
        nq =
            normalized q

        s =
            sqrt (1 - (nq.reals ^ 2.0))

        angles =
            Angle.radians <| 2.0 * acos nq.reals

        axis =
            nq.imaginaries |> Vector3d.divideBy s
    in
    ( angles, axis )


fromAngleAxis : ( Angle, Vec ) -> Quaternion
fromAngleAxis ( angles, axis ) =
    let
        normalizedAxis =
            Vector3d.normalize axis

        sines =
            angles
                |> Quantity.divideBy 2.0
                |> Angle.sin

        cosines =
            angles
                |> Quantity.divideBy 2.0
                |> Angle.cos
    in
    { reals = cosines, imaginaries = normalizedAxis |> Vector3d.multiplyBy sines }


fromEuler : ChannelOrder -> Vector3d Angle.Radians WorldCoordinates -> Quaternion
fromEuler ( c0, c1, c2 ) es =
    let
        orderToAxis order =
            case order of
                X ->
                    Vector3d.unitless 1 0 0

                Y ->
                    Vector3d.unitless 0 1 0

                Z ->
                    Vector3d.unitless 0 0 1

        q0s =
            fromAngleAxis ( Vector3d.xComponent es, orderToAxis c0 )

        q1s =
            fromAngleAxis ( Vector3d.yComponent es, orderToAxis c1 )

        q2s =
            fromAngleAxis ( Vector3d.zComponent es, orderToAxis c2 )
    in
    mul q0s (mul q1s q2s)


dot : Quaternion -> Quaternion -> Float
dot q0 q1 =
    q0.reals * q1.reals + Quantity.toFloat (normalizeProductUnits (Vector3d.dot q0.imaginaries q1.imaginaries))



-- Uses the Shoemake quaternion formula, with guards around edge cases


slerp : Float -> Quaternion -> Quaternion -> Quaternion
slerp a q0Inp q1Inp =
    let
        q0 =
            normalized q0Inp

        q1 =
            normalized q1Inp

        rawDot =
            dot q0 q1

        q1Shortest =
            if dotProd < 0 then
                negate q1

            else
                q1

        dotProd =
            abs rawDot

        theta =
            acos <| clamp dotProd -1 1

        component0 =
            scalarMul (Basics.sin ((1 - a) * theta) / Basics.sin theta) q0

        component1 =
            scalarMul (Basics.sin (a * theta) / Basics.sin theta) q1Shortest
    in
    if dotProd > 0.9995 then
        normalized <| plus (scalarMul (1 - a) q0) (scalarMul a q1Shortest)

    else
        plus component0 component1
