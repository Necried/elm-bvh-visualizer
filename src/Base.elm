module Base exposing (..)

import Axis3d exposing (z)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Quantity exposing (Product, Quantity, Unitless)
import Vector3d exposing (Vector3d)


type WorldCoordinates
    = WorldCoordinates


type RoseTree a
    = Node a (List (RoseTree a))


type alias Point =
    Point3d Meters WorldCoordinates


type alias VecMeters =
    Vector3d Meters WorldCoordinates


type alias Vec =
    Vector3d Unitless WorldCoordinates


type AxisName
    = X
    | Y
    | Z


type alias ChannelOrder =
    ( AxisName, AxisName, AxisName )


type Channel
    = RotationChannel ChannelOrder
    | PositionRotationChannel ChannelOrder ChannelOrder



-- Helpers


showAxisName : AxisName -> String
showAxisName axis =
    case axis of
        X ->
            "X"

        Y ->
            "Y"

        Z ->
            "Z"


normalizeProductUnits : Quantity Float (Product Unitless Unitless) -> Quantity Float Unitless
normalizeProductUnits p =
    Quantity.over (Quantity.float 1) p


normalizeVectorUnits : Vector3d (Product Unitless Unitless) WorldCoordinates -> Vector3d Unitless WorldCoordinates
normalizeVectorUnits v =
    Vector3d.over (Quantity.float 1) v


vecMap : (Float -> Float) -> Vec -> Vec
vecMap f vec =
    let
        ( x, y, z ) =
            Vector3d.components vec
    in
    Vector3d.unitless (f (Quantity.toFloat x)) (f (Quantity.toFloat y)) (f (Quantity.toFloat z))



-- NOTE: This is only used for converting vector representations to starting point locations
-- for shapes.


vecToPoint : VecMeters -> Point
vecToPoint vec =
    let
        ( x, y, z ) =
            Vector3d.components vec
    in
    Point3d.xyz x y z


vecUnitlessToPoint : Vec -> Point
vecUnitlessToPoint vec =
    let
        { x, y, z } =
            Vector3d.unwrap vec
    in
    Point3d.xyz (Length.meters x) (Length.meters y) (Length.meters z)


pointToVec : Point -> VecMeters
pointToVec p =
    let
        { x, y, z } =
            Point3d.unwrap p
    in
    Vector3d.xyz (Length.meters x) (Length.meters y) (Length.meters z)


sumVectors : List (Vector3d units coordinates) -> Vector3d units coordinates
sumVectors =
    List.foldr Vector3d.plus Vector3d.zero


treeSize : RoseTree a -> Int
treeSize (Node _ forest) =
    1 + (List.sum <| List.map treeSize forest)

mapRoseTree : (a -> b) -> RoseTree a -> RoseTree b
mapRoseTree f (Node a children) =
    Node (f a) <| List.map (mapRoseTree f) children

vecMin : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
vecMin v0 v1 =
    let 
        w0 = Vector3d.unwrap v0
        w1 = Vector3d.unwrap v1
    in Vector3d.unsafe { x = min w0.x w1.x, y = min w0.y w1.y, z = min w0.z w1.z }


vecMax : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
vecMax v0 v1 =
    let 
        w0 = Vector3d.unwrap v0
        w1 = Vector3d.unwrap v1
    in Vector3d.unsafe { x = max w0.x w1.x, y = max w0.y w1.y, z = max w0.z w1.z }

ninfVec : VecMeters
ninfVec =
    let ninf = Length.meters (-1/0)
    in Vector3d.xyz ninf ninf ninf

infVec : VecMeters
infVec =
    let inf = Length.meters (1/0)
    in Vector3d.xyz inf inf inf

vecDistanceFrom : VecMeters -> VecMeters -> Length.Length
vecDistanceFrom v0 v1 =
    let
        dx = Length.inMeters (Vector3d.xComponent v1) - Length.inMeters (Vector3d.xComponent v0)
        dy = Length.inMeters (Vector3d.yComponent v1) - Length.inMeters (Vector3d.yComponent v0)
        dz = Length.inMeters (Vector3d.zComponent v1) - Length.inMeters (Vector3d.zComponent v0)
    in
        Length.meters (sqrt (dx^2 + dy^2 + dz^2))