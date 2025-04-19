module AnimationTypes exposing (..)

import Array exposing (Array)
import Base exposing (..)
import Duration exposing (Duration)
import Point3d exposing (Point3d)
import Quaternions exposing (Quaternion)
import Time exposing (Posix)


type alias Animation =
    { skeleton : SkeletonTree, frames : Array FrameData, frameTime : Duration }


type alias SkeletonTree =
    RoseTree Joint


type alias SkeletonWithRotationTree =
    RoseTree ( Joint, Quaternion )


type alias Joint =
    { name : String, offset : VecMeters }


type alias FrameData =
    { position : VecMeters, rotations : List Quaternion }


type RotationTree
    = RotationTree Quaternion (List Quaternion)


type Status
    = Pause
    | Play


type ParseTimer
    = Uninitialized
    | Started Posix
    | Ended Duration


type alias AppModel =
    { animationData : Maybe Animation
    , errorMessage : Maybe String
    , time : Duration
    , fps : Int
    , animationStatus : Status
    , parseTimer : ParseTimer
    , dropdownOpen : Bool 
    }


mkJoint : String -> VecMeters -> Joint
mkJoint name offset =
    { name = name, offset = offset }


mapSkeleton : (VecMeters -> VecMeters) -> SkeletonTree -> SkeletonTree
mapSkeleton f (Node { name, offset } children) =
    Node { name = name, offset = f offset } (List.map (mapSkeleton f) children)


modifyRootPosition : VecMeters -> SkeletonWithRotationTree -> SkeletonWithRotationTree
modifyRootPosition newOffset (Node ( { name, offset }, rot ) children) =
    Node ( { name = name, offset = newOffset }, rot ) children
