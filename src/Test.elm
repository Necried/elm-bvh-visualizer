module Test exposing (..)

import Quaternions exposing (quaternion)


test1 : Bool
test1 =
    (quaternion 1 0 1 0 |> Quaternions.mul (quaternion 1 0.5 0.5 0.75))
        == quaternion 0.5 1.25 1.5 0.25


testCases : List Bool
testCases =
    [ test1 ]
