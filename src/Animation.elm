module Animation exposing (..)

{-| The example uses an onAnimationFrameDelta subscription to implement a simple
loading spinner, and shows how you can incorporate elm-3d-scene into an elm-ui
layout.
-}

import Angle exposing (Angle)
import AnimationTypes exposing (..)
import Array exposing (Array)
import Axis3d
import BVHUpload exposing (..)
import Base exposing (..)
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cylinder3d
import Debug
import Direction3d
import Duration exposing (Duration)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Frame3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Quantity
import Quaternions exposing (Quaternion)
import Scene3d
import Scene3d.Material as Material
import State exposing (State)
import Vector3d
import Viewpoint3d


{-| Receive a Tick message on every animation frame with elapsed duration since
last frame (should usually be around 16 milliseconds)
-}
type Msg
    = Tick Duration
    | FileUpload BVHUpload.Msg
    | SetFrame Float Int
    | PauseAnimation
    | PlayAnimation
    | ToggleDropdown


main : Program () AppModel Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( AppModel, Cmd Msg )
init () =
    ( { animationData = Nothing
      , errorMessage = Nothing
      , time = Duration.seconds 0
      , fps = 0
      , animationStatus = Play
      , parseTimer = Uninitialized
      , dropdownOpen = False
      }
    , Cmd.none
    )


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        Tick duration ->
            case model.animationData of
                Nothing ->
                    ( model, Cmd.none )

                Just { skeleton, frames, frameTime } ->
                    let
                        newTime =
                            Duration.inSeconds model.time + Duration.inSeconds duration

                        newFps =
                            floor (1 / Quantity.unwrap duration)
                    in
                    case model.animationStatus of
                        Play ->
                            ( { model | time = Duration.seconds newTime, fps = newFps }, Cmd.none )

                        Pause ->
                            ( model, Cmd.none )

        FileUpload bvhMsg ->
            BVHUpload.update bvhMsg model
                |> Tuple.mapSecond (Cmd.map FileUpload)

        SetFrame frameTime idx ->
            let
                newTime =
                    Duration.seconds (frameTime * toFloat idx)
            in
            ( { model | time = newTime }, Cmd.none )

        PauseAnimation ->
            ( { model | animationStatus = Pause }, Cmd.none )

        PlayAnimation ->
            ( { model | animationStatus = Play }, Cmd.none )

        ToggleDropdown ->
            ( { model | dropdownOpen = not model.dropdownOpen }, Cmd.none )


subscriptions : AppModel -> Sub Msg
subscriptions model =
    -- Subscribe to animation frames and wrap each time step (a number of
    -- milliseconds) into a Duration value and then into a Tick message
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


lerp : VecMeters -> VecMeters -> Float -> VecMeters
lerp v0 v1 a =
    Vector3d.scaleBy (1 - a) v0 |> Vector3d.plus (Vector3d.scaleBy a v1)


propagateTree : SkeletonTree -> List Quaternion -> ( SkeletonWithRotationTree, List Quaternion )
propagateTree (Node joint children) qs =
    case qs of
        [] ->
            ( Node ( joint, Quaternions.identity ) [], [] )

        q :: rots ->
            let
                ( newChildren, rest ) =
                    propagateForest children rots
            in
            ( Node ( joint, q ) newChildren, rest )


propagateForest : List SkeletonTree -> List Quaternion -> ( List SkeletonWithRotationTree, List Quaternion )
propagateForest trees qs =
    case trees of
        [] ->
            ( [], qs )

        t :: ts ->
            let
                ( newTree, newRots ) =
                    propagateTree t qs

                ( newForest, newRots1 ) =
                    propagateForest ts newRots
            in
            ( newTree :: newForest, newRots1 )


propagateRotation : SkeletonTree -> List Quaternion -> SkeletonWithRotationTree
propagateRotation skel rots =
    Tuple.first <| propagateTree skel rots



-- TODO: This is also calculated when rendering the skeleton, so how do we
-- merge this into renderSkeleton?


worldPosition : SkeletonWithRotationTree -> List ( String, VecMeters )
worldPosition (Node ( { name, offset }, rot ) children) =
    ( name, offset ) :: List.concatMap (worldPositionHelper ( offset, rot )) children


worldPositionHelper : ( VecMeters, Quaternion ) -> SkeletonWithRotationTree -> List ( String, VecMeters )
worldPositionHelper ( parentPos, parentOrient ) (Node ( { name, offset }, rot ) children) =
    let
        newRootPos =
            -- sumVectors parentOffsets
            Vector3d.plus parentPos (Quaternions.vecMul parentOrient offset)

        newRootRot =
            Quaternions.normalized <| Quaternions.mul rot parentOrient
    in
    ( name, newRootPos ) :: List.concatMap (worldPositionHelper ( newRootPos, newRootRot )) children


renderSkeleton : SkeletonWithRotationTree -> List (Scene3d.Entity WorldCoordinates)
renderSkeleton (Node ( { name, offset }, rot ) children) =
    List.concatMap (renderSkeletonHelper ( offset, rot )) children


renderSkeletonHelper : ( VecMeters, Quaternion ) -> SkeletonWithRotationTree -> List (Scene3d.Entity WorldCoordinates)
renderSkeletonHelper ( parentPos, parentOrient ) (Node ( { name, offset }, rot ) children) =
    let
        newRootPos =
            -- sumVectors parentOffsets
            Vector3d.plus parentPos (Quaternions.vecMul parentOrient offset)

        newRootRot =
            Quaternions.normalized <| Quaternions.mul rot parentOrient

        -- endingVec =
        --    worldPosition parentOffsets parentOrients offset
        -- newParentOffsets = offset :: parentOffsets
        -- newOrients = rot :: parentOrients
        mkJoint =
            List.map (Scene3d.cylinder (Material.color Color.blue)) <|
                Maybe.withDefault [] <|
                    Maybe.map List.singleton <|
                        Cylinder3d.from
                            (vecToPoint parentPos)
                            (vecToPoint newRootPos)
                            -- <| Vector3d.plus startingVec offset)
                            (Length.cssPixels 20)
    in
    mkJoint ++ List.concatMap (renderSkeletonHelper ( newRootPos, newRootRot )) children



-- Given the current time and the frame time of the animation,
-- calculate which frame data to index into and the parameter which
-- is between 0 and 1, which will be used for interpolation


frameIndex : Duration -> Duration -> ( Int, Float )
frameIndex timeElapsed frameTime =
    let
        idx =
            floor (Duration.inSeconds timeElapsed / Duration.inSeconds frameTime)

        q1 =
            toFloat idx * Duration.inSeconds frameTime

        q2 =
            toFloat (idx + 1) * Duration.inSeconds frameTime

        alpha =
            (Duration.inSeconds timeElapsed - q1) / (q2 - q1)
    in
    ( idx, alpha )


viewSkeletonInfo : AppModel -> SkeletonWithRotationTree -> Element.Element Msg
viewSkeletonInfo model skeletonTree =
    let
        showVector { x, y, z } =
            "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ "," ++ String.fromFloat z ++ ")"

        viewJointInfo : ( String, VecMeters ) -> Element.Element Msg
        viewJointInfo ( name, worldPos ) =
            Element.text <| name ++ showVector (Vector3d.unwrap worldPos)
    in
    Element.column [ Element.width Element.fill ]
        [ Element.el
            [ Background.color (Element.rgb255 240 240 240)
            , Element.padding 10
            , Border.rounded 5
            , Border.width 1
            , Border.color (Element.rgb255 200 200 200)
            , Element.mouseOver [ Background.color (Element.rgb255 230 230 230) ]
            , onClick ToggleDropdown
            ]
            (if model.dropdownOpen then
                Element.text "Hide Animation Info"

             else
                Element.text "Show Animation Info"
            )
        , if model.dropdownOpen then
            Element.column
                [ Background.color (Element.rgb255 255 255 255)
                , Border.widthEach { top = 0, bottom = 1, left = 1, right = 1 }
                , Border.color (Element.rgb255 200 200 200)
                , Border.rounded 5
                , Element.spacing 5
                , Element.padding 10
                ]
            <|
                Element.text "World Positions: "
                    :: ((List.map viewJointInfo <| worldPosition skeletonTree)
                            ++ Element.text "Local Positions and Rotations"
                            :: viewSkeletonInfoHelper skeletonTree
                       )

          else
            Element.none
        ]


viewSkeletonInfoHelper : SkeletonWithRotationTree -> List (Element.Element Msg)
viewSkeletonInfoHelper (Node ( { name, offset }, q ) children) =
    let
        showVector { x, y, z } =
            "(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ "," ++ String.fromFloat z ++ ")"

        toDegrees x =
            x * 180.0 / pi

        convertToDegrees { x, y, z } =
            { x = toDegrees x, y = toDegrees y, z = toDegrees z }

        showOffset =
            Vector3d.toMeters offset
                |> showVector

        showRotations =
            "Quaternion: (" ++ String.fromFloat q.reals ++ ", " ++ showVector (q.imaginaries |> Vector3d.unwrap) ++ ")"

        -- Quaternions.euler q
        --     |> Vector3d.unwrap
        --     |> convertToDegrees
        --    |> showVector
        jointInfo =
            Element.text (name ++ ": " ++ showOffset ++ " " ++ showRotations)
    in
    jointInfo :: List.concatMap viewSkeletonInfoHelper children


drawGrid : Point3d.Point3d Length.Meters coordinates -> Float -> Scene3d.Entity coordinates
drawGrid center step =
    let
        size =
            10

        centerX = Length.inMeters (Point3d.xCoordinate center)
        centerZ = Length.inMeters (Point3d.zCoordinate center)

        range = List.range -size size

        positions : List ( Float, Float )
        positions =
            List.concatMap
                (\dx ->
                    List.map (\dz -> ( centerX + toFloat dx * step, centerZ + toFloat dz * step ))
                        range
                )
                range

        gridDot : Float -> Float -> Scene3d.Entity coordinates
        gridDot x z =
            let
                dotCenter =
                    Frame3d.atPoint <| Point3d.meters x 0 z

                blockShape =
                    Block3d.centeredOn dotCenter
                        ( Length.meters 0.05
                        , Length.meters 0.01
                        , Length.meters 0.05
                        )
                
                blockColor = if (x, z) == (0, 0) then Color.red else Color.gray
            in
            Scene3d.block (Material.color blockColor) blockShape
    in
    Scene3d.group (List.map (\( x, z ) -> gridDot x z) positions)

boundingBox : List VecMeters -> (VecMeters, VecMeters)
boundingBox positions =
    List.foldl
        (\v (min, max) ->
            ( vecMin min v
            , vecMax max v
            )
        )
        (infVec, ninfVec)
        positions


boundingRadius : List VecMeters -> Length.Length
boundingRadius positions =
    let
        (min, max) = boundingBox positions
        center = Vector3d.scaleBy 0.5 (Vector3d.plus min max)
        distances = List.map (\p -> vecDistanceFrom center p) positions
    in
        Quantity.maximum distances |> Maybe.withDefault (Length.meters 1)

computeCameraOffset : Length.Length -> Angle -> VecMeters
computeCameraOffset r fov =
    let
        dist =
            (Length.inMeters r / tan (Angle.inRadians fov / 2))

        direction =
            Vector3d.normalize (Vector3d.meters 1 1 1)
    in
        Vector3d.scaleBy dist direction
        |> Vector3d.toUnitless
        |> Vector3d.fromMeters

view : AppModel -> Html Msg
view model =
    case model.animationData of
        Nothing ->
            BVHUpload.view model |> Html.map FileUpload

        Just { skeleton, frames, frameTime } ->
            let

                sceneBase entityList =
                    Scene3d.unlit
                        { camera = camera
                        , dimensions = ( Pixels.int 1920, Pixels.int 1080 )
                        , entities = drawGrid rootPosOrigin scaleFactor :: entityList
                        , clipDepth = Length.cssPixels 10
                        , background = Scene3d.transparentBackground
                        }

                numFrames =
                    Array.length frames

                ( rotIdxUnbounded, alpha ) =
                    frameIndex model.time frameTime

                rotIdx =
                    clamp 0 (numFrames - 1) rotIdxUnbounded

                mframe0 =
                    Array.get rotIdx frames

                mframe1 =
                    Array.get (rotIdx + 1) frames

                rootPosOrigin = Point3d.origin
                    -- case Array.get 0 frames of
                    --    Nothing -> Point3d.origin
                    --    Just frame0 -> vecToPoint <| Vector3d.scaleBy scaleFactor <| frame0.position

                rootPos0 =
                    case mframe0 of
                        Nothing ->
                            Vector3d.zero

                        Just frame0 ->
                            frame0.position

                rootPos1 =
                    case mframe1 of
                        Nothing ->
                            rootPos0

                        Just frame1 ->
                            frame1.position

                rootPos =
                    lerp rootPos0 rootPos1 alpha

                rotData : List Quaternion
                rotData =
                    case ( mframe0, mframe1 ) of
                        ( Just frame0, Just frame1 ) ->
                            List.map2 (Quaternions.slerp alpha) frame0.rotations frame1.rotations

                        ( Just frame0, Nothing ) ->
                            frame0.rotations

                        -- Impossible case
                        _ ->
                            []


                cameraOffset = (Vector3d.xyz (Length.meters 3) (Length.meters 3) (Length.meters 3))
                cameraDistance = vecDistanceFrom (Vector3d.plus rootPos cameraOffset) rootPos

                verticalFOV = Angle.degrees 60
                halfFOV = Angle.radians (Angle.inRadians verticalFOV / 2)
                visibleHeight = Quantity.timesUnitless (Quantity.float <| Angle.tan halfFOV * 2) cameraDistance
                targetRadius = Quantity.timesUnitless (Quantity.float 0.4) visibleHeight 
                scaleFactor = Debug.log "factor" <|
                    Length.inMeters targetRadius / 
                    Length.inMeters (boundingRadius <| List.map Tuple.second <| worldPosition treeDataWithRotations)
                
                camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.lookAt
                                { eyePoint = vecToPoint <| Vector3d.plus rootPos (computeCameraOffset targetRadius verticalFOV)
                                , focalPoint = vecToPoint rootPos
                                , upDirection = Direction3d.positiveY
                                }
                        , verticalFieldOfView = verticalFOV
                        }

                treeDataWithRotations =
                    propagateRotation skeleton rotData
                        |> modifyRootPosition rootPos

                appOffset : (Joint, Quaternion) -> (Joint, Quaternion)
                appOffset ({name, offset}, rot) = 
                    (mkJoint name (Vector3d.scaleBy scaleFactor offset), rot)

                scaledtreeDataWithRotations =
                    let (Node ({name, offset}, rot) children) = treeDataWithRotations
                    in Node ({name = name, offset = offset}, rot) <| List.map (mapRoseTree appOffset) children

                fpsDisplay =
                    Element.text ("FPS: " ++ String.fromInt model.fps)

                frameDisplay =
                    Element.text ("Frame: " ++ String.fromInt rotIdx)

                frameSlider =
                    Input.slider
                        [ Border.solid
                        , Border.color (Element.rgb255 127 127 127)
                        , Border.width 1
                        , Border.rounded 9
                        ]
                        { onChange = floor >> SetFrame (Duration.inSeconds frameTime)
                        , label = Input.labelLeft [] (Element.text "Frame:")
                        , min = 0
                        , max = toFloat numFrames
                        , value = clamp 0 (toFloat numFrames) (toFloat rotIdx)
                        , thumb = Input.defaultThumb
                        , step = Just 1
                        }

                red =
                    Element.rgb 1 0 0

                green =
                    Element.rgb 0 1 0

                pauseButton =
                    Input.button
                        [ Background.color red ]
                        { onPress = Just PauseAnimation, label = Element.text "Pause" }

                playButton =
                    Input.button
                        [ Background.color green ]
                        { onPress = Just PlayAnimation, label = Element.text "Play" }

                parseDisplay =
                    case model.parseTimer of
                        Ended t ->
                            Element.text <| "Parse time: " ++ (String.fromFloat <| Duration.inSeconds t) ++ " s"

                        _ ->
                            Element.none
            in
            Element.layout [] <|
                Element.column [ Element.spacing 3 ] <|
                    [ Element.row [ Element.spacing 3 ] <| [ fpsDisplay, frameDisplay, parseDisplay ]
                    , Element.row [ Element.spacing 3 ] <| [ pauseButton, playButton ]
                    , frameSlider
                    , Element.html <| sceneBase (renderSkeleton scaledtreeDataWithRotations)
                    , viewSkeletonInfo model scaledtreeDataWithRotations
                    ]
