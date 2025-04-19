module BVHParser exposing (..)

-- Reference: https://research.cs.wisc.edu/graphics/Courses/cs-838-1999/Jeff/BVH.html

import Angle
import AnimationTypes exposing (..)
import Array exposing (Array)
import Base exposing (..)
import Duration exposing (..)
import Parser exposing (..)
import Point3d
import Quaternions exposing (Quaternion)
import Vector3d exposing (at_)


runBVHParser =
    run parseBVH


parseBVH : Parser Animation
parseBVH =
    parseBVHJoints
        |> andThen
            (\( skeleton, chans ) ->
                parseMotionData chans
                    |> andThen
                        (\{ frames, frameTime } ->
                            succeed { skeleton = skeleton, frames = Array.fromList frames, frameTime = frameTime }
                        )
            )



-- Main parsing functions


parseBVHJoints : Parser ( SkeletonTree, List Channel )
parseBVHJoints =
    parseRootJointChannel
        |> andThen
            (\( rootJoint, rootChannel ) ->
                incIndent (loop ( Node rootJoint [], [ rootChannel ] ) parseChildren)
            )


parseJointsChannels : Parser ( SkeletonTree, List Channel )
parseJointsChannels =
    parseSingleJointChannel
        |> andThen (\( j, c ) -> incIndent <| loop ( Node j [], [ c ] ) parseChildren)


parseChildren : ( SkeletonTree, List Channel ) -> Parser (Step ( SkeletonTree, List Channel ) ( SkeletonTree, List Channel ))
parseChildren ( Node tree listChilds, cs ) =
    oneOf
        [ backtrackable
            (parseCloseBrace
                |. (getPosition |> andThen (\n -> Debug.log ("rowCloseBrace: " ++ Debug.toString n) (succeed ())))
                |> map (\_ -> Done ( Node tree listChilds, cs ))
            )
        , succeed (\( childTree, chan ) -> Loop ( Node tree (listChilds ++ [ childTree ]), cs ++ chan ))
            |. alignIndent
            |. (getPosition |> andThen (\n -> Debug.log ("rowParser: " ++ Debug.toString n) (succeed ())))
            |= parseJointsChannels
        , parseEndSite
            |. (getPosition |> andThen (\n -> Debug.log ("rowEndSite: " ++ Debug.toString n) (succeed ())))
            |> map (\_ -> Loop ( Node tree listChilds, cs ))
        ]



-- Parsing helpers for skeleton first part


parseRoot : Parser Joint
parseRoot =
    succeed mkJoint
        |= parseHeader
        |. parseOpenBrace
        |= incIndent parseOffset
        |. newline


parseSingleJoint : Parser Joint
parseSingleJoint =
    succeed mkJoint
        |= parseJoint
        |. parseOpenBrace
        |= incIndent parseOffset
        |. newline


parseSingleJointChannel : Parser ( Joint, Channel )
parseSingleJointChannel =
    succeed Tuple.pair
        |= parseSingleJoint
        |= incIndent parseChannel


parseRootJointChannel : Parser ( Joint, Channel )
parseRootJointChannel =
    succeed Tuple.pair
        |= parseRoot
        |= incIndent parseChannel


parseHeader : Parser String
parseHeader =
    succeed identity
        |. keyword "HIERARCHY"
        |. newline
        |. keyword "ROOT"
        |. whitespace
        |= parseJointName


parseOffset : Parser VecMeters
parseOffset =
    succeed Vector3d.meters
        |. spacesTabs
        |. keyword "OFFSET"
        |. whitespace
        |= parseFloat
        |. whitespace
        |= parseFloat
        |. whitespace
        |= parseFloat


parseChannel : Parser Channel
parseChannel =
    spacesTabs
        |. keyword "CHANNELS"
        |. spacesTabs
        |> andThen (\_ -> int |. spacesTabs)
        |> andThen
            (\n ->
                case n of
                    3 ->
                        succeed RotationChannel |= parseOrder

                    6 ->
                        succeed PositionRotationChannel
                            |= parseOrder
                            |= parseOrder

                    _ ->
                        problem "Only 3 or 6 channels are supported"
            )


parseOrder : Parser ChannelOrder
parseOrder =
    let
        mkTriple a b c =
            ( a, b, c )
    in
    oneOf
        [ succeed mkTriple
            |= parseAxisRotation
            |= parseAxisRotation
            |= parseAxisRotation
        , succeed mkTriple
            |= parseAxisPosition
            |= parseAxisPosition
            |= parseAxisPosition
        ]


parseJoint : Parser String
parseJoint =
    succeed identity
        |. keyword "JOINT"
        |. whitespace
        |= parseJointName


parseAxisRotation : Parser AxisName
parseAxisRotation =
    oneOf
        [ succeed X |. keyword "Xrotation"
        , succeed Y |. keyword "Yrotation"
        , succeed Z |. keyword "Zrotation"
        ]
        |. spacesTabs


parseAxisPosition : Parser AxisName
parseAxisPosition =
    oneOf
        [ succeed X |. keyword "Xposition"
        , succeed Y |. keyword "Yposition"
        , succeed Z |. keyword "Zposition"
        ]
        |. spacesTabs


parseEndSite : Parser VecMeters
parseEndSite =
    succeed identity
        |. keyword "End Site"
        |. newline
        |. parseOpenBrace
        |. newline
        |= incIndent parseOffset
        |. newline
        |. parseCloseBrace



-- Motion parsing helpers


parseMotionData chans =
    succeed (\( _, frmTime ) frmData -> { frames = frmData, frameTime = frmTime })
        |= parseMotionHeader
        |= parseMotionFrames chans


parseMotionHeader : Parser ( Int, Duration )
parseMotionHeader =
    succeed (\n frm -> ( n, Duration.seconds frm ))
        |. keyword "MOTION"
        |. newline
        |. keyword "Frames:"
        |. spacesTabs
        |= int
        |. newline
        |. keyword "Frame Time:"
        |. newline
        |= float


parseMotionFrames : List Channel -> Parser (List FrameData)
parseMotionFrames chans =
    loop [] (parseMotionFramesHelper chans)


parseMotionFramesHelper : List Channel -> List FrameData -> Parser (Step (List FrameData) (List FrameData))
parseMotionFramesHelper chans frmList =
    oneOf
        [ backtrackable <|
            succeed (\frmData -> Loop <| frmData :: frmList)
                |= parseMotionFrame chans
                |. (getPosition |> andThen (\pos -> Debug.log (Debug.toString pos) (succeed ())))
                |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse frmList))
        ]


parseMotionFrame : List Channel -> Parser FrameData
parseMotionFrame chans =
    case chans of
        (PositionRotationChannel pChans rChans) :: rest ->
            parseRootPosition pChans
                |> andThen
                    (\pos ->
                        loop ( RotationChannel rChans :: rest, [] ) parseMotionRotations
                            |> andThen (\qs -> succeed { position = pos, rotations = qs })
                    )

        -- Impossible cases
        (RotationChannel rChans) :: rest ->
            succeed { position = Vector3d.zero, rotations = [] }

        [] ->
            succeed { position = Vector3d.zero, rotations = [] }


type alias ParsingState =
    ( List Channel, List Quaternion )


parseMotionRotations : ParsingState -> Parser (Step ParsingState (List Quaternion))
parseMotionRotations ( chans, qs ) =
    case chans of
        [] ->
            succeed <| Done (List.reverse qs)

        (RotationChannel c) :: cs ->
            succeed (\q -> Loop ( cs, q :: qs ))
                |= parseRotation c

        -- Shouldn't happen
        (PositionRotationChannel pc rc) :: cs ->
            succeed <| Done (List.reverse qs)


parseRootPosition : ChannelOrder -> Parser VecMeters
parseRootPosition ( a0, a1, a2 ) =
    let
        toOrdered n0 n1 n2 =
            List.sort [ ( showAxisName a0, n0 ), ( showAxisName a1, n1 ), ( showAxisName a2, n2 ) ]
                |> List.unzip
                |> Tuple.second
                |> (\xs ->
                        case xs of
                            [ f0, f1, f2 ] ->
                                Vector3d.meters f0 f1 f2

                            _ ->
                                Vector3d.zero
                   )
    in
    succeed toOrdered
        |. spaces
        |= parseFloat
        |. spaces
        |= parseFloat
        |. spaces
        |= parseFloat


parseRotation : ChannelOrder -> Parser Quaternion
parseRotation order =
    let
        sortOrder r0 r1 r2 =
            case order of
                ( X, Y, Z ) ->
                    ( r0, r1, r2 )

                ( X, Z, Y ) ->
                    ( r0, r2, r1 )

                ( Y, X, Z ) ->
                    ( r1, r0, r2 )

                ( Y, Z, X ) ->
                    ( r1, r2, r0 )

                ( Z, X, Y ) ->
                    ( r2, r0, r1 )

                ( Z, Y, X ) ->
                    ( r2, r1, r0 )

                _ ->
                    ( r0, r1, r2 )

        toQuaternion ( r0, r1, r2 ) =
            Vector3d.xyz (Angle.degrees r0) (Angle.degrees r1) (Angle.degrees r2)
                |> Quaternions.fromEuler ( X, Y, Z )
    in
    succeed (\r0 r1 r2 -> toQuaternion <| sortOrder r0 r1 r2)
        |. spaces
        |= parseFloat
        |. spaces
        |= parseFloat
        |. spaces
        |= parseFloat



-- Utils


newline : Parser ()
newline =
    succeed ()
        |. spacesTabs
        |. chompWhile (\c -> c == '\n')


whitespace : Parser ()
whitespace =
    chompWhile (\c -> c == ' ')


parseJointName : Parser String
parseJointName =
    getChompedString <|
        succeed ()
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_')


parseOpenBrace : Parser ()
parseOpenBrace =
    succeed ()
        |. spacesTabs
        |. symbol "{"
        |. newline


parseCloseBrace : Parser ()
parseCloseBrace =
    succeed ()
        |. spacesTabs
        |. symbol "}"
        |. newline


parseFloat : Parser Float
parseFloat =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , float
        ]


incIndent : Parser a -> Parser a
incIndent p =
    getIndent
        |> andThen (\n -> withIndent (n + 2) p)


alignIndent : Parser ()
alignIndent =
    getIndent
        |> andThen
            (\ind ->
                getCol
                    |> andThen
                        (\col ->
                            if col < ind then
                                whitespace

                            else
                                succeed ()
                        )
            )


spacesTabs : Parser ()
spacesTabs =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')
