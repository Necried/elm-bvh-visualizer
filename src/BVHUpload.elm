module BVHUpload exposing (..)

import AnimationTypes exposing (..)
import BVHParser exposing (runBVHParser)
import Base exposing (..)
import Browser
import Duration
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Parser
import Result exposing (..)
import Task
import Time exposing (Posix)



-- UPDATE


type Msg
    = BvhRequested
    | BvhSelected File
    | BvhLoaded String
    | StartTime String Posix
    | StopTime Posix


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        BvhRequested ->
            ( model
            , Select.file [ "text/bvh" ] BvhSelected
            )

        BvhSelected file ->
            ( model
            , Task.perform BvhLoaded (File.toString file)
            )

        BvhLoaded content ->
            ( model, Task.perform (StartTime content) Time.now )

        StartTime content t ->
            case runBVHParser content of
                Err error ->
                    ( { model | errorMessage = Just (Debug.toString error) }, Cmd.none )

                Ok anim ->
                    ( { model
                        | animationData = Just anim
                        , parseTimer = Started t
                      }
                    , Task.perform StopTime Time.now
                    )

        StopTime t1 ->
            case model.parseTimer of
                Started t0 ->
                    ( { model | parseTimer = Ended <| Duration.from t0 t1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : AppModel -> Html Msg
view model =
    case model.errorMessage of
        Nothing -> button [ onClick BvhRequested ] [ text "Load BVH" ]
        Just err -> text err