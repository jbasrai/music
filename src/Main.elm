port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model = Bool

type alias Msg = String


init : () -> (Model, Cmd Msg)
init flags =
  ( True
  , Cmd.none
  )


view : Model -> Document Msg
view model =
  Document
    "music "
    [ text "music"
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  -- case msg of
  ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
