port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)


{--

controls:
- chords
- bpm
- beats

display:
chord

--}


-- I don't immediately see what types get me here and unstructured list seems good enough?
-- Let's see what happens.
type alias Chord = String
type alias Voicing = String

allVoicings : Dict Chord (List Voicing)
allVoicings =
  Dict.fromList
    [ ( "minMaj7"
      , [ "rootPosition"
        , "firstInversion"
        , "secondInversion"
        , "thirdInversion"
        ]
      )

    , ( "aug7"
      , [ "rootPosition"
        , "halfInversion"
        ]
      )
    ]


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
  let
      renderPage : Html Msg
      renderPage = 
        div
          [ style "height" "100%"
          , style "display" "flex"
          ]
          [ renderVoicingPicker
          ]

      renderVoicingPicker : Html Msg
      renderVoicingPicker =
        allVoicings
        |> Dict.map renderChordAndVoicings
        |> Dict.values
        |> div
            [ style "border-right" "3px solid black"
            , style "width" "200px"
            , style "display" "flex"
            , style "flex-direction" "column"
            ]

      renderChordAndVoicings : Chord -> List Voicing -> Html Msg
      renderChordAndVoicings chord voicings =
        div 
          [ style "border-bottom" "3px solid black"
          ]
          [ text chord ]

      -- renderVoicings : List Voicing -> Html Msg
  in
    Document
      "music "
      [ renderPage
      ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  -- case msg of
  ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
