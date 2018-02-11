import Html exposing (Html, div, text, span, button)
import Dict
import Debug exposing (log)
import Random
import Random.List exposing (shuffle)
import Chord exposing (Chord)


main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- INIT
type alias Model =
  { chords : List Chord
  }


init : (Model, Cmd Msg)
init =
  (Model Chord.all, Random.generate NewChord (Random.List.shuffle Chord.all))



-- UPDATE


type Msg = RandomChords | NewChord (List Chord)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RandomChords ->
    ( model
    , Random.generate NewChord (Random.List.shuffle model.chords)
    )

  NewChord chords ->
    (Model chords, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div [] [text (toString model.chords)]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
