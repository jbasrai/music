import Html exposing (Html, div, text, span, button, h1)
import Html.Events exposing (onClick)
import Dict
import Debug exposing (log)
import Random
import Random.List
import Note exposing (Note)
import Maybe

-- TODO: pseudo random


main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- INIT
type alias Model =
  { key : Note
  }


init : (Model, Cmd Msg)
init =
  ( Model Note.Bb
  , Random.generate NextNote (randomNote Note.Bb)
  )



-- UPDATE


type Msg = RandomNote | NextNote Note

randomNote : Note -> Random.Generator Note
randomNote exclude =
  Note.all
  |> List.filter (\note -> note /= exclude)
  |> Random.List.choose
  |> Random.map (\(note, _) -> Maybe.withDefault Note.Db note)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  RandomNote ->
    ( model
    , Random.generate NextNote (randomNote model.key)
    )

  NextNote note ->
    ( Model note
    , Cmd.none
    )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text (Note.toString model.key)]
    , button [onClick RandomNote] [text "next"]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
