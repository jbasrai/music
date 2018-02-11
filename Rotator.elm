import Html exposing (Html, div, text, span, button, h1)
import Html.Events exposing (onClick)
import Dict
import Debug exposing (log)
import Random
import Random.List
import Note exposing (Note)
import Maybe
import Time exposing (Time)

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
  , lastTick : Time
  , shouldSync : Bool
  , isPaused : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { key = Note.Bb
    , lastTick = 0
    , shouldSync = False
    , isPaused = False
    }
  , Random.generate NextNote (randomNote Note.Bb)
  )



-- UPDATE


type Msg = RandomNote | NextNote Note | Tick Time | Sync | Pause

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
    ( { model | key = note }
    , Cmd.none
    )

  Tick time ->
    if (not model.isPaused
      && (
        model.shouldSync
        || Time.inSeconds (time - model.lastTick) >= 3
      )
    ) then
      ( { model |
            lastTick = time,
            shouldSync = False
        }
      , Random.generate NextNote (randomNote model.key)
      )
    else 
      (model, Cmd.none)

  Sync ->
    ( { model |
          shouldSync = True,
          isPaused = False
      }
    , Cmd.none
    )

  Pause ->
    ( { model | isPaused = True }
    , Cmd.none
    )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text (Note.toString model.key)]
    , div [] [text (if (model.isPaused) then "Paused" else "Playing")]
    , button [onClick Sync] [text "sync"]
    , button [onClick Pause] [text "pause"]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (50 * Time.millisecond) Tick
