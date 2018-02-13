import Html exposing (Html, div, text, span, button, h1, input)
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
  , counts : Int
  , lastTick : Time
  , shouldSync : Bool
  , isPaused : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { key = Note.Bb
    , counts = 2
    , lastTick = 0
    , shouldSync = False
    , isPaused = False
    }
  , Random.generate NextNote (randomNote Note.Bb)
  )



-- UPDATE


type Msg = RandomNote | NextNote Note | Tick Time | Sync | Pause | IncrementCount | DecrementCount

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
    let
        delta = time - model.lastTick

        leak =
          let
              x = delta - (toFloat model.counts * 1000)
          in
              if (x > 1000) then 0 else x
    in
        if (not model.isPaused
          && (
            model.shouldSync
            || Time.inSeconds delta >= toFloat model.counts
          )
        ) then
          ( { model |
                lastTick = time - (log "leak" leak),
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


  IncrementCount ->
    ( { model | counts = model.counts + 1 }
    , Cmd.none
    )

  DecrementCount ->
    ( { model | counts = model.counts - 1 }
    , Cmd.none
    )

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [text (Note.toString model.key)]
    , div []
      [ button [onClick DecrementCount] [text "-"]
      , text (toString model.counts)
      , button [onClick IncrementCount] [text "+"]
      ]
    , div [] [text (if (model.isPaused) then "Paused" else "Playing")]
    , button [onClick Sync] [text "sync"]
    , button [onClick Pause] [text "pause"]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (50 * Time.millisecond) Tick