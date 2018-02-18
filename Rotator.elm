import Html exposing (Html, div, text, span, button, h1, input, label)
import Html.Events exposing (onClick, onCheck)
import Html.Attributes exposing (type_, checked)
import Dict
import Debug exposing (log)
import Random
import Random.List
import Note exposing (Note, NoteString)
import Maybe
import Time exposing (Time)

-- TODO: pseudo random
-- TODO: save settings in local storage
-- TODO: styles & ui

main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- INIT

type alias Voicing =
  { root : Note
  }

type alias Model =
  { selectedRoots : List Note
  , voicing : Voicing
  , tempo : Int
  , counts : Int
  , lastTick : Time
  , shouldSync : Bool
  , isPaused : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { selectedRoots = Note.all
    , voicing = Voicing Note.Bb
    , tempo = 60
    , counts = 2
    , lastTick = 0
    , shouldSync = False
    , isPaused = False
    }
  , Random.generate NextVoicing (randomVoicing Note.all)
  )



-- UPDATE


type Msg
  = NextVoicing Voicing
  | ToggleRoot Note Bool
  | ToggleAllRoots Bool
  | Tick Time
  | Sync
  | Pause
  | IncrementCount
  | DecrementCount
  | IncrementTempo
  | DecrementTempo

randomNote : List Note -> Random.Generator Note
randomNote includes =
  Random.List.choose includes
  |> Random.map (\(note, _) -> Maybe.withDefault Note.Bb note)

randomVoicing : List Note -> Random.Generator Voicing
randomVoicing includeNotes =
  Random.map Voicing (randomNote includeNotes)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  NextVoicing voicing ->
    ( { model | voicing = voicing }
    , Cmd.none
    )

  ToggleRoot root checked ->
    if (checked) then
      ( { model | selectedRoots = root :: model.selectedRoots }
      , Cmd.none
      )
    else
      ( { model | selectedRoots = List.filter (\e -> e /= root) model.selectedRoots }
      , Cmd.none
      )

  ToggleAllRoots checked ->
    if (checked) then
      ( { model | selectedRoots = Note.all }
      , Cmd.none
      )
    else 
      ( { model | selectedRoots = [] }
      , Cmd.none
      )

  Tick time ->
    let
        tempo = toFloat model.tempo
        counts = toFloat model.counts

        duration : Time
        duration = Time.second * 60 / tempo * counts

        rotateAt : Time
        rotateAt = model.lastTick + duration
    in
        if (model.lastTick == 0) then
          ( { model | lastTick = time }
          , Cmd.none
          )
        else if (model.shouldSync) then
          ( { model |
                lastTick = time,
                shouldSync = False
            }
          , Random.generate NextVoicing (randomVoicing model.selectedRoots)
          )
        else if (not model.isPaused && time >= rotateAt) then
          ( { model | lastTick = rotateAt }
          , Random.generate NextVoicing (randomVoicing model.selectedRoots)
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

  IncrementTempo ->
    ( { model | tempo = model.tempo + 2 }
    , Cmd.none
    )

  DecrementTempo ->
    ( { model | tempo = model.tempo - 2 }
    , Cmd.none
    )

-- VIEW

renderRootCheckbox : List Note -> Note -> Html Msg
renderRootCheckbox selectedRoots root = span []
  [ input
      [ type_ "checkbox"
      , checked <| List.member root selectedRoots
      , onCheck (ToggleRoot root)
      ] []
  , label [] [text <| String.join " / " (Note.toString root)]
  ]

renderRoots : List Note -> Html Msg
renderRoots selectedRoots =
  let
      allRoots = span []
        [ input
          [ type_ "checkbox"
          , checked <| List.length selectedRoots == 12
          , onCheck ToggleAllRoots
          ] []
        , label [] [text "Roots"]
        ]

      roots =
        List.map (renderRootCheckbox selectedRoots) Note.all
  in
      div [] (allRoots :: roots)

view : Model -> Html Msg
view model = div []
  [ renderRoots model.selectedRoots
  , h1 [] [text <| toString model.voicing]
  , div []
    [ button [onClick DecrementTempo] [text "-"]
    , text (toString model.tempo)
    , button [onClick IncrementTempo] [text "+"]
    ]
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
