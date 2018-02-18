import Html exposing (Html, div, text, span, button, h1, input, label)
import Html.Events exposing (onClick, onCheck)
import Html.Attributes exposing (type_, checked)
import Dict
import Debug exposing (log)
import Random
import Random.List
import Note exposing (Note, NoteString)
import Chord exposing (Chord)
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
  { rootString : Maybe NoteString
  , root : Maybe Note
  , chord : Maybe Chord
  }

type alias Model =
  { selectedRoots : List Note
  , selectedChords : List Chord
  , voicing : Maybe Voicing
  , tempo : Int
  , counts : Int
  , lastTick : Time
  , shouldSync : Bool
  , isPaused : Bool
  }


init : (Model, Cmd Msg)
init =
  ( { selectedRoots = Note.all
    , selectedChords = Chord.all
    , voicing = Nothing
    , tempo = 60
    , counts = 2
    , lastTick = 0
    , shouldSync = False
    , isPaused = False
    }
  , Random.generate NextVoicing (randomVoicing Note.all Chord.all)
  )



-- UPDATE


type Msg
  = NextVoicing Voicing
  | NextVoicing2 Voicing (Maybe NoteString)
  | ToggleRoot Note Bool
  | ToggleChord Chord Bool
  | ToggleAllRoots Bool
  | ToggleAllChords Bool
  | Tick Time
  | Sync
  | Pause
  | IncrementCount
  | DecrementCount
  | IncrementTempo
  | DecrementTempo

randomNote : List Note -> Random.Generator (Maybe Note)
randomNote includes =
  Random.List.choose includes
  |> Random.map Tuple.first

randomNoteString : Maybe Note -> Random.Generator (Maybe NoteString)
randomNoteString noteOpt =
  noteOpt
  |> Maybe.map Note.toString
  |> Maybe.withDefault []
  |> Random.List.choose
  |> Random.map Tuple.first

randomChord : List Chord -> Random.Generator (Maybe Chord)
randomChord includes =
  includes
  |> Random.List.choose
  |> Random.map Tuple.first

randomVoicing : List Note -> List Chord -> Random.Generator Voicing
randomVoicing includeNotes includeChords =
  Random.map2 (Voicing Nothing) (randomNote includeNotes) (randomChord includeChords)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  NextVoicing voicing ->
    ( model
    , Random.generate (NextVoicing2 voicing) (randomNoteString voicing.root)
    )

  NextVoicing2 ({root, chord} as voicing) rootString ->
    let
        nextVoicing = case (root, chord) of
          (Nothing, Nothing) -> Nothing
          _ -> Just { voicing | rootString = rootString }
    in
        ( { model | voicing = nextVoicing }
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

  ToggleChord chord checked ->
    if (checked) then
      ( { model | selectedChords = chord :: model.selectedChords }
      , Cmd.none
      )
    else
      ( { model | selectedChords = List.filter (\e -> e /= chord) model.selectedChords }
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

  ToggleAllChords checked ->
    if (checked) then
      ( { model | selectedChords = Chord.all }
      , Cmd.none
      )
    else 
      ( { model | selectedChords = [] }
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
          , Random.generate NextVoicing (randomVoicing model.selectedRoots model.selectedChords)
          )
        else if (not model.isPaused && time >= rotateAt) then
          ( { model | lastTick = rotateAt }
          , Random.generate NextVoicing (randomVoicing model.selectedRoots model.selectedChords)
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


renderCheckbox : Bool -> (Bool -> Msg) -> String -> Html Msg
renderCheckbox c oc t = span []
  [ input
      [ type_ "checkbox"
      , checked c
      , onCheck oc
      ] []
  , label [] [text t]
  ]
    

renderRootCheckbox : List Note -> Note -> Html Msg
renderRootCheckbox selectedRoots root =
  renderCheckbox
    (List.member root selectedRoots)
    (ToggleRoot root)
    (String.join " / " (Note.toString root))


renderChordCheckbox : List Chord -> Chord -> Html Msg
renderChordCheckbox selectedChords chord =
  renderCheckbox
    (List.member chord selectedChords)
    (ToggleChord chord)
    (toString chord)


renderRoots : List Note -> Html Msg
renderRoots selectedRoots =
  let
      allRoots =
        renderCheckbox (List.length selectedRoots == 12) ToggleAllRoots "Roots"

      roots =
        List.map (renderRootCheckbox selectedRoots) Note.all
  in
      div [] (allRoots :: roots)

renderChords : List Chord -> Html Msg
renderChords selectedChords =
  let
      allChords =
        renderCheckbox (List.length selectedChords == 9) ToggleAllChords "Chords"

      chords =
        List.map (renderChordCheckbox selectedChords) Chord.all
  in
      div [] (allChords :: chords)

renderVoicing : Maybe Voicing -> Html Msg
renderVoicing voicing =
  voicing
  |> Maybe.map (\voicing ->
      [ voicing.rootString
      , Maybe.map toString voicing.chord
      ])
  |> (Maybe.map <| List.map <| Maybe.withDefault "")
  |> Maybe.map (String.join " ")
  |> Maybe.withDefault "choose a note or chord"
  |> text
  |> List.singleton
  |> h1 []

view : Model -> Html Msg
view model = div []
  [ renderRoots model.selectedRoots
  , renderChords model.selectedChords
  , renderVoicing model.voicing
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
