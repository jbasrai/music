module Generator exposing
  ( Pitch(..)
  , Note(..)
  , Letter(..)
  , Accidental(..)
  , Mode(..)
  , Interval(..)
  , noteOptions
  , chooseNote
  , scale
  , jump
  )
import Random exposing (Generator)
import Random.List
import Array



type Note = Note Letter Accidental



type Chord
  = Maj6
  | Min6
  | Maj7
  | Min7
  | Dom7
  | MinMaj7
  | HalfDim7
  | Dim7
  | Aug7



type Interval
  = P1
  | Mi3
  | Ma3
  | P5
  | Mi7
  | Ma7
  | Flat9
  | Ma9
  | P11
  -- | Sharp11
  | Flat13
  | Ma13



-- 1 based
address : Interval -> (Mode, Int)
address interval =
  case interval of
    P1 -> (Ionian, 1)
    Mi3 -> (Phrygian, 3)
    Ma3 -> (Ionian, 3)
    P5 -> (Ionian, 5)
    Mi7 -> (Phrygian, 7)
    Ma7 -> (Ionian, 7)
    Flat9 -> (Phrygian, 2)
    Ma9 -> (Ionian, 2)
    P11 -> (Ionian, 4)
    Flat13 -> (Phrygian, 6)
    Ma13 -> (Ionian, 6)



type Pitch
  = Do
  | Di
  | Re
  | Ri
  | Mi
  | Fa
  | Fi
  | Sol
  | Si
  | La
  | Li
  | Ti



-- only meant to translate pitches to roots, so notes like "E Sharp" don't get returned
noteOptions : Pitch -> List Note
noteOptions pitch =
    case pitch of
        Do -> 
            [ Note C Natural
            ]
        Di ->
            [ Note C Sharp
            , Note D Flat
            ]
        Re -> 
            [ Note D Natural
            ]
        Ri -> 
            [ Note E Flat
            ]
        Mi -> 
            [ Note E Natural
            ]
        Fa -> 
            [ Note F Natural
            ]
        Fi -> 
            [ Note F Sharp
            , Note G Flat
            ]
        Sol -> 
            [ Note G Natural
            ]
        Si -> 
            [ Note A Flat
            ]
        La -> 
            [ Note A Natural
            ]
        Li -> 
            [ Note B Flat
            ]
        Ti -> 
            [ Note B Natural
            ]


chooseNote : Pitch -> Generator Note
chooseNote pitch =
  pitch
  |> noteOptions
  |> Random.List.choose
  |> Random.map Tuple.first
  |> Random.map (Maybe.withDefault (Note C Natural))



type Letter
  = A
  | B
  | C
  | D
  | E
  | F
  | G



type Accidental
  = Natural
  | Flat
  | Sharp



type Mode = Ionian | Phrygian



type alias Scale = List Note



ionian : Note -> Maybe Scale
ionian note =
  case note of
    Note A Natural -> Just
      [ Note A Natural
      , Note B Natural
      , Note C Sharp
      , Note D Natural
      , Note E Natural
      , Note F Sharp
      , Note G Sharp
      ]
    Note B Flat -> Just
      [ Note B Flat
      , Note C Natural
      , Note D Natural
      , Note E Flat
      , Note F Natural
      , Note G Natural
      , Note A Natural
      ]
    Note B Natural -> Just
      [ Note B Natural
      , Note C Sharp
      , Note D Sharp
      , Note E Natural
      , Note F Sharp
      , Note G Sharp
      , Note A Sharp
      ]
    Note C Natural -> Just
      [ Note C Natural
      , Note D Natural
      , Note E Natural
      , Note F Natural
      , Note G Natural
      , Note A Natural
      , Note B Natural
      ]
    Note E Flat -> Just
      [ Note E Flat
      , Note F Natural
      , Note G Natural
      , Note A Flat
      , Note B Flat
      , Note C Natural
      , Note D Natural
      ]
    Note E Natural -> Just
      [ Note E Natural
      , Note F Sharp
      , Note G Sharp
      , Note A Natural
      , Note B Natural
      , Note C Sharp
      , Note D Sharp
      ]
    Note F Natural -> Just
      [ Note F Natural
      , Note G Natural
      , Note A Natural
      , Note B Flat
      , Note C Natural
      , Note D Natural
      , Note E Natural 
      ]
    Note F Sharp -> Just
      [ Note F Sharp
      , Note G Sharp
      , Note A Sharp
      , Note B Natural
      , Note C Sharp
      , Note D Sharp
      , Note E Sharp
      ]
    Note G Natural -> Just
      [ Note G Natural
      , Note A Natural
      , Note B Natural
      , Note C Natural
      , Note D Natural
      , Note E Natural
      , Note F Sharp
      ]
    _ ->
      Debug.crash ("Note " ++ (toString note) ++ " not found.")



scale : Mode -> Note -> Maybe Scale
scale mode note =
  case mode of
    Ionian ->
      ionian note
    _ -> 
      Debug.crash ("Mode " ++ (toString mode) ++ " not found.")



jump : Interval -> Note -> Maybe Note
jump interval note =
  let
      (mode, i) =
        Debug.log "address" (address interval)
  in
      scale mode note
      |> Maybe.map Array.fromList
      |> Maybe.andThen (Array.get (i - 1))


{--
type alias Voicing =
  { chord : Chord
  , root : Pitch
  , interval : Interval
  }



drill : Voicing -> Generator Drill
drill { chord, root, interval }=
  let
    rootNote : Generator Note
    rootNote =
      note root

    leadNote : Generator Note
    leadNote =
      Random.map (jump interval) rootNote
  in
    Random.map2
      (Drill chord)
      rootNote
      leadNote



type alias Drill =
  { chord : Chord
  , root : Note
  , lead : Note
  }



drillSheet : List { chord : Chord, lead : Lead } -> List Drill
drillSheet xs =
  []
--}
