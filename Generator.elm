module Generator exposing (Pitch(..), Note(..), Letter(..), Accidental(..), notes, note)
import Random exposing (Generator)
import Random.List



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
  | Sharp11
  | Flat13
  | Ma13



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
notes : Pitch -> List Note
notes pitch =
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


note : Pitch -> Generator Note
note pitch =
  pitch
  |> notes
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


{-- jump : Interval -> Note -> Note



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
