module Generator exposing (..)



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



type Note = Letter Accidental



type alias Voicing =
    { chord : Chord
    , lead : Interval
    , root : Pitch
    }



type alias Drill =
    { chord : Chord
    , root : Note
    , lead : Note
    }



drillSheet : List { chord : Chord, lead : Lead } -> List Drill
drillSheet xs =
    []
