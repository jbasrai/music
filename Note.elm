module Note exposing (Note(..), toString, all)

type Note
  = A
  | Ab
  | B
  | Bb
  | C
  | D
  | Db
  | E
  | Eb
  | F
  | G
  | Gb

all : List Note
all =
  [ A
  , Ab
  , B
  , Bb
  , C
  , D
  , Db
  , E
  , Eb
  , F
  , G
  , Gb
  ]

next : Note -> Note
next note = case note of
  A -> Ab
  Ab -> B
  B -> Bb
  Bb -> C
  C -> Db
  Db -> D
  D -> Eb
  Eb -> E
  E -> F
  F -> Gb
  Gb -> G
  G -> Ab

toString : Note -> String
toString note = case note of
  A -> "A"
  Ab -> "Ab"
  B -> "B"
  Bb -> "Bb"
  C -> "C"
  D -> "D"
  Db -> "Db"
  E -> "E"
  Eb -> "Eb"
  F -> "F"
  G -> "G"
  Gb -> "Gb" 
