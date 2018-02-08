module Tone exposing (Tone(..))

type Tone
  = P1
  | Mi3
  | M3
  | Di5
  | P5
  | A5
  | Mi6
  | M6
  | Di7
  | Mi7
  | M7

steps : Tone -> Int
steps interval =
  case interval of
    P1 -> 0
    Mi3 -> 3
    M3 -> 4
    Di5 -> 6
    P5 -> 7
    A5 -> 8
    Mi6 -> 8
    M6 -> 9
    Di7 -> 9
    Mi7 -> 10
    M7 -> 11
