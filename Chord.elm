module Chord exposing (Chord(..), all, tones, toStrings)

import Tone exposing (..)

type Chord
  = Maj6
  | Min6
  | Maj7
  | Min7
  | Dom7
  | HalfDim7
  | Dim7
  | MinMaj7
  | Aug7

all : List Chord
all =
  [ Maj6
  , Min6
  , Maj7
  , Min7
  , Dom7
  , HalfDim7
  , Dim7
  , MinMaj7
  , Aug7
  ]

tones : Chord -> List Tone
tones chord =
  case chord of
    Maj6 -> [P1, M3, P5, M6]
    Min6 -> [P1, Mi3, P5, M6]
    Maj7 -> [P1, M3, P5, M7]
    Min7 -> [P1, Mi3, P5, Mi7]
    Dom7 -> [P1, M3, P5, Mi7]
    HalfDim7 -> [P1, Mi3, Di5, Mi7]
    Dim7 -> [P1, Mi3, Di5, Di7]
    MinMaj7 -> [P1, Mi3, P5, M7]
    Aug7 -> [P1, M3, Mi6, Mi7]

{--}
toStrings : Chord -> List String
toStrings chord = case chord of
  Maj6 -> ["6", "Maj6", "M6"]
  Min6 -> ["-6", "min6", "m6"]
  Maj7 -> ["Δ", "Δ7", "Maj7", "M7"]
  Min7 -> ["-7", "min7", "m7"]
  Dom7 -> ["7", "dom7"]
  HalfDim7 -> ["ø", "m7b5", "min7b5", "-7♭5"]
  Dim7 -> ["°7", "dim7"]
  MinMaj7 -> ["minMaj7", "-Δ7"]
  Aug7 -> ["aug7", "+7"]
--}
