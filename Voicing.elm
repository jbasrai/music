module Voicing exposing (Voicing, all)

import Note exposing (Note(..))
import Chord exposing (Chord(..))
import Tone exposing (Tone(..))

type alias Voicing =
  { root: Note
  , chord: Chord
  , lead: Tone
  }

explode : (a -> b -> c) -> List a -> List b -> List c
explode f xs ys =
  List.map (\x -> List.map (\y -> f x y) ys) xs
  |> List.concat

all : List Voicing
all =
  List.map Chord.tones Chord.all
  |> List.map2 (,) Chord.all
  |> List.map (\(x, y) -> explode (,) [x] y)
  |> List.concat
  |> explode (,) Note.all
  |> List.map (\(root, (chord, lead)) -> Voicing root chord lead)
