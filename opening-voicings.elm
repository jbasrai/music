import Html exposing (Html, div, text, span, button)
import Dict
import Debug exposing (log)



main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


explode xs ys =
  List.map (\x -> List.map (\y -> (x, y)) ys) xs
    |> List.concat


-- INIT
type alias Target =
  { root : String
  , chord : String
  , lead : Int
  }


-- allTargets : List Target
allTargets =
  let
      roots : List String
      roots =
        [ "A"
        , "Aflat"
        , "B"
        , "Bflat"
        , "C"
        , "D"
        , "Dflat"
        , "E"
        , "Eflat"
        , "F"
        , "G"
        , "Gflat"
        ]

      voicings : List (String, List Int)
      voicings =
        [ ("Maj6", [1, 3, 5, 6])
        , ("min6", [1, 3, 5, 6])
        ]

      explodedVoicings : List (String, Int)
      explodedVoicings =
        List.map (\(chord, lead) -> ([chord], lead)) voicings
        |> List.map (\(chord, lead) -> (explode chord lead))
        |> List.concat

      explodedEverything : List Target
      explodedEverything =
        explode roots explodedVoicings
        |> List.map (\(root, (chord, lead)) -> Target root chord lead)

  in
      explodedEverything


type alias Model =
  { table : List Target
  }


type Msg = Nil


init : (Model, Cmd Msg)
init =
  (Model allTargets, Cmd.none)



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div [] [text "hello"]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
