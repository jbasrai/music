import Html exposing (Html, div, text, span, button)
import Dict
import Debug exposing (log)
import Random
import Random.List exposing (shuffle)


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


allTargets =
  let
      explodedEverything : List Target
      explodedEverything =
        List.map (\(chord, lead) -> ([chord], lead)) voicings
        |> List.map (\(chord, lead) -> (explode chord lead))
        |> List.concat
        |> explode roots
        |> List.map (\(root, (chord, lead)) -> Target root chord lead)

  in
      Debug.log "all targets" explodedEverything


type alias Model =
  { table : List Target
  }


type Msg = Randomize | Update (List Target)


init : (Model, Cmd Msg)
init =
  ( Model allTargets
  , Random.generate Update (shuffle allTargets)
  )



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      (model, Random.generate Update (shuffle model.table))

    Update table ->
      (Model table, Cmd.none)




-- VIEW


textWithSpace : String -> Html Msg
textWithSpace t =
  text (t ++ " ")


renderRow : Target -> Html Msg
renderRow {root, chord, lead} =
  div []
    [ span [] [textWithSpace root]
    , span [] [textWithSpace chord]
    , span [] [textWithSpace (toString lead)]
    ]


renderTable : List Target -> List (Html Msg)
renderTable targets =
  List.map renderRow targets


view : Model -> Html Msg
view model =
  div [] (renderTable model.table)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
