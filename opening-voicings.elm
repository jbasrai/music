import Html exposing (Html, div, text, span, button)
import Dict
import Debug exposing (log)
import Random
import Random.List exposing (shuffle)
import Voicing exposing (Voicing)


main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }


-- INIT
type alias Model =
  { table : List Voicing
  }


init : (Model, Cmd Msg)
init =
  (Model Voicing.all, Cmd.none)



-- UPDATE


type alias Msg = String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div [] [text (toString (List.length model.table))]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
