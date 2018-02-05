import Html exposing (Html, div, text, span)



main =
  Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }



-- INIT


type alias Model =
  { root: String
  , chord: String
  , lead: String
  }


type Msg = Nil


init : (Model, Cmd Msg)
init =
  (Model "Bflat" "Maj7" "7", Cmd.none)



-- UPDATE


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ span [] [text (model.root ++ " ")]
    , span [] [text (model.chord ++ " ")]
    , span [] [text (model.lead ++ " ")]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
