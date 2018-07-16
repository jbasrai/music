import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Generator exposing
  ( Pitch(..)
  , Note(..)
  , Letter(..)
  , Accidental(..)
  , Mode(..)
  , Interval(..)
  , chooseNote
  , scale
  , jump
  )
import Random exposing (Generator)
import Random.List


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model = Note


init : (Model, Cmd Msg)
init =
  ( Note C Natural
  , fetch
  )



-- UPDATE


type Msg = Get | Go Note


fetch : Cmd Msg
fetch = Random.generate Go (chooseNote Fi)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Get -> (model, fetch)
    Go x -> (x, Cmd.none)



-- VIEW

view : Model -> Html Msg
view model =
    div []
      [ text <| toString <| jump Ma7 (Note F Sharp)
      ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
