import Html exposing (Html, div, text)



main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model = Int


model : Model
model = 0



-- UPDATE


type alias Msg = Int


update : Model -> Msg -> Model
update model msg = 0



-- VIEW

view : Model -> Html Msg
view model =
    div [] [text "hello"]
