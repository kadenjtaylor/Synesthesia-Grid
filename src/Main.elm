module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Browser exposing (..)


type alias Model =
    String
-- TODO: Hold a 3d-grid (sparse)
-- That works out to a list of rows and a list of cols (both ordered) and a list of values at certain intersections


type alias Msg =
    String

view: Model -> Html Msg
view model =
    div [ class "jumbotron" ]
        [ h1 [] [ text "Welcome to Dunder Mifflin!" ]
        , p []
            [ text "Dunder Mifflin Inc. (stock symbol "
            , strong [] [ text "DMI" ]
            , text <|
                """ 
                ) is a micro-cap regional paper and office 
                supply distributor with an emphasis on servicing 
                small-business clients.
                """
            ]
        ]


initialModel: Model
initialModel = "Hello"


update: Msg -> Model -> Model
update msg model =
    model

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }