module Main exposing (..)

import Browser exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias UserID =
    String


type alias FilledCell =
    { row : String
    , col : String
    , val : Int
    }



-- We'll expand this to include data from multiple users, aggregates, etc


type alias CellContent =
    Int


type alias Sheet =
    Dict String (Dict String CellContent)


type alias Model =
    { row_names : List String
    , column_names : List String
    , cells : Sheet
    }



-- TODO: Hold a 3d-grid (sparse)
-- That works out to a list of rows and a list of cols (both ordered) and a list of values at certain intersections


type alias Msg =
    String


columnNames : List String -> List (Html Msg)
columnNames column_names =
    List.map
        (\name -> th [ style "border" "1px solid black", scope "col" ] [ text name ])
        column_names


headers : Model -> List (Html Msg)
headers model =
    [ tr []
        (columnNames (" " :: model.column_names))
    ]


intensityToColor : Int -> String
intensityToColor i =
    case i of
        0 ->
            "#FF0000"

        1 ->
            "#FF4600"

        2 ->
            "#FF8800"

        3 ->
            "#FFAD00"

        4 ->
            "#FFC500"

        5 ->
            "#EEE600"

        7 ->
            "#CFE600"

        8 ->
            "#8CE600"

        9 ->
            "#5EE600"

        default ->
            "grey"


formatCell : Sheet -> String -> String -> Html Msg
formatCell sheet row col =
    case Dict.get row sheet of
        Just c ->
            case Dict.get col c of
                Just cellContent ->
                    td
                        [ style "border" "1px solid black"
                        , style "background"
                            (intensityToColor
                                cellContent
                            )
                        ]
                        [ text (toString cellContent) ]

                Nothing ->
                    td [ style "border" "1px solid black", style "background" "" ] [ text " " ]

        Nothing ->
            td [ style "border" "1px solid black", style "background" "" ] [ text " " ]


rows : Model -> List (Html Msg)
rows model =
    List.map
        (\r -> tr [] (th [ style "border" "1px solid black", scope "row" ] [ text r ] :: List.map (\c -> formatCell model.cells r c) model.column_names))
        model.row_names


tableView : Model -> Html Msg
tableView model =
    table
        []
        (headers model
            ++ rows model
        )


initialModel : Model
initialModel =
    { row_names = [ "the sun", "the moon", "truth", "friendship", "intelligence", "coffee", "love", "sex", "sleep", "justice", "thinking" ]
    , column_names = [ "bright", "loud", "spicy", "crunchy", "shrill", "rough", "smooth", "shiny", "bitter" ]
    , cells =
        Dict.fromList
            [ ( "the sun", Dict.fromList [ ( "spicy", 5 ) ] )
            ]
    }


update : Msg -> Model -> Model
update msg model =
    model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = tableView
        , update = update
        }
