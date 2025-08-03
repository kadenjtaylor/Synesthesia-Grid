module Main exposing (..)

import Browser exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseLeave, onMouseOver)


type alias FilledCell =
    { row : String
    , col : String
    , val : Int
    }



-- We'll expand this to include data from multiple users, aggregates, etc


type alias CellContent =
    Int


type alias Sheet =
    Dict ( String, String ) CellContent


type alias Model =
    { row_names : List String
    , column_names : List String
    , cells : Sheet
    , focusCell : Maybe ( String, String )
    }



-- TODO: Hold a 3d-grid (sparse)
-- That works out to a list of rows and a list of cols (both ordered) and a list of values at certain intersections


type Msg
    = Highlight String String
    | KillHighlight



-- Styles


cellStyle : List (Attribute Msg)
cellStyle =
    [ style "border" "1px solid black", style "text-align" "center", style "width" "60px", style "min-width" "60px", style "height" "60px", style "height" "60px" ]


columnNames : List String -> List (Html Msg)
columnNames column_names =
    List.map
        (\name -> th ([ scope "col", style "background" borderColor ] ++ cellStyle) [ text name ])
        column_names


headers : Model -> List (Html Msg)
headers model =
    [ tr []
        (columnNames (" " :: model.column_names))
    ]


borderColor =
    "#bfdadaff"


highlightColor =
    "#b1add1ff"


partialHighlightColor =
    "#e2e1eeff"


emptyColor =
    "#fdfaefff"


cellColor : Int -> String
cellColor i =
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

        _ ->
            emptyColor


formatCell : Sheet -> String -> String -> Maybe ( String, String ) -> Html Msg
formatCell sheet row col focus =
    let
        isFocused =
            case focus of
                Just ( r, c ) ->
                    row == r && col == c

                Nothing ->
                    False

        isPartialFocus =
            case focus of
                Just ( r, c ) ->
                    row == r || col == c

                Nothing ->
                    False

        contents =
            Dict.get ( row, col ) sheet

        color =
            if isFocused then
                highlightColor

            else if isPartialFocus then
                partialHighlightColor

            else
                case contents of
                    Just i ->
                        cellColor i

                    Nothing ->
                        emptyColor

        label =
            case contents of
                Just i ->
                    toString i

                Nothing ->
                    " "
    in
    td
        ([ style "background"
            color
         , onMouseOver (Highlight row col)
         , onMouseLeave KillHighlight
         ]
            ++ cellStyle
        )
        [ text label ]


rows : Model -> List (Html Msg)
rows model =
    List.map
        (\r -> tr [] (th ([ scope "row", style "background" borderColor ] ++ cellStyle) [ text r ] :: List.map (\c -> formatCell model.cells r c model.focusCell) model.column_names))
        model.row_names


tableView : Model -> Html Msg
tableView model =
    table
        []
        (headers model
            ++ rows model
        )


type alias Cell =
    { concept : String
    , descriptor : String
    , intensity : Int
    }


initialModel : Model
initialModel =
    { row_names = [ "the sun", "the moon", "truth", "friendship", "intelligence", "coffee", "love", "sex", "sleep", "justice", "thinking", "safety", "money", "confusion", "certainty" ]
    , column_names = [ "bright", "loud", "spicy", "crunchy", "shrill", "rough", "smooth", "shiny", "bitter", "hot", "cold", "dry", "wet", "heavy", "light", "gooey", "oily", "creamy", "salty", "blurry", "quiet" ]
    , cells =
        Dict.fromList
            [ ( ( "the sun", "spicy" ), 8 ) ]
    , focusCell = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Highlight row col ->
            { model | focusCell = Just ( row, col ) }

        KillHighlight ->
            { model | focusCell = Nothing }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = tableView
        , update = update
        }
