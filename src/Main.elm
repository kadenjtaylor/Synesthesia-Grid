module Main exposing (..)

import Browser exposing (..)
import Debug exposing (toString)
import Dict exposing (Dict)
import Element exposing (height, width)
import Element.Background as Background exposing (color)
import Element.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseLeave, onMouseOver, onSubmit)


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
    , editCell : Maybe ( String, String )
    }



-- TODO: Hold a 3d-grid (sparse)
-- That works out to a list of rows and a list of cols (both ordered) and a list of values at certain intersections


type Msg
    = Highlight String String
    | KillHighlight
    | EditCell String String
    | AdjustValue String String String



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
            "#eed600ff"

        6 ->
            "#EEE600"

        7 ->
             "#a9e600ff"

        8 ->
            "#8CE600"

        9 ->
            "#04c404ff"

        _ ->
            emptyColor


formatCell : Sheet -> String -> String -> Maybe ( String, String ) -> Maybe ( String, String ) -> Html Msg
formatCell sheet row col focusCell editCell =
    let
        isFocused =
            case focusCell of
                Just ( r, c ) ->
                    row == r && col == c

                Nothing ->
                    False

        isPartialFocus =
            case focusCell of
                Just ( r, c ) ->
                    row == r || col == c

                Nothing ->
                    False

        isEditing =
            case editCell of
                Just ( r, c ) ->
                    row == r && col == c

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
         , style "font-size" "xx-large"
         , title (String.concat [ "How ", col, " is ", row, "?" ])
         , onClick (EditCell row col)
         ]
            ++ cellStyle
        )
        (if isEditing then
            [ input [ style "font-size" "xx-large", style "max-width" "50px", style "text-align" "center", onInput (\s -> AdjustValue row col s) ] [ text label ] ]

         else
            [ text label ]
        )



-- numberInput : Element.Element Msg
-- numberInput =
--     Input.slider
--         [ Element.height (Element.px 30)
--         -- Here is where we're creating/styling the "track"
--         , Element.behindContent
--             (Element.el
--                 [ Element.width Element.fill
--                 , Element.height (Element.px 2)
--                 , Element.centerY
--                 ]
--                 Element.none
--             )
--         ]
--         { onChange = \f -> AdjustValue f
--         , label =
--             Input.labelAbove []
--                 (Element.text
--                     "Slider"
--                 )
--         , min = 0
--         , max = 75
--         , step = Just 1
--         , value = 0
--         , thumb =
--             Input.defaultThumb
--         }


rows : Model -> List (Html Msg)
rows model =
    List.map
        (\r -> tr [] (th ([ scope "row", style "background" borderColor ] ++ cellStyle) [ text r ] :: List.map (\c -> formatCell model.cells r c model.focusCell model.editCell) model.column_names))
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
    , column_names = [ "bright", "loud", "spicy", "crunchy", "shrill", "rough", "smooth", "shiny", "bitter", "hot", "cold", "dry", "wet", "heavy", "light", "gooey", "oily", "creamy", "salty", "blurry", "quiet", "sharp", "dull" ]
    , cells =
        Dict.fromList
            [ ( ( "the sun", "spicy" ), 8 ) ]
    , focusCell = Nothing
    , editCell = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Highlight row col ->
            { model | focusCell = Just ( row, col ) }

        KillHighlight ->
            { model | focusCell = Nothing }

        EditCell row col ->
            { model | focusCell = Nothing, editCell = Just ( row, col ) }

        AdjustValue r c i ->
            let
                num =
                    String.toInt i
            in
            case num of
                Just newValue ->
                    { model | editCell = Nothing, cells = Dict.insert ( r, c ) newValue model.cells }

                Nothing ->
                    { model | editCell = Nothing }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = tableView
        , update = update
        }
