module Notebook.Chart exposing (ChartKind(..), label, kinds, chartable, view)

{-| Adapts a cell's [`Lang.Value`](Lang#Value) output to the vendored [`Chart`](Chart) library
(elm-svg). A value is **chartable** when it is a list of numbers, or a table (list of records)
with a numeric column — then the notebook offers a Bar / Line / Scatter rendering beside the data
grid. This module only extracts `(label, value)` / `(x, y)` data from the runtime value; the SVG
drawing is elm-svg's job.

@docs ChartKind, label, kinds, chartable, view

-}

import Chart
import Html exposing (Html)
import Lang exposing (Value(..))
import Notebook.Value as Value


{-| Which chart to draw. -}
type ChartKind
    = Bar
    | Line
    | Scatter


{-| All chart kinds, for the toggle. -}
kinds : List ChartKind
kinds =
    [ Bar, Line, Scatter ]


{-| A short label for a chart kind. -}
label : ChartKind -> String
label kind =
    case kind of
        Bar ->
            "Bar"

        Line ->
            "Line"

        Scatter ->
            "Scatter"


{-| Can this value be charted at all? -}
chartable : Value -> Bool
chartable value =
    not (List.isEmpty (series value))


{-| Draw the value as a chart of the given kind. -}
view : ChartKind -> Value -> Html msg
view kind value =
    case kind of
        Bar ->
            Chart.bars Chart.defaults (series value)

        Line ->
            Chart.line Chart.defaults (series value)

        Scatter ->
            Chart.scatter Chart.defaults (scatterPoints value)



-- DATA EXTRACTION ------------------------------------------------------------


{-| A `(label, value)` series: a numeric list keyed by index, or a table's first numeric column
keyed by its first text column (else by row index).
-}
series : Value -> List ( String, Float )
series value =
    case value of
        VList items ->
            if not (List.isEmpty items) && List.all isNum items then
                List.indexedMap (\i x -> ( String.fromInt (i + 1), numOf x )) items

            else if Value.isTable value then
                tableSeries value

            else
                []

        _ ->
            []


tableSeries : Value -> List ( String, Float )
tableSeries value =
    case firstColumn isNum value (Value.tableColumns value) of
        Just yName ->
            List.indexedMap (rowPoint (firstColumn isText value (Value.tableColumns value)) yName) (rows value)

        Nothing ->
            []


rowPoint : Maybe String -> String -> Int -> Value -> ( String, Float )
rowPoint labelCol yName index row =
    ( rowLabel labelCol index row, rowValue yName row )


rowLabel : Maybe String -> Int -> Value -> String
rowLabel labelCol index row =
    case labelCol of
        Just lName ->
            case Value.fieldOf lName row of
                Just (VStr s) ->
                    s

                Just other ->
                    Value.inlineValue other

                Nothing ->
                    String.fromInt (index + 1)

        Nothing ->
            String.fromInt (index + 1)


rowValue : String -> Value -> Float
rowValue yName row =
    case Value.fieldOf yName row of
        Just (VNum v) ->
            v

        _ ->
            0


{-| `(x, y)` points for a scatter: the first two numeric columns of a table, else the series
keyed by index.
-}
scatterPoints : Value -> List ( Float, Float )
scatterPoints value =
    if Value.isTable value then
        case numericColumns value of
            xName :: yName :: _ ->
                List.filterMap (twoCols xName yName) (rows value)

            _ ->
                indexed (series value)

    else
        indexed (series value)


twoCols : String -> String -> Value -> Maybe ( Float, Float )
twoCols xName yName row =
    case ( Value.fieldOf xName row, Value.fieldOf yName row ) of
        ( Just (VNum x), Just (VNum y) ) ->
            Just ( x, y )

        _ ->
            Nothing


indexed : List ( String, Float ) -> List ( Float, Float )
indexed pairs =
    List.indexedMap (\i ( _, v ) -> ( toFloat (i + 1), v )) pairs


numericColumns : Value -> List String
numericColumns value =
    List.filter (columnIs isNum value) (Value.tableColumns value)


firstColumn : (Value -> Bool) -> Value -> List String -> Maybe String
firstColumn pred value cols =
    List.filter (columnIs pred value) cols |> List.head


columnIs : (Value -> Bool) -> Value -> String -> Bool
columnIs pred value name =
    case rows value of
        first :: _ ->
            case Value.fieldOf name first of
                Just v ->
                    pred v

                Nothing ->
                    False

        [] ->
            False


rows : Value -> List Value
rows value =
    case value of
        VList items ->
            items

        _ ->
            []


isNum : Value -> Bool
isNum v =
    case v of
        VNum _ ->
            True

        _ ->
            False


isText : Value -> Bool
isText v =
    case v of
        VStr _ ->
            True

        _ ->
            False


numOf : Value -> Float
numOf v =
    case v of
        VNum n ->
            n

        _ ->
            0
