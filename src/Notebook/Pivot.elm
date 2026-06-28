module Notebook.Pivot exposing
    ( Spec, Agg(..), Grid, Row
    , defaultSpec, pivot
    , withRow, withColumn, withValue, withAgg
    , aggLabel, aggFromString, aggs
    )

{-| **Pivot tables** over a table value: group its rows by a *row field* and a *column field*, then
aggregate a *value field* into each cell — the cross-tab a spreadsheet user reaches for. Everything
here is pure; the host supplies the [`Spec`](#Spec) (which fields, which aggregation) and renders the
returned [`Grid`](#Grid).

@docs Spec, Agg, Grid, Row
@docs defaultSpec, pivot
@docs withRow, withColumn, withValue, withAgg
@docs aggLabel, aggFromString, aggs

-}

import Lang exposing (Value(..))
import Notebook.Value as Value


{-| How to combine the value field within a cell. -}
type Agg
    = Sum
    | Count
    | Mean
    | Min
    | Max


{-| What to pivot: the field whose distinct values become rows, the one whose distinct values become
columns, the field to aggregate, and how. -}
type alias Spec =
    { row : String, column : String, value : String, agg : Agg }


{-| The computed cross-tab: the column headers and one [`Row`](#Row) per distinct row value. -}
type alias Grid =
    { columns : List String, rows : List Row }


{-| One pivot row: its row-value label and the aggregated cell for each column (`""` when empty). -}
type alias Row =
    { label : String, cells : List String }


{-| All aggregations, for a picker. -}
aggs : List Agg
aggs =
    [ Sum, Count, Mean, Min, Max ]


{-| A short label for an aggregation. -}
aggLabel : Agg -> String
aggLabel agg =
    case agg of
        Sum ->
            "Sum"

        Count ->
            "Count"

        Mean ->
            "Mean"

        Min ->
            "Min"

        Max ->
            "Max"


aggFromString : String -> Agg
aggFromString s =
    case s of
        "Count" ->
            Count

        "Mean" ->
            Mean

        "Min" ->
            Min

        "Max" ->
            Max

        _ ->
            Sum


withRow : String -> Spec -> Spec
withRow name spec =
    { spec | row = name }


withColumn : String -> Spec -> Spec
withColumn name spec =
    { spec | column = name }


withValue : String -> Spec -> Spec
withValue name spec =
    { spec | value = name }


withAgg : Agg -> Spec -> Spec
withAgg agg spec =
    { spec | agg = agg }


{-| A sensible starting spec for a table: a text column for the rows, a different column for the
columns, a numeric column for the value, summed. -}
defaultSpec : Value -> Spec
defaultSpec value =
    let
        cols =
            Value.tableColumns value

        texts =
            List.filter (columnIs isText value) cols

        nums =
            List.filter (columnIs isNum value) cols

        first fallback xs =
            List.head xs |> Maybe.withDefault fallback

        firstCol =
            first "" cols

        rowField =
            first firstCol texts

        columnField =
            cols |> List.filter (\c -> c /= rowField) |> first firstCol

        valueField =
            first firstCol nums
    in
    { row = rowField, column = columnField, value = valueField, agg = Sum }


{-| Compute the cross-tab. -}
pivot : Spec -> Value -> Grid
pivot spec value =
    let
        records =
            rowsOf value

        rowKeys =
            distinct (List.map (keyOf spec.row) records)

        colKeys =
            distinct (List.map (keyOf spec.column) records)

        cellFor r c =
            records
                |> List.filter (\rec -> keyOf spec.row rec == r && keyOf spec.column rec == c)
                |> aggregate spec

        toRow r =
            { label = r, cells = List.map (cellFor r) colKeys }
    in
    { columns = colKeys, rows = List.map toRow rowKeys }


aggregate : Spec -> List Value -> String
aggregate spec records =
    case spec.agg of
        Count ->
            String.fromInt (List.length records)

        _ ->
            let
                nums =
                    List.filterMap (numAt spec.value) records
            in
            if List.isEmpty nums then
                ""

            else
                Value.numberToString (combine spec.agg nums)


combine : Agg -> List Float -> Float
combine agg nums =
    case agg of
        Mean ->
            List.sum nums / toFloat (List.length nums)

        Min ->
            List.minimum nums |> Maybe.withDefault 0

        Max ->
            List.maximum nums |> Maybe.withDefault 0

        _ ->
            List.sum nums


keyOf : String -> Value -> String
keyOf field record =
    Value.fieldOf field record |> Maybe.map Value.displayValue |> Maybe.withDefault ""


numAt : String -> Value -> Maybe Float
numAt field record =
    case Value.fieldOf field record of
        Just (VNum n) ->
            Just n

        _ ->
            Nothing


rowsOf : Value -> List Value
rowsOf value =
    case value of
        VList items ->
            items

        _ ->
            []


distinct : List String -> List String
distinct xs =
    List.foldl
        (\x seen ->
            if List.member x seen then
                seen

            else
                seen ++ [ x ]
        )
        []
        xs


columnIs : (Value -> Bool) -> Value -> String -> Bool
columnIs pred value name =
    case rowsOf value of
        first :: _ ->
            Value.fieldOf name first |> Maybe.map pred |> Maybe.withDefault False

        [] ->
            False


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
