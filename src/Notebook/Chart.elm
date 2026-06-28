module Notebook.Chart exposing
    ( ChartKind(..), label, chartable, chartableKinds, view
    , numericColumns, defaultColumn
    )

{-| Adapts a cell's [`Lang.Value`](Lang#Value) output to the vendored [`Chart`](Chart) library
(elm-svg). A value is **chartable** when it is a list of numbers, or a table (list of records)
with a numeric column — then the notebook offers a Bar / Line / Scatter / **Histogram** /
**multi-series Lines** rendering beside the data grid, with a picker for which numeric column to
plot. This module only extracts the `(label, value)` / `(x, y)` data from the runtime value (and
bins it for a histogram); the SVG drawing is elm-svg's job.

@docs ChartKind, label, chartable, chartableKinds, view
@docs numericColumns, defaultColumn

-}

import Chart
import Html exposing (Html)
import Lang exposing (Value(..))
import Notebook.Value as Value


{-| Which chart to draw. `Histogram` bins a numeric column into buckets; `MultiLine` plots every
numeric column as its own line. -}
type ChartKind
    = Bar
    | Line
    | Scatter
    | Histogram
    | MultiLine


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

        Histogram ->
            "Histogram"

        MultiLine ->
            "Lines"


{-| Can this value be charted at all? -}
chartable : Value -> Bool
chartable value =
    not (List.isEmpty (series Nothing value))


{-| The chart kinds that make sense for a value: scatter and multi-line need a table with two-plus
numeric columns; the rest apply to any chartable value. -}
chartableKinds : Value -> List ChartKind
chartableKinds value =
    let
        multi =
            List.length (numericColumns value) >= 2
    in
    [ Bar, Line, Histogram ]
        ++ (if multi then
                [ Scatter, MultiLine ]

            else
                []
           )


{-| Draw the value as a chart of the given kind, plotting the chosen numeric column (where that
applies; `Nothing` falls back to the first numeric column). -}
view : ChartKind -> Maybe String -> Value -> Html msg
view kind col value =
    case kind of
        Bar ->
            Chart.bars Chart.defaults (series col value)

        Line ->
            Chart.line Chart.defaults (series col value)

        Scatter ->
            Chart.scatter Chart.defaults (scatterPoints value)

        Histogram ->
            Chart.bars Chart.defaults (histogram (columnValues col value))

        MultiLine ->
            Chart.multiLine Chart.defaults (multiSeries value)



-- DATA EXTRACTION ------------------------------------------------------------


{-| A `(label, value)` series: a numeric list keyed by index, or a table's first numeric column
keyed by its first text column (else by row index).
-}
series : Maybe String -> Value -> List ( String, Float )
series col value =
    case value of
        VList items ->
            if not (List.isEmpty items) && List.all isNum items then
                List.indexedMap (\i x -> ( String.fromInt (i + 1), numOf x )) items

            else if Value.isTable value then
                tableSeries col value

            else
                []

        _ ->
            []


tableSeries : Maybe String -> Value -> List ( String, Float )
tableSeries col value =
    case chosenColumn col value of
        Just yName ->
            List.indexedMap (rowPoint (firstColumn isText value (Value.tableColumns value)) yName) (rows value)

        Nothing ->
            []


{-| The numeric column to plot: the caller's pick if it is a numeric column, else the first. -}
chosenColumn : Maybe String -> Value -> Maybe String
chosenColumn col value =
    case col of
        Just name ->
            if List.member name (numericColumns value) then
                Just name

            else
                defaultColumn value

        Nothing ->
            defaultColumn value


{-| The first numeric column of a table (the default chart column), if any. -}
defaultColumn : Value -> Maybe String
defaultColumn value =
    numericColumns value |> List.head


{-| The raw values of the chosen numeric column (or a plain numeric list), for a histogram. -}
columnValues : Maybe String -> Value -> List Float
columnValues col value =
    case value of
        VList items ->
            if not (List.isEmpty items) && List.all isNum items then
                List.map numOf items

            else
                case chosenColumn col value of
                    Just yName ->
                        List.filterMap (\row -> Maybe.map numOf (numAt yName row)) (rows value)

                    Nothing ->
                        []

        _ ->
            []


numAt : String -> Value -> Maybe Value
numAt name row =
    case Value.fieldOf name row of
        Just (VNum v) ->
            Just (VNum v)

        _ ->
            Nothing


{-| Bin a list of numbers into ~10 equal-width buckets, as `(range-label, count)` bars. -}
histogram : List Float -> List ( String, Float )
histogram values =
    case ( List.minimum values, List.maximum values ) of
        ( Just lo, Just hi ) ->
            let
                n =
                    List.length values

                binCount =
                    Basics.max 1 (Basics.min 12 (round (sqrt (toFloat n))))

                width =
                    if hi == lo then
                        1

                    else
                        (hi - lo) / toFloat binCount

                bucketOf x =
                    Basics.min (binCount - 1) (floor ((x - lo) / width))

                count b =
                    toFloat (List.length (List.filter (\x -> bucketOf x == b) values))

                binLabel b =
                    roundTo (lo + toFloat b * width) ++ "–" ++ roundTo (lo + toFloat (b + 1) * width)
            in
            List.map (\b -> ( binLabel b, count b )) (List.range 0 (binCount - 1))

        _ ->
            []


roundTo : Float -> String
roundTo x =
    Value.numberToString (toFloat (round (x * 100)) / 100)


{-| One line series per numeric column, each plotted against the row index. -}
multiSeries : Value -> List ( String, List ( Float, Float ) )
multiSeries value =
    let
        pointsFor name =
            List.indexedMap (\i row -> ( toFloat (i + 1), rowValue name row )) (rows value)
    in
    List.map (\name -> ( name, pointsFor name )) (numericColumns value)


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
                indexed (series Nothing value)

    else
        indexed (series Nothing value)


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
