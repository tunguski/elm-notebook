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
import Scale
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| Which chart to draw. `Histogram` bins a numeric column into buckets; `MultiLine` plots every
numeric column as its own line. -}
type ChartKind
    = Bar
    | Line
    | Scatter
    | Histogram
    | MultiLine
    | Box
    | Trend


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

        Box ->
            "Box"

        Trend ->
            "Trend"


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
    [ Bar, Line, Histogram, Box ]
        ++ (if multi then
                [ Scatter, Trend, MultiLine ]

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

        Box ->
            boxChart Chart.defaults value

        Trend ->
            trendChart Chart.defaults value



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



-- BOX-AND-WHISKER + TRENDLINE (drawn here with Scale, reusing Chart's frame/dots/polyline) -------


{-| The numeric series to summarise: one per numeric column of a table (keyed by its name), or a
single unnamed series for a plain numeric list. -}
columnSeries : Value -> List ( String, List Float )
columnSeries value =
    case value of
        VList items ->
            if not (List.isEmpty items) && List.all isNum items then
                [ ( "", List.map numOf items ) ]

            else
                List.map (\name -> ( name, colNums name value )) (numericColumns value)

        _ ->
            []


colNums : String -> Value -> List Float
colNums name value =
    List.filterMap
        (\row ->
            case Value.fieldOf name row of
                Just (VNum v) ->
                    Just v

                _ ->
                    Nothing
        )
        (rows value)


plotWidth : Chart.Config -> Float
plotWidth c =
    c.width - c.left - c.right


plotHeight : Chart.Config -> Float
plotHeight c =
    c.height - c.top - c.bottom


{-| The SVG root (Chart's own `root` isn't exposed, so mirror it here). -}
chartRoot : Chart.Config -> List (Svg msg) -> Svg msg
chartRoot c children =
    Svg.svg
        [ SA.viewBox ("0 0 " ++ Scale.num c.width ++ " " ++ Scale.num c.height)
        , SA.width (Scale.num c.width)
        , SA.height (Scale.num c.height)
        ]
        children


{-| A box-and-whisker plot: one box per numeric column, showing min / Q1 / median / Q3 / max. -}
boxChart : Chart.Config -> Value -> Svg msg
boxChart c value =
    let
        serieses =
            columnSeries value

        allVals =
            List.concatMap Tuple.second serieses

        yS =
            Scale.linear (Scale.niceBounds allVals) ( c.top + plotHeight c, c.top )

        count =
            List.length serieses

        slot =
            plotWidth c / toFloat (Basics.max 1 count)

        box i ( name, vals ) =
            boxGlyph c yS (c.left + slot * (toFloat i + 0.5)) (slot * 0.28) name vals
    in
    chartRoot c (Chart.frame c yS ++ List.indexedMap box serieses)


boxGlyph : Chart.Config -> Scale.Scale -> Float -> Float -> String -> List Float -> Svg msg
boxGlyph c yS cx halfW name vals =
    let
        sorted =
            List.sort vals

        size =
            List.length sorted

        py p =
            Scale.convert yS (quantileAt sorted size p)

        boxTop =
            py 0.75

        boxBottom =
            py 0.25
    in
    Svg.g []
        [ vline c.axis cx (py 1) (py 0)
        , Svg.rect
            [ SA.x (Scale.num (cx - halfW))
            , SA.y (Scale.num boxTop)
            , SA.width (Scale.num (2 * halfW))
            , SA.height (Scale.num (Basics.max 0.5 (boxBottom - boxTop)))
            , SA.fill "rgba(91, 110, 245, 0.18)"
            , SA.stroke c.color
            , SA.strokeWidth "1.5"
            ]
            []
        , hline c.color (cx - halfW) (cx + halfW) (py 0.5)
        , hline c.axis (cx - halfW * 0.6) (cx + halfW * 0.6) (py 1)
        , hline c.axis (cx - halfW * 0.6) (cx + halfW * 0.6) (py 0)
        , boxLabel c cx name
        ]


boxLabel : Chart.Config -> Float -> String -> Svg msg
boxLabel c cx name =
    if name == "" then
        Svg.text ""

    else
        Svg.text_
            [ SA.x (Scale.num cx)
            , SA.y (Scale.num (c.height - c.bottom + 13))
            , SA.fill c.label
            , SA.fontSize "9"
            , SA.textAnchor "middle"
            ]
            [ Svg.text name ]


vline : String -> Float -> Float -> Float -> Svg msg
vline color x y1 y2 =
    Svg.line
        [ SA.x1 (Scale.num x), SA.y1 (Scale.num y1), SA.x2 (Scale.num x), SA.y2 (Scale.num y2), SA.stroke color, SA.strokeWidth "1" ]
        []


hline : String -> Float -> Float -> Float -> Svg msg
hline color x1 x2 y =
    Svg.line
        [ SA.x1 (Scale.num x1), SA.y1 (Scale.num y), SA.x2 (Scale.num x2), SA.y2 (Scale.num y), SA.stroke color, SA.strokeWidth "1.5" ]
        []


{-| A scatter plot with a least-squares trend line overlaid (the first two numeric columns). -}
trendChart : Chart.Config -> Value -> Svg msg
trendChart c value =
    let
        pts =
            scatterPoints value

        xs =
            List.map Tuple.first pts

        ys =
            List.map Tuple.second pts

        xS =
            Scale.linear (Scale.niceBounds xs) ( c.left, c.left + plotWidth c )

        yS =
            Scale.linear (Scale.niceBounds ys) ( c.top + plotHeight c, c.top )

        place ( x, y ) =
            ( Scale.convert xS x, Scale.convert yS y )

        fit =
            leastSquares xs ys

        xlo =
            minOr 0 xs

        xhi =
            maxOr 1 xs

        trendLine =
            List.map place
                [ ( xlo, fit.intercept + fit.slope * xlo )
                , ( xhi, fit.intercept + fit.slope * xhi )
                ]
    in
    chartRoot c
        (Chart.frame c yS
            ++ Chart.dotsOf c.color (List.map place pts)
            ++ [ Chart.polylineOf "#e8590c" trendLine ]
        )


leastSquares : List Float -> List Float -> { slope : Float, intercept : Float }
leastSquares xs ys =
    let
        mx =
            meanOf xs

        my =
            meanOf ys

        sxx =
            List.sum (List.map (\x -> (x - mx) * (x - mx)) xs)

        sxy =
            List.sum (List.map2 (\x y -> (x - mx) * (y - my)) xs ys)

        slope =
            if sxx == 0 then
                0

            else
                sxy / sxx
    in
    { slope = slope, intercept = my - slope * mx }


meanOf : List Float -> Float
meanOf xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


minOr : Float -> List Float -> Float
minOr fallback xs =
    Maybe.withDefault fallback (List.minimum xs)


maxOr : Float -> List Float -> Float
maxOr fallback xs =
    Maybe.withDefault fallback (List.maximum xs)


quantileAt : List Float -> Int -> Float -> Float
quantileAt sorted size p =
    if size == 0 then
        0

    else
        let
            pos =
                p * toFloat (size - 1)

            lo =
                floor pos

            hi =
                ceiling pos
        in
        nthF lo sorted + (nthF hi sorted - nthF lo sorted) * (pos - toFloat lo)


nthF : Int -> List Float -> Float
nthF i xs =
    case List.head (List.drop i xs) of
        Just v ->
            v

        Nothing ->
            0
