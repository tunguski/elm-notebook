module Chart exposing
    ( Config, defaults, sized, colored, palette
    , bars, line, scatter, multiLine
    , frame, polylineOf, dotsOf
    )

{-| Small, dependency-free SVG charts: **bar**, **line**, **scatter** and **multi-series line**.

Each chart is one call — `Chart.bars Chart.defaults data` — returning an `Svg` you drop into a
page. Data is plain Elm: a category chart is a `List ( String, Float )` (label, value); a scatter
or a line series is a `List ( Float, Float )` (x, y). Colours come from the [`Config`](#Config),
applied inline (the JS backend does not bind `Svg.Attributes.class`, so SVG nodes are styled by
attribute, not CSS). The number-crunching lives in [`Scale`](Scale).

For bespoke charts the building blocks are exposed too: [`frame`](#frame) draws the axes and
labels, [`polylineOf`](#polylineOf)/[`dotsOf`](#dotsOf) draw marks in that coordinate system.

@docs Config, defaults, palette
@docs bars, line, scatter, multiLine
@docs frame, polylineOf, dotsOf

-}

import Scale exposing (Scale)
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| How a chart looks and how much room the axes get. -}
type alias Config =
    { width : Float
    , height : Float
    , top : Float
    , right : Float
    , bottom : Float
    , left : Float
    , color : String
    , axis : String
    , label : String
    , showXLabels : Bool
    }


{-| A reasonable default config (380×200, one accent colour). -}
defaults : Config
defaults =
    { width = 380
    , height = 200
    , top = 12
    , right = 14
    , bottom = 28
    , left = 40
    , color = "#5b6ef5"
    , axis = "#c2ccdc"
    , label = "#61708a"
    , showXLabels = True
    }


{-| `defaults` resized. Build config changes **here**, inside the module that owns the `Config`
alias — the elm-lang JS backend miscompiles a record update on a record alias imported from
another module (the un-updated fields come back `undefined`), so callers should reach for these
constructors rather than `{ Chart.defaults | width = … }` at their own site.
-}
sized : Float -> Float -> Config
sized w h =
    { defaults | width = w, height = h }


{-| `defaults` with a different mark colour (see [`sized`](#sized) on why this lives here). -}
colored : String -> Config
colored color =
    { defaults | color = color }


{-| A small qualitative colour palette for multi-series charts. -}
palette : List String
palette =
    [ "#5b6ef5", "#e8590c", "#0f9d58", "#c2410c", "#7c3aed", "#0891b2", "#d6336c", "#65a30d" ]


plotW : Config -> Float
plotW c =
    c.width - c.left - c.right


plotH : Config -> Float
plotH c =
    c.height - c.top - c.bottom


{-| The X scale for `count` evenly-spaced category positions (centres of slots). -}
indexScale : Config -> Int -> Scale
indexScale c count =
    Scale.linear ( 0, toFloat (Basics.max 1 (count - 1)) ) ( c.left, c.left + plotW c )


yScaleFor : Config -> List Float -> Scale
yScaleFor c values =
    let
        ( lo, hi ) =
            Scale.niceBounds values
    in
    Scale.linear ( lo, hi ) ( c.top + plotH c, c.top )


root : Config -> List (Svg msg) -> Svg msg
root c children =
    -- NB: Svg.Attributes.class is unbound in the elm-lang JS backend, so SVG nodes carry no
    -- classes — every colour is set inline via the Config. Width/height give the svg its size.
    Svg.svg
        [ SA.viewBox ("0 0 " ++ Scale.num c.width ++ " " ++ Scale.num c.height)
        , SA.width (Scale.num c.width)
        , SA.height (Scale.num c.height)
        ]
        children



-- HIGH-LEVEL CHARTS ----------------------------------------------------------


{-| A bar chart of `(label, value)` pairs. -}
bars : Config -> List ( String, Float ) -> Svg msg
bars c data =
    let
        values =
            List.map Tuple.second data

        yS =
            yScaleFor c values

        count =
            List.length data

        slot =
            plotW c / toFloat (Basics.max 1 count)

        barW =
            slot * 0.64

        zeroY =
            Scale.convert yS 0

        bar i ( lbl, v ) =
            let
                cx =
                    c.left + slot * (toFloat i + 0.5)

                y =
                    Scale.convert yS v
            in
            Svg.g []
                [ Svg.rect
                    [ SA.x (Scale.num (cx - barW / 2))
                    , SA.y (Scale.num (Basics.min y zeroY))
                    , SA.width (Scale.num barW)
                    , SA.height (Scale.num (Basics.max 0.5 (abs (zeroY - y))))
                    , SA.fill c.color
                    ]
                    []
                , xLabel c count cx lbl
                ]
    in
    root c (frame c yS ++ List.indexedMap bar data)


{-| A line chart of `(label, value)` pairs (X is the category index). -}
line : Config -> List ( String, Float ) -> Svg msg
line c data =
    let
        values =
            List.map Tuple.second data

        yS =
            yScaleFor c values

        xS =
            indexScale c (List.length data)

        count =
            List.length data

        pts =
            List.indexedMap (\i ( _, v ) -> ( Scale.convert xS (toFloat i), Scale.convert yS v )) data

        labels =
            List.indexedMap (\i ( lbl, _ ) -> xLabel c count (Scale.convert xS (toFloat i)) lbl) data
    in
    root c (frame c yS ++ (polylineOf c.color pts :: dotsOf c.color pts ++ labels))


{-| A scatter plot of `(x, y)` points. -}
scatter : Config -> List ( Float, Float ) -> Svg msg
scatter c data =
    let
        xS =
            Scale.linear (Scale.niceBounds (List.map Tuple.first data)) ( c.left, c.left + plotW c )

        yS =
            yScaleFor c (List.map Tuple.second data)

        pts =
            List.map (\( x, y ) -> ( Scale.convert xS x, Scale.convert yS y )) data
    in
    root c (frame c yS ++ dotsOf c.color pts)


{-| Several named `(x, y)` line series, each in a palette colour. -}
multiLine : Config -> List ( String, List ( Float, Float ) ) -> Svg msg
multiLine c serieses =
    let
        allX =
            List.concatMap (\( _, pts ) -> List.map Tuple.first pts) serieses

        allY =
            List.concatMap (\( _, pts ) -> List.map Tuple.second pts) serieses

        xS =
            Scale.linear (Scale.niceBounds allX) ( c.left, c.left + plotW c )

        yS =
            yScaleFor c allY

        place pts =
            List.map (\( x, y ) -> ( Scale.convert xS x, Scale.convert yS y )) pts

        draw i ( _, pts ) =
            polylineOf (colorAt i) (place pts)
    in
    root c (frame c yS ++ List.indexedMap draw serieses)



-- BUILDING BLOCKS ------------------------------------------------------------


{-| The axes, a zero baseline and the high/low Y tick labels for a chart with the given Y scale. -}
frame : Config -> Scale -> List (Svg msg)
frame c yS =
    let
        left =
            c.left

        right =
            c.left + plotW c

        zeroY =
            Scale.convert yS 0
    in
    [ axisLine c left c.top left (c.top + plotH c)
    , axisLine c left zeroY right zeroY
    , tickLabel c (left - 5) (c.top + 4) (Scale.num yS.d1)
    , tickLabel c (left - 5) (c.top + plotH c) (Scale.num yS.d0)
    ]


{-| A `<polyline>` through pixel points, in the given colour. -}
polylineOf : String -> List ( Float, Float ) -> Svg msg
polylineOf color pts =
    Svg.polyline
        [ SA.points (Scale.pointsString pts)
        , SA.fill "none"
        , SA.stroke color
        , SA.strokeWidth "2"
        ]
        []


{-| A `<circle>` dot at each pixel point, in the given colour. -}
dotsOf : String -> List ( Float, Float ) -> List (Svg msg)
dotsOf color pts =
    List.map
        (\( x, y ) ->
            Svg.circle
                [ SA.cx (Scale.num x), SA.cy (Scale.num y), SA.r "2.6", SA.fill color ]
                []
        )
        pts


axisLine : Config -> Float -> Float -> Float -> Float -> Svg msg
axisLine c x1 y1 x2 y2 =
    Svg.line
        [ SA.x1 (Scale.num x1)
        , SA.y1 (Scale.num y1)
        , SA.x2 (Scale.num x2)
        , SA.y2 (Scale.num y2)
        , SA.stroke c.axis
        , SA.strokeWidth "1"
        ]
        []


tickLabel : Config -> Float -> Float -> String -> Svg msg
tickLabel c x y txt =
    Svg.text_
        [ SA.x (Scale.num x), SA.y (Scale.num y), SA.fill c.label, SA.fontSize "9", SA.textAnchor "end" ]
        [ Svg.text txt ]


xLabel : Config -> Int -> Float -> String -> Svg msg
xLabel c count cx lbl =
    if c.showXLabels && count <= 16 then
        Svg.text_
            [ SA.x (Scale.num cx)
            , SA.y (Scale.num (c.height - c.bottom + 13))
            , SA.fill c.label
            , SA.fontSize "9"
            , SA.textAnchor "middle"
            ]
            [ Svg.text (clip lbl) ]

    else
        Svg.text ""


clip : String -> String
clip s =
    if String.length s > 7 then
        String.left 6 s ++ "…"

    else
        s


colorAt : Int -> String
colorAt i =
    case List.drop (modBy (List.length palette) i) palette of
        c :: _ ->
            c

        [] ->
            "#5b6ef5"
