module Notebook.Sparkline exposing (points, svg)

{-| A **sparkline**: a tiny, axis-less line chart of a numeric series, small enough to sit inline in
a table cell. [`Notebook.View`](Notebook-View) draws one beside each numeric column in the profile
panel so a column's shape (trend, spikes, flatness) is legible at a glance. Only the point geometry
lives here; the SVG is built with the same primitives as [`Chart`](Chart).

@docs points, svg

-}

import Svg exposing (Svg)
import Svg.Attributes as SA


boxW : Float
boxW =
    84


boxH : Float
boxH =
    22


pad : Float
pad =
    3


{-| Map a numeric series to `(x, y)` pixel points in an 84×22 box: x evenly spaced left→right, y
inverted so larger values sit higher. A single point sits at the centre; an empty series has none. -}
points : List Float -> List ( Float, Float )
points values =
    case values of
        [] ->
            []

        [ _ ] ->
            [ ( boxW / 2, boxH / 2 ) ]

        _ ->
            let
                n =
                    List.length values

                lo =
                    Maybe.withDefault 0 (List.minimum values)

                hi =
                    Maybe.withDefault 0 (List.maximum values)

                spanY =
                    hi - lo

                dx =
                    (boxW - 2 * pad) / toFloat (n - 1)

                yOf v =
                    if spanY == 0 then
                        boxH / 2

                    else
                        boxH - pad - (v - lo) / spanY * (boxH - 2 * pad)
            in
            List.indexedMap (\i v -> ( pad + toFloat i * dx, yOf v )) values


{-| A sparkline of the series — a blue polyline with a red dot marking the final value. -}
svg : List Float -> Svg msg
svg values =
    let
        pts =
            points values

        d =
            String.join " " (List.map (\( x, y ) -> num x ++ "," ++ num y) pts)
    in
    Svg.svg
        [ SA.viewBox ("0 0 " ++ num boxW ++ " " ++ num boxH)
        , SA.width (num boxW)
        , SA.height (num boxH)
        ]
        (Svg.polyline [ SA.points d, SA.fill "none", SA.stroke "#2d7af6", SA.strokeWidth "1.5" ] []
            :: endDot pts
        )


endDot : List ( Float, Float ) -> List (Svg msg)
endDot pts =
    case List.reverse pts of
        ( x, y ) :: _ ->
            [ Svg.circle [ SA.cx (num x), SA.cy (num y), SA.r "1.8", SA.fill "#d94d3a" ] [] ]

        [] ->
            []


num : Float -> String
num x =
    String.fromFloat (toFloat (round (x * 100)) / 100)
