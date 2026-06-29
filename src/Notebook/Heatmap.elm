module Notebook.Heatmap exposing (range, color)

{-| **Conditional formatting** for a table's numeric columns: each value is shaded by where it sits
between its column's minimum and maximum, so a column reads as a one-glance heat map (faint = low,
saturated = high). The colour maths is pure here; [`Notebook.View`](Notebook-View) paints the cells.

@docs range, color

-}


{-| The `(min, max)` of a numeric column, or `Nothing` for an empty column. -}
range : List Float -> Maybe ( Float, Float )
range xs =
    case ( List.minimum xs, List.maximum xs ) of
        ( Just lo, Just hi ) ->
            Just ( lo, hi )

        _ ->
            Nothing


{-| The background for a value given its column's `(min, max)`: a single blue whose opacity tracks
the value's position in the range (low ⇒ faint, high ⇒ strong). A degenerate column (min == max)
shades everything at the mid tone. -}
color : ( Float, Float ) -> Float -> String
color ( lo, hi ) v =
    let
        t =
            if hi == lo then
                0.5

            else
                clampUnit ((v - lo) / (hi - lo))

        alpha =
            0.07 + 0.63 * t
    in
    "rgba(45, 122, 246, " ++ trim alpha ++ ")"


clampUnit : Float -> Float
clampUnit x =
    if x < 0 then
        0

    else if x > 1 then
        1

    else
        x


trim : Float -> String
trim x =
    String.fromFloat (toFloat (round (x * 1000)) / 1000)
