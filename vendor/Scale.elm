module Scale exposing
    ( Scale, linear, convert, invert
    , niceBounds, ticks
    , num, point, pointsString
    )

{-| The pure maths behind a chart: mapping a **data domain** onto a **pixel range**, choosing
sensible bounds and tick marks, and formatting coordinates. No SVG here — it is just numbers, so
it is fully unit-tested and reusable by any renderer.

@docs Scale, linear, convert, invert
@docs niceBounds, ticks
@docs num, point, pointsString

-}


{-| A linear scale from a data interval `(d0, d1)` to a pixel interval `(r0, r1)`. -}
type alias Scale =
    { d0 : Float
    , d1 : Float
    , r0 : Float
    , r1 : Float
    }


{-| Build a linear scale. A zero-width domain is nudged to width 1 so nothing divides by zero. -}
linear : ( Float, Float ) -> ( Float, Float ) -> Scale
linear ( d0, d1 ) ( r0, r1 ) =
    { d0 = d0
    , d1 =
        if d1 == d0 then
            d0 + 1

        else
            d1
    , r0 = r0
    , r1 = r1
    }


{-| Map a value from the data domain to the pixel range. -}
convert : Scale -> Float -> Float
convert s v =
    s.r0 + (s.r1 - s.r0) * (v - s.d0) / (s.d1 - s.d0)


{-| Map a pixel back to a data value (the inverse of [`convert`](#convert)). -}
invert : Scale -> Float -> Float
invert s p =
    s.d0 + (s.d1 - s.d0) * (p - s.r0) / (s.r1 - s.r0)


{-| Bounds that make a nice axis for a set of values: always include a zero baseline, and never
collapse to a single point.
-}
niceBounds : List Float -> ( Float, Float )
niceBounds values =
    let
        lo =
            Basics.min 0 (List.minimum values |> Maybe.withDefault 0)

        hi =
            Basics.max 0 (List.maximum values |> Maybe.withDefault 1)
    in
    if hi == lo then
        ( lo, lo + 1 )

    else
        ( lo, hi )


{-| `n + 1` evenly-spaced tick values spanning `(lo, hi)` inclusive. -}
ticks : Int -> ( Float, Float ) -> List Float
ticks n ( lo, hi ) =
    let
        steps =
            Basics.max 1 n
    in
    List.map
        (\i -> lo + (hi - lo) * toFloat i / toFloat steps)
        (List.range 0 steps)


{-| Format a number compactly: integers without a trailing `.0`, others to a few decimals. -}
num : Float -> String
num n =
    if isNaN n || isInfinite n then
        "0"

    else if toFloat (round n) == n && abs n < 1.0e12 then
        String.fromInt (round n)

    else
        String.fromFloat (toFloat (round (n * 1000)) / 1000)


{-| An `"x,y"` coordinate string for SVG points/paths (rounded to 2 dp). -}
point : ( Float, Float ) -> String
point ( x, y ) =
    round2 x ++ "," ++ round2 y


{-| A space-separated list of `"x,y"` coordinates for a `<polyline>`/`<polygon>` `points`. -}
pointsString : List ( Float, Float ) -> String
pointsString pts =
    String.join " " (List.map point pts)


round2 : Float -> String
round2 f =
    String.fromFloat (toFloat (round (f * 100)) / 100)
