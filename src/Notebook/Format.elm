module Notebook.Format exposing (Format(..), all, next, label, format)

{-| **Number formatting** for a data grid's numeric cells: cycle a table between automatic display,
fixed decimals, integer, percentage and thousands-separated. Pure number→string here;
[`Notebook.View`](Notebook-View) applies the chosen format to every numeric cell.

@docs Format, all, next, label, format

-}

import Notebook.Value as Value


{-| How to render a number. -}
type Format
    = Auto
    | Fixed2
    | Fixed0
    | Percent
    | Thousands


{-| Every format, for cycling. -}
all : List Format
all =
    [ Auto, Fixed2, Fixed0, Percent, Thousands ]


{-| The next format in the cycle (wraps around). -}
next : Format -> Format
next f =
    case f of
        Auto ->
            Fixed2

        Fixed2 ->
            Fixed0

        Fixed0 ->
            Percent

        Percent ->
            Thousands

        Thousands ->
            Auto


{-| A short chip label for a format. -}
label : Format -> String
label f =
    case f of
        Auto ->
            "1.23"

        Fixed2 ->
            "0.00"

        Fixed0 ->
            "0"

        Percent ->
            "%"

        Thousands ->
            "1,000"


{-| Render a number under the given format. -}
format : Format -> Float -> String
format f x =
    case f of
        Auto ->
            Value.numberToString x

        Fixed2 ->
            fixed 2 x

        Fixed0 ->
            String.fromInt (round x)

        Percent ->
            fixed 1 (x * 100) ++ "%"

        Thousands ->
            thousands (round x)


fixed : Int -> Float -> String
fixed places x =
    let
        factor =
            toFloat (10 ^ places)

        rounded =
            toFloat (round (x * factor)) / factor
    in
    padDecimals places (String.fromFloat rounded)


padDecimals : Int -> String -> String
padDecimals places str =
    let
        neg =
            String.startsWith "-" str

        body =
            if neg then
                String.dropLeft 1 str

            else
                str

        sign =
            if neg then
                "-"

            else
                ""
    in
    case String.split "." body of
        [ whole ] ->
            if places == 0 then
                sign ++ whole

            else
                sign ++ whole ++ "." ++ String.repeat places "0"

        [ whole, frac ] ->
            sign ++ whole ++ "." ++ String.left places (frac ++ String.repeat places "0")

        _ ->
            str


thousands : Int -> String
thousands i =
    let
        grouped =
            groupThousands (String.fromInt (abs i))
    in
    if i < 0 then
        "-" ++ grouped

    else
        grouped


groupThousands : String -> String
groupThousands s =
    String.fromList (List.reverse (commaEvery3 (List.reverse (String.toList s))))


commaEvery3 : List Char -> List Char
commaEvery3 chars =
    case chars of
        a :: b :: c :: d :: rest ->
            a :: b :: c :: ',' :: commaEvery3 (d :: rest)

        _ ->
            chars
