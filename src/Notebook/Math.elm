module Notebook.Math exposing (inline, replaceSymbols)

{-| Lightweight inline **math** for Markdown cells: the text between `$…$` is rendered with Greek
letters and common operators substituted (`\alpha` ⇒ α, `\leq` ⇒ ≤, `\sum` ⇒ ∑, …), `^` raised to a
superscript and `_` lowered to a subscript. It is not a full TeX engine — just enough that a formula
like `$\sigma^2 = \frac{1}{n}\sum (x_i - \mu)^2$` reads as mathematics rather than source.

@docs inline, replaceSymbols

-}

import Html exposing (Html, span, sub, sup, text)
import Html.Attributes as HA


{-| Render the contents of a `$…$` span as HTML. -}
inline : String -> Html msg
inline source =
    span [ HA.class "nb-math" ] (scan (String.toList (expandFrac (replaceSymbols source))) [] [])


{-| Rewrite each `\frac{a}{b}` to `a⁄b` (with the fraction slash) before the main scan. -}
expandFrac : String -> String
expandFrac source =
    String.fromList (fracScan (String.toList source))


fracScan : List Char -> List Char
fracScan chars =
    case chars of
        '\\' :: 'f' :: 'r' :: 'a' :: 'c' :: '{' :: rest ->
            let
                ( numer, afterNum ) =
                    readBrace rest []

                ( denom, afterDen ) =
                    case afterNum of
                        '{' :: more ->
                            readBrace more []

                        _ ->
                            ( "", afterNum )
            in
            String.toList numer ++ ('⁄' :: String.toList denom) ++ fracScan afterDen

        c :: rest ->
            c :: fracScan rest

        [] ->
            []


{-| Substitute every known `\name` macro with its Unicode symbol (longer names first, so `\leq`
beats `\le`). Exposed for testing. -}
replaceSymbols : String -> String
replaceSymbols source =
    List.foldl (\( from, to ) acc -> String.replace from to acc) source symbols


symbols : List ( String, String )
symbols =
    [ ( "\\alpha", "α" )
    , ( "\\beta", "β" )
    , ( "\\gamma", "γ" )
    , ( "\\delta", "δ" )
    , ( "\\epsilon", "ε" )
    , ( "\\theta", "θ" )
    , ( "\\lambda", "λ" )
    , ( "\\mu", "μ" )
    , ( "\\pi", "π" )
    , ( "\\rho", "ρ" )
    , ( "\\sigma", "σ" )
    , ( "\\tau", "τ" )
    , ( "\\phi", "φ" )
    , ( "\\omega", "ω" )
    , ( "\\Delta", "Δ" )
    , ( "\\Sigma", "Σ" )
    , ( "\\Omega", "Ω" )
    , ( "\\sum", "∑" )
    , ( "\\prod", "∏" )
    , ( "\\sqrt", "√" )
    , ( "\\infty", "∞" )
    , ( "\\partial", "∂" )
    , ( "\\times", "×" )
    , ( "\\cdot", "·" )
    , ( "\\div", "÷" )
    , ( "\\pm", "±" )
    , ( "\\approx", "≈" )
    , ( "\\neq", "≠" )
    , ( "\\leq", "≤" )
    , ( "\\le", "≤" )
    , ( "\\geq", "≥" )
    , ( "\\ge", "≥" )
    , ( "\\rightarrow", "→" )
    , ( "\\leftarrow", "←" )
    , ( "\\to", "→" )
    , ( "\\in", "∈" )
    ]


scan : List Char -> List Char -> List (Html msg) -> List (Html msg)
scan chars buf nodes =
    case chars of
        [] ->
            List.reverse (flush buf nodes)

        '^' :: rest ->
            let
                ( inner, after ) =
                    readArg rest
            in
            scan after [] (sup [] [ text inner ] :: flush buf nodes)

        '_' :: rest ->
            let
                ( inner, after ) =
                    readArg rest
            in
            scan after [] (sub [] [ text inner ] :: flush buf nodes)

        c :: rest ->
            scan rest (c :: buf) nodes


{-| The argument of a `^`/`_`: a `{ … }` group, or the single next character. -}
readArg : List Char -> ( String, List Char )
readArg chars =
    case chars of
        '{' :: rest ->
            readBrace rest []

        c :: rest ->
            ( String.fromList [ c ], rest )

        [] ->
            ( "", [] )


readBrace : List Char -> List Char -> ( String, List Char )
readBrace chars acc =
    case chars of
        '}' :: rest ->
            ( String.fromList (List.reverse acc), rest )

        c :: rest ->
            readBrace rest (c :: acc)

        [] ->
            ( String.fromList (List.reverse acc), [] )


flush : List Char -> List (Html msg) -> List (Html msg)
flush buf nodes =
    if List.isEmpty buf then
        nodes

    else
        text (String.fromList (List.reverse buf)) :: nodes
