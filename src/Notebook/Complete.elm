module Notebook.Complete exposing (currentToken, completions, apply)

{-| **Code completion** for a cell: the identifier being typed at the caret, the in-scope names that
extend it, and how to splice a chosen completion back into the source.

It is deliberately small — a prefix match over the names the kernel currently knows (the prelude, the
standard library and everything defined in earlier cells) — but that is most of the value: it turns
"what was that function called again?" into a Tab.

@docs currentToken, completions, apply

-}


{-| The identifier immediately to the left of the caret (the run of letters / digits / `_` ending at
`caret`), or `""` if the character before the caret isn't part of an identifier. A `.` ends the token,
so `List.ma|` yields `ma`. -}
currentToken : String -> Int -> String
currentToken source caret =
    String.left caret source
        |> String.reverse
        |> takeIdent
        |> String.reverse


takeIdent : String -> String
takeIdent s =
    case String.uncons s of
        Just ( c, rest ) ->
            if isIdentChar c then
                String.cons c (takeIdent rest)

            else
                ""

        Nothing ->
            ""


isIdentChar : Char -> Bool
isIdentChar c =
    Char.isAlphaNum c || c == '_'


{-| The in-scope `names` that complete the token at the caret: a prefix match, sorted, de-duplicated,
excluding an exact match, capped to a handful. Empty when there's no token to complete. -}
completions : String -> Int -> List String -> List String
completions source caret names =
    let
        token =
            currentToken source caret
    in
    if token == "" then
        []

    else
        names
            |> List.filter (\name -> name /= token && String.startsWith token name)
            |> List.sort
            |> dedup
            |> List.take 8


dedup : List String -> List String
dedup names =
    case names of
        a :: b :: rest ->
            if a == b then
                dedup (b :: rest)

            else
                a :: dedup (b :: rest)

        _ ->
            names


{-| Replace the token at the caret with `chosen`, returning the new source and the new caret offset
(just past the inserted name). -}
apply : String -> Int -> String -> ( String, Int )
apply source caret chosen =
    let
        start =
            caret - String.length (currentToken source caret)
    in
    ( String.left start source ++ chosen ++ String.dropLeft caret source
    , start + String.length chosen
    )
