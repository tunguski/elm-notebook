module Notebook.Hint exposing (unboundName, closest, distance)

{-| **Smart error help.** When a cell fails because it mentions a name that isn't in scope — a typo,
usually — these helpers turn the raw interpreter message into a useful "did you mean …?" suggestion:
[`unboundName`](#unboundName) pulls the offending name out of the message, and [`closest`](#closest)
finds the nearest in-scope name by [edit distance](#distance).

@docs unboundName, closest, distance

-}


{-| The variable name an "undefined / unbound variable: X" error is complaining about, if that's
what the message is. -}
unboundName : String -> Maybe String
unboundName message =
    if String.contains "variable" (String.toLower message) then
        case String.split ":" message of
            _ :: rest ->
                let
                    name =
                        String.join ":" rest
                            |> String.trim
                            |> String.words
                            |> List.head
                            |> Maybe.withDefault ""
                in
                if name == "" then
                    Nothing

                else
                    Just name

            [] ->
                Nothing

    else
        Nothing


{-| The candidate name closest to `target` by edit distance, if one is near enough (within roughly a
third of the name's length, so unrelated names don't produce noise). `_` and an exact self-match are
ignored. -}
closest : String -> List String -> Maybe String
closest target candidates =
    candidates
        |> List.filter (\c -> c /= "_" && c /= target && c /= "")
        |> List.map (\c -> ( distance target c, c ))
        |> List.sortBy Tuple.first
        |> List.head
        |> Maybe.andThen
            (\( d, c ) ->
                if d <= threshold target then
                    Just c

                else
                    Nothing
            )


threshold : String -> Int
threshold target =
    -- allow a transposition (distance 2) on short names, more on longer ones
    max 2 (String.length target // 3)


{-| The Levenshtein edit distance between two strings (single-row dynamic programming). -}
distance : String -> String -> Int
distance a b =
    let
        bs =
            String.toList b

        row0 =
            List.range 0 (List.length bs)

        nextRow ( i, ca ) prev =
            let
                folder ( bj, diag, up ) ( left, acc ) =
                    let
                        cost =
                            if bj == ca then
                                0

                            else
                                1

                        v =
                            Basics.min (Basics.min (left + 1) (up + 1)) (diag + cost)
                    in
                    ( v, acc ++ [ v ] )

                ( _, rest ) =
                    List.foldl folder
                        ( i + 1, [] )
                        (List.map3 (\bj diag up -> ( bj, diag, up )) bs prev (List.drop 1 prev))
            in
            (i + 1) :: rest
    in
    List.foldl nextRow row0 (List.indexedMap (\i ca -> ( i, ca )) (String.toList a))
        |> List.reverse
        |> List.head
        |> Maybe.withDefault (List.length bs)
