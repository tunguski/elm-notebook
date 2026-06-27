module Notebook.Csv exposing (toElm)

{-| Turn pasted CSV/TSV text into a runnable Elm table — `name = [ { col = val, … }, … ]` — so a
spreadsheet export drops straight into the notebook as a real `List` of records.

The delimiter (comma or tab) is auto-detected from the header. Column headers are sanitised into
valid Elm field names (and de-duplicated). A column whose every value parses as a number is emitted
as numbers; otherwise its values are quoted strings. There is no quoted-field/escaped-delimiter
handling — this is for quick paste-and-explore, not a full RFC-4180 parser.

@docs toElm

-}


{-| Generate the Elm source binding `name` to the parsed table, or fail with a message. -}
toElm : String -> String -> Result String String
toElm name csv =
    parse csv
        |> Result.andThen
            (\( headers, rows ) ->
                if List.isEmpty headers then
                    Err "No columns found."

                else if List.isEmpty rows then
                    Err "No data rows found (need a header line and at least one row)."

                else
                    Ok (render (safeName name) (dedupe (List.map sanitizeField headers)) rows)
            )


parse : String -> Result String ( List String, List (List String) )
parse csv =
    let
        lines =
            String.lines (String.trim csv)
                |> List.filter (\l -> String.trim l /= "")
    in
    case lines of
        [] ->
            Err "Empty input."

        header :: dataLines ->
            let
                delim =
                    detectDelim header
            in
            Ok ( splitRow delim header, List.map (splitRow delim) dataLines )


detectDelim : String -> String
detectDelim line =
    if countOccurrences "\t" line > countOccurrences "," line then
        "\t"

    else
        ","


countOccurrences : String -> String -> Int
countOccurrences needle haystack =
    List.length (String.indexes needle haystack)


splitRow : String -> String -> List String
splitRow delim line =
    String.split delim line |> List.map String.trim



-- RENDERING ------------------------------------------------------------------


render : String -> List String -> List (List String) -> String
render name headers rows =
    let
        numericFlags =
            List.indexedMap (\i _ -> columnNumeric i rows) headers

        recordOf row =
            "{ "
                ++ String.join ", "
                    (List.indexedMap
                        (\i h -> h ++ " = " ++ cellLiteral (isNumeric i numericFlags) (nth i row))
                        headers
                    )
                ++ " }"

        body =
            String.join "\n    , " (List.map recordOf rows)
    in
    name ++ " =\n    [ " ++ body ++ "\n    ]"


columnNumeric : Int -> List (List String) -> Bool
columnNumeric i rows =
    let
        cells =
            List.map (nth i) rows
    in
    not (List.isEmpty cells)
        && List.all (\c -> c /= "" && String.toFloat c /= Nothing) cells


isNumeric : Int -> List Bool -> Bool
isNumeric i flags =
    nthBool i flags


cellLiteral : Bool -> String -> String
cellLiteral numeric value =
    if numeric then
        if value == "" then
            "0"

        else
            value

    else
        "\"" ++ escape value ++ "\""


escape : String -> String
escape s =
    s
        |> String.replace "\\" "\\\\"
        |> String.replace "\"" "\\\""



-- FIELD NAMES ----------------------------------------------------------------


sanitizeField : String -> String
sanitizeField raw =
    let
        cleaned =
            String.toList (String.trim raw)
                |> List.map keepIdentChar
                |> String.fromList
                |> String.toLower
    in
    case String.uncons cleaned of
        Just ( first, rest ) ->
            if Char.isLower first then
                String.cons first rest

            else
                "c_" ++ cleaned

        Nothing ->
            "col"


keepIdentChar : Char -> Char
keepIdentChar c =
    if Char.isAlphaNum c || c == '_' then
        c

    else
        '_'


{-| Make duplicate field names unique by suffixing `2`, `3`, … in order. -}
dedupe : List String -> List String
dedupe names =
    let
        step name ( seen, acc ) =
            let
                count =
                    List.length (List.filter ((==) name) seen)

                unique =
                    if count == 0 then
                        name

                    else
                        name ++ String.fromInt (count + 1)
            in
            ( name :: seen, acc ++ [ unique ] )
    in
    List.foldl step ( [], [] ) names |> Tuple.second


safeName : String -> String
safeName raw =
    case sanitizeField raw of
        "" ->
            "data"

        other ->
            other



-- HELPERS --------------------------------------------------------------------


nth : Int -> List String -> String
nth i xs =
    List.drop i xs |> List.head |> Maybe.withDefault ""


nthBool : Int -> List Bool -> Bool
nthBool i xs =
    List.drop i xs |> List.head |> Maybe.withDefault False
