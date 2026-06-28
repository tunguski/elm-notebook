module Notebook.Import exposing (toElm, looksLikeJson)

{-| Turn pasted data into a runnable Elm table cell, **auto-detecting** the format: a JSON array of
objects, or (delegating to [`Notebook.Csv`](Notebook-Csv)) CSV / TSV. The result binds `name` to a
`List` of records — `name = [ { col = val, … }, … ]` — so the kernel runs it like any other cell and
later cells can explore the data.

JSON scalars become Elm literals (strings quoted, numbers bare, booleans `True`/`False`, `null` → 0);
the interpreter is dynamically typed, so columns needn't be uniform.

@docs toElm, looksLikeJson

-}

import Json.Decode as D
import Notebook.Csv as Csv


{-| Generate the Elm binding for pasted `text`, choosing JSON or CSV/TSV by its shape. -}
toElm : String -> String -> Result String String
toElm name text =
    if looksLikeJson text then
        jsonToElm name text

    else
        Csv.toElm name text


{-| Does this text look like a JSON array (its first non-space character is `[`)? -}
looksLikeJson : String -> Bool
looksLikeJson text =
    String.startsWith "[" (String.trimLeft text)


jsonToElm : String -> String -> Result String String
jsonToElm name text =
    case D.decodeString (D.list rowDecoder) text of
        Ok [] ->
            Err "the JSON array is empty"

        Ok rows ->
            Ok (name ++ " =\n    [ " ++ String.join "\n    , " (List.map recordLiteral rows) ++ "\n    ]")

        Err err ->
            Err ("could not read JSON (expected an array of objects): " ++ D.errorToString err)


recordLiteral : List ( String, String ) -> String
recordLiteral fields =
    "{ " ++ String.join ", " (List.map (\( k, v ) -> sanitize k ++ " = " ++ v) fields) ++ " }"


rowDecoder : D.Decoder (List ( String, String ))
rowDecoder =
    D.keyValuePairs scalarLiteral


{-| Decode one JSON scalar to the Elm literal source that produces it. -}
scalarLiteral : D.Decoder String
scalarLiteral =
    D.oneOf
        [ D.string |> D.map (\s -> "\"" ++ escape s ++ "\"")
        , D.float |> D.map String.fromFloat
        , D.bool
            |> D.map
                (\b ->
                    if b then
                        "True"

                    else
                        "False"
                )
        , D.null "0"
        ]


escape : String -> String
escape s =
    s |> String.replace "\\" "\\\\" |> String.replace "\"" "\\\""


{-| Turn an arbitrary JSON key into a valid lowercase Elm field name. -}
sanitize : String -> String
sanitize key =
    let
        cleaned =
            key
                |> String.toLower
                |> String.toList
                |> List.map
                    (\c ->
                        if Char.isAlphaNum c then
                            c

                        else
                            '_'
                    )
                |> String.fromList
    in
    case String.toList cleaned of
        first :: _ ->
            if Char.isDigit first then
                "f_" ++ cleaned

            else
                cleaned

        [] ->
            "field"
