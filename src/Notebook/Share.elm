module Notebook.Share exposing (encode, decode, link)

{-| **Share a notebook by link.** A notebook is serialised to JSON ([`Notebook.Serialize`](Notebook-Serialize))
and then to a compact, URL-safe token — each character's Unicode code point as a dot-separated
integer, so the token is pure digits and dots (no escaping needed) and round-trips losslessly through
[`decode`](#decode). A whole notebook can therefore travel in a hyperlink: no server, no account. The
host shows the link to copy and a box to paste one back.

@docs encode, decode, link

-}

import Char
import Notebook.Doc exposing (Doc)
import Notebook.Serialize as Serialize


{-| Encode a notebook into a URL-safe token. -}
encode : Doc -> String
encode doc =
    Serialize.encode doc
        |> String.toList
        |> List.map (\c -> String.fromInt (Char.toCode c))
        |> String.join "."


{-| Decode a token produced by [`encode`](#encode) back into a notebook (`Nothing` if it is empty or
not a valid token). -}
decode : String -> Maybe Doc
decode token =
    if String.trim token == "" then
        Nothing

    else
        let
            codes =
                String.split "." token |> List.map String.toInt
        in
        if List.any ((==) Nothing) codes then
            Nothing

        else
            codes
                |> List.filterMap identity
                |> List.map Char.fromCode
                |> String.fromList
                |> Serialize.decode
                |> Result.toMaybe


{-| A full shareable link: the page's base URL with the notebook in a `#nb=` fragment. -}
link : String -> Doc -> String
link base doc =
    base ++ "#nb=" ++ encode doc
