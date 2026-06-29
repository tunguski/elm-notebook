module Notebook.Share exposing (encode, decode, link)

{-| **Share a notebook by link.** A notebook is serialised to JSON ([`Notebook.Serialize`](Notebook-Serialize))
and percent-encoded into a compact, URL-safe token that round-trips through [`decode`](#decode), so a
whole notebook can travel in a hyperlink — no server, no account. The host shows the link to copy and
a box to paste one back.

@docs encode, decode, link

-}

import Notebook.Doc exposing (Doc)
import Notebook.Serialize as Serialize
import Url


{-| Encode a notebook into a URL-safe token (percent-encoded JSON). -}
encode : Doc -> String
encode doc =
    Url.percentEncode (Serialize.encode doc)


{-| Decode a token produced by [`encode`](#encode) back into a notebook (`Nothing` if it is not a
valid token). -}
decode : String -> Maybe Doc
decode token =
    Url.percentDecode token
        |> Maybe.andThen (\json -> Result.toMaybe (Serialize.decode json))


{-| A full shareable link: the page's base URL with the notebook in a `#nb=` fragment. -}
link : String -> Doc -> String
link base doc =
    base ++ "#nb=" ++ encode doc
