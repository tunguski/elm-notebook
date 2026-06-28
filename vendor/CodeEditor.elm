module CodeEditor exposing (Config, Chord, view)

{-| A small, self-contained syntax-highlighted code-editing widget, factored out of the editor so it
can be reused over **any** language — not just the interpreted Elm of `Editor`. It is the
react-simple-code-editor technique: a transparent `<textarea>` (which owns the caret, selection and
typing) layered exactly over a `<pre>` of coloured `<span>`s. Both share the same font, padding and
wrapping, so the highlighted text underneath stays aligned with what is typed. A line-number gutter
runs down the left, highlighting the caret's line.

It is deliberately decoupled from `Editor`'s `Msg`: you pass a `highlight` function (e.g.
`Highlight.segments` for Elm, `Highlight.cssSegments` for CSS) and an `onChange` that receives the
new text together with the caret offset. The embedder owns all state. The Bootstrap theme builder
uses it with `Highlight.cssSegments` for its single editable CSS file.

The widget renders these classes, which the embedder is expected to style (they intentionally do not
clash with `Editor`'s own `ed-*` classes):

  - `ce-editor` / `ce-flex` / `ce-area`  layout
  - `ce-gutter` / `ce-gutter-line` (`.current`)  the line-number gutter
  - `ce-code` (shared by both layers) · `ce-pre` (highlight) · `ce-textarea` (input)
  - `ce-seg` and `ce-seg-<kind>`  one per highlighter token class

-}

import Html exposing (Html, div, pre, span, text, textarea)
import Html.Attributes exposing (class, classList, value)
import Html.Events exposing (on, preventDefaultOn)
import Json.Decode as Decode


{-| How to drive the editor:

  - `source` — the current text.
  - `caret` — the caret offset, used only to highlight the current line in the gutter.
  - `highlight` — turns the source into `(class, text)` segments (e.g. `Highlight.cssSegments`).
  - `onChange` — called with the new text and the new caret offset on every edit.

-}
type alias Config msg =
    { source : String
    , caret : Int
    , gutter : Bool
    , highlight : String -> List ( String, String )
    , onChange : String -> Int -> msg
    , onKey : Maybe (Chord -> Maybe msg)
    }


{-| A keydown reduced to what the host cares about: the key name and the modifiers held. The host's
`onKey` returns `Just msg` to handle (and swallow) the keystroke, or `Nothing` to let it through. -}
type alias Chord =
    { key : String, shift : Bool, ctrl : Bool, meta : Bool, alt : Bool }


view : Config msg -> Html msg
view cfg =
    div [ class "ce-editor" ]
        [ div [ class "ce-flex" ]
            [ if cfg.gutter then
                gutter cfg.source cfg.caret

              else
                text ""
            , div [ class "ce-area" ]
                [ pre [ class "ce-code ce-pre" ]
                    (List.map renderSegment (cfg.highlight cfg.source) ++ [ text "\n" ])
                , textarea
                    (onEdit cfg.onChange
                        :: value cfg.source
                        :: class "ce-code ce-textarea"
                        :: keyAttr cfg.onKey
                    )
                    []
                ]
            ]
        ]


{-| Route keydowns through the host's `onKey`: a `Just msg` handles and swallows the keystroke (so
e.g. Ctrl+Enter runs without inserting a newline, Alt+Arrow moves without scrolling); a `Nothing`
lets the key fall through to normal text editing. -}
keyAttr : Maybe (Chord -> Maybe msg) -> List (Html.Attribute msg)
keyAttr maybeHandler =
    case maybeHandler of
        Nothing ->
            []

        Just handler ->
            [ preventDefaultOn "keydown" (chordDecoder handler) ]


chordDecoder : (Chord -> Maybe msg) -> Decode.Decoder ( msg, Bool )
chordDecoder handler =
    Decode.map5 Chord
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "altKey" Decode.bool)
        |> Decode.andThen
            (\chord ->
                case handler chord of
                    Just msg ->
                        Decode.succeed ( msg, True )

                    Nothing ->
                        Decode.fail "unhandled key"
            )


{-| A `<textarea>` input handler capturing both the new text and the caret offset (`selectionStart`). -}
onEdit : (String -> Int -> msg) -> Html.Attribute msg
onEdit toMsg =
    on "input"
        (Decode.map2 toMsg
            (Decode.at [ "target", "value" ] Decode.string)
            (Decode.at [ "target", "selectionStart" ] Decode.int)
        )


{-| A line-number gutter beside the code, highlighting the line the caret is on. Aligned to the code
by sharing its font, size, line-height and top padding. -}
gutter : String -> Int -> Html msg
gutter source caret =
    let
        lineCount =
            List.length (String.lines source)

        current =
            currentLine source caret
    in
    div [ class "ce-gutter" ]
        (List.map (gutterLine current) (List.range 1 lineCount))


gutterLine : Int -> Int -> Html msg
gutterLine current n =
    div [ classList [ ( "ce-gutter-line", True ), ( "current", n == current ) ] ]
        [ text (String.fromInt n) ]


{-| The 1-based line the caret sits on (one past the newlines before it). -}
currentLine : String -> Int -> Int
currentLine source caret =
    1 + List.length (List.filter ((==) '\n') (String.toList (String.left caret source)))


renderSegment : ( String, String ) -> Html msg
renderSegment ( cls, txt ) =
    span [ class (segClass cls) ] [ text txt ]


{-| The CSS class for each highlighter token kind (`""` is the default foreground). -}
segClass : String -> String
segClass cls =
    if cls == "" then
        "ce-seg"

    else
        "ce-seg-" ++ cls
