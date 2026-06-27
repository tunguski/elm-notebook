module Notebook.View exposing (Config, notebook, suggestionsPanel, valueHtml, markdownHtml)

{-| The HTML view of a notebook: editable cells, their outputs (scalars, lists, tables,
errors), a tiny live Markdown renderer for prose cells, and the suggestions side-panel.

The view is decoupled from any application via a [`Config`](#Config) record of message
constructors — the host (`Main`) decides what each button does. All styling is class-based
(see `src/notebook.css`); nothing here reaches outside the notebook.

@docs Config, notebook, suggestionsPanel, valueHtml, markdownHtml

-}

import Html exposing (Html, a, button, div, h2, h3, h4, li, p, span, strong, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes as HA
import Html.Events as HE
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Output(..))
import Notebook.Doc exposing (Doc)
import Notebook.Suggest exposing (Suggestion)
import Notebook.Value as Value exposing (Value(..))


{-| The callbacks the host wires up for the notebook's interactive controls. -}
type alias Config msg =
    { onEdit : Int -> String -> msg
    , onRun : Int -> msg
    , onDelete : Int -> msg
    , onMoveUp : Int -> msg
    , onMoveDown : Int -> msg
    , onConvert : Int -> CellKind -> msg
    , onInsert : Suggestion -> msg
    }



-- NOTEBOOK -------------------------------------------------------------------


{-| Render the whole notebook: every cell, in order. -}
notebook : Config msg -> Doc -> Html msg
notebook config doc =
    div [ HA.class "nb-cells" ]
        (List.map (cellView config) doc.cells)


cellView : Config msg -> Cell -> Html msg
cellView config cell =
    case cell.kind of
        Code ->
            codeCellView config cell

        Markdown ->
            markdownCellView config cell


codeCellView : Config msg -> Cell -> Html msg
codeCellView config cell =
    div [ HA.class "nb-cell nb-cell-code" ]
        [ div [ HA.class "nb-gutter" ]
            [ span [ HA.class "nb-prompt" ] [ text (promptLabel "In" cell.count) ] ]
        , div [ HA.class "nb-body" ]
            [ sourceEditor config cell
            , cellToolbar config cell
            , outputView cell.output cell.count
            ]
        ]


markdownCellView : Config msg -> Cell -> Html msg
markdownCellView config cell =
    div [ HA.class "nb-cell nb-cell-md" ]
        [ div [ HA.class "nb-gutter" ] [ span [ HA.class "nb-prompt nb-prompt-md" ] [ text "md" ] ]
        , div [ HA.class "nb-body" ]
            [ markdownHtml cell.source
            , sourceEditor config cell
            , cellToolbar config cell
            ]
        ]


sourceEditor : Config msg -> Cell -> Html msg
sourceEditor config cell =
    textarea
        [ HA.class "nb-source"
        , HA.value cell.source
        , HA.placeholder (placeholderFor cell.kind)
        , HA.attribute "rows" (String.fromInt (editorRows cell.source))
        , HA.attribute "spellcheck" "false"
        , HE.onInput (config.onEdit cell.id)
        ]
        []


cellToolbar : Config msg -> Cell -> Html msg
cellToolbar config cell =
    let
        convertButton =
            case cell.kind of
                Code ->
                    toolButton "To text" (config.onConvert cell.id Markdown)

                Markdown ->
                    toolButton "To code" (config.onConvert cell.id Code)

        runButton =
            case cell.kind of
                Code ->
                    button
                        [ HA.class "nb-btn nb-btn-run", HE.onClick (config.onRun cell.id) ]
                        [ text "▶ Run" ]

                Markdown ->
                    text ""
    in
    div [ HA.class "nb-toolbar" ]
        [ runButton
        , div [ HA.class "nb-toolbar-spacer" ] []
        , toolButton "↑" (config.onMoveUp cell.id)
        , toolButton "↓" (config.onMoveDown cell.id)
        , convertButton
        , toolButton "✕" (config.onDelete cell.id)
        ]


toolButton : String -> msg -> Html msg
toolButton label msg =
    button [ HA.class "nb-btn nb-btn-ghost", HE.onClick msg ] [ text label ]


outputView : Output -> Maybe Int -> Html msg
outputView output count =
    case output of
        OutNone ->
            text ""

        OutError message ->
            div [ HA.class "nb-out nb-out-error" ]
                [ span [ HA.class "nb-prompt nb-prompt-err" ] [ text (promptLabel "Out" count) ]
                , div [ HA.class "nb-error" ] [ text message ]
                ]

        OutValue value ->
            div [ HA.class "nb-out" ]
                [ span [ HA.class "nb-prompt nb-prompt-out" ] [ text (promptLabel "Out" count) ]
                , div [ HA.class "nb-value" ] [ valueHtml value ]
                ]


promptLabel : String -> Maybe Int -> String
promptLabel tag count =
    case count of
        Just n ->
            tag ++ " [" ++ String.fromInt n ++ "]"

        Nothing ->
            tag ++ " [ ]"


placeholderFor : CellKind -> String
placeholderFor kind =
    case kind of
        Code ->
            "an expression, e.g.  mean (column \"salary\" people)"

        Markdown ->
            "# A heading, then notes in **markdown**…"


editorRows : String -> Int
editorRows source =
    String.lines source |> List.length |> clamp 2 20



-- VALUE RENDERING ------------------------------------------------------------


{-| Render a value: tables become grids, records become key/value tables, everything else a
typed, monospaced scalar.
-}
valueHtml : Value -> Html msg
valueHtml value =
    if Value.isTable value then
        tableHtml value

    else
        case value of
            VRecord fields ->
                recordHtml fields

            _ ->
                span [ HA.class (scalarClass value) ] [ text (scalarText value) ]


tableHtml : Value -> Html msg
tableHtml value =
    let
        cols =
            Value.tableColumns value

        rows =
            case value of
                VList xs ->
                    xs

                _ ->
                    []
    in
    div [ HA.class "nb-table-wrap" ]
        [ table [ HA.class "nb-table" ]
            [ thead [] [ tr [] (List.map (\c -> th [] [ text c ]) cols) ]
            , tbody [] (List.map (tableRow cols) rows)
            ]
        ]


tableRow : List String -> Value -> Html msg
tableRow cols row =
    case row of
        VRecord fields ->
            tr [] (List.map (\c -> td [] [ cellHtml (lookupField c fields) ]) cols)

        _ ->
            tr [] [ td [] [ valueHtml row ] ]


cellHtml : Maybe Value -> Html msg
cellHtml maybeValue =
    case maybeValue of
        Just value ->
            span [ HA.class (scalarClass value) ] [ text (scalarText value) ]

        Nothing ->
            text ""


recordHtml : List ( String, Value ) -> Html msg
recordHtml fields =
    div [ HA.class "nb-table-wrap" ]
        [ table [ HA.class "nb-table nb-record" ]
            [ tbody []
                (List.map
                    (\( k, v ) ->
                        tr []
                            [ th [] [ text k ]
                            , td [] [ span [ HA.class (scalarClass v) ] [ text (scalarText v) ] ]
                            ]
                    )
                    fields
                )
            ]
        ]


lookupField : String -> List ( String, Value ) -> Maybe Value
lookupField key fields =
    case fields of
        ( k, v ) :: rest ->
            if k == key then
                Just v

            else
                lookupField key rest

        [] ->
            Nothing


scalarText : Value -> String
scalarText value =
    case value of
        VStr str ->
            str

        _ ->
            Value.toInline value


scalarClass : Value -> String
scalarClass value =
    case value of
        VNum _ ->
            "nb-v nb-v-num"

        VStr _ ->
            "nb-v nb-v-str"

        VBool _ ->
            "nb-v nb-v-bool"

        _ ->
            "nb-v nb-v-other"



-- SUGGESTIONS ----------------------------------------------------------------


{-| The side-panel of context-aware next steps. -}
suggestionsPanel : (Suggestion -> msg) -> List Suggestion -> Html msg
suggestionsPanel onInsert items =
    div [ HA.class "nb-suggest" ]
        [ h3 [ HA.class "nb-suggest-title" ] [ text "Suggested next steps" ]
        , p [ HA.class "nb-suggest-lead" ]
            [ text "Based on your last result. Click one to add it as a new cell." ]
        , div [ HA.class "nb-suggest-list" ] (List.map (suggestionCard onInsert) items)
        ]


suggestionCard : (Suggestion -> msg) -> Suggestion -> Html msg
suggestionCard onInsert suggestion =
    button
        [ HA.class ("nb-suggest-card nb-suggest-" ++ kindClass suggestion.kind)
        , HA.title suggestion.source
        , HE.onClick (onInsert suggestion)
        ]
        [ span [ HA.class "nb-suggest-label" ] [ text suggestion.label ]
        , span [ HA.class "nb-suggest-detail" ] [ text suggestion.detail ]
        , span [ HA.class "nb-suggest-code" ] [ text (firstLine suggestion.source) ]
        ]


kindClass : CellKind -> String
kindClass kind =
    case kind of
        Code ->
            "code"

        Markdown ->
            "md"


firstLine : String -> String
firstLine source =
    case String.lines source of
        first :: _ ->
            first

        [] ->
            source



-- MINIMAL MARKDOWN -----------------------------------------------------------


{-| Render a small Markdown subset (headings, bullet lists, paragraphs; inline `**bold**`
and `` `code` ``) used by prose cells. -}
markdownHtml : String -> Html msg
markdownHtml source =
    div [ HA.class "nb-md" ]
        (blocksToHtml (groupLines (List.map classifyLine (String.lines source)) []))


type Line
    = LHead Int String
    | LItem String
    | LText String
    | LBlank


classifyLine : String -> Line
classifyLine raw =
    let
        line =
            String.trimRight raw
    in
    if String.startsWith "#" line then
        LHead (countHashes line) (String.trimLeft (String.dropLeft (countHashes line) line))

    else if String.startsWith "- " line then
        LItem (String.dropLeft 2 line)

    else if String.trim line == "" then
        LBlank

    else
        LText line


countHashes : String -> Int
countHashes line =
    String.toList line
        |> takeWhileCount ((==) '#')


takeWhileCount : (Char -> Bool) -> List Char -> Int
takeWhileCount pred chars =
    case chars of
        c :: rest ->
            if pred c then
                1 + takeWhileCount pred rest

            else
                0

        [] ->
            0


{-| Group classified lines into blocks: runs of text become a paragraph, runs of items a
list, headings stand alone. Carries pending paragraph/item buffers. -}
groupLines : List Line -> List Line -> List Block
groupLines lines pending =
    case lines of
        [] ->
            flushPending pending

        (LHead level body) :: rest ->
            flushPending pending ++ (BHead level body :: groupLines rest [])

        LBlank :: rest ->
            flushPending pending ++ groupLines rest []

        ((LItem _) as item) :: rest ->
            if isItemBuffer pending then
                groupLines rest (pending ++ [ item ])

            else
                flushPending pending ++ groupLines rest [ item ]

        ((LText _) as txt) :: rest ->
            if isTextBuffer pending then
                groupLines rest (pending ++ [ txt ])

            else
                flushPending pending ++ groupLines rest [ txt ]


type Block
    = BHead Int String
    | BPara (List String)
    | BList (List String)


isItemBuffer : List Line -> Bool
isItemBuffer pending =
    case pending of
        (LItem _) :: _ ->
            True

        _ ->
            False


isTextBuffer : List Line -> Bool
isTextBuffer pending =
    case pending of
        (LText _) :: _ ->
            True

        _ ->
            False


flushPending : List Line -> List Block
flushPending pending =
    case pending of
        [] ->
            []

        (LItem _) :: _ ->
            [ BList (List.filterMap itemText pending) ]

        _ ->
            [ BPara (List.filterMap textText pending) ]


itemText : Line -> Maybe String
itemText line =
    case line of
        LItem s ->
            Just s

        _ ->
            Nothing


textText : Line -> Maybe String
textText line =
    case line of
        LText s ->
            Just s

        _ ->
            Nothing


blocksToHtml : List Block -> List (Html msg)
blocksToHtml blocks =
    List.map blockToHtml blocks


blockToHtml : Block -> Html msg
blockToHtml block =
    case block of
        BHead level body ->
            headingTag level (inline body)

        BPara lines ->
            p [] (inline (String.join " " lines))

        BList items ->
            ul [] (List.map (\i -> li [] (inline i)) items)


headingTag : Int -> List (Html msg) -> Html msg
headingTag level children =
    if level <= 1 then
        h2 [ HA.class "nb-h1" ] children

    else if level == 2 then
        h3 [ HA.class "nb-h2" ] children

    else
        h4 [ HA.class "nb-h3" ] children


{-| Inline formatting: `**bold**` and `` `code` ``, otherwise plain text. -}
inline : String -> List (Html msg)
inline source =
    inlineScan (String.toList source) [] []


inlineScan : List Char -> List Char -> List (Html msg) -> List (Html msg)
inlineScan chars textRev nodes =
    case chars of
        [] ->
            List.reverse (flushText textRev nodes)

        '*' :: '*' :: rest ->
            case readUntil [ '*', '*' ] rest [] of
                Just ( inner, after ) ->
                    inlineScan after [] (strong [] [ text inner ] :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('*' :: '*' :: textRev) nodes

        '`' :: rest ->
            case readUntil [ '`' ] rest [] of
                Just ( inner, after ) ->
                    inlineScan after [] (Html.code [] [ text inner ] :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('`' :: textRev) nodes

        c :: rest ->
            inlineScan rest (c :: textRev) nodes


flushText : List Char -> List (Html msg) -> List (Html msg)
flushText textRev nodes =
    if List.isEmpty textRev then
        nodes

    else
        text (String.fromList (List.reverse textRev)) :: nodes


readUntil : List Char -> List Char -> List Char -> Maybe ( String, List Char )
readUntil delim chars acc =
    if matchesPrefix delim chars then
        Just ( String.fromList (List.reverse acc), List.drop (List.length delim) chars )

    else
        case chars of
            c :: rest ->
                readUntil delim rest (c :: acc)

            [] ->
                Nothing


matchesPrefix : List Char -> List Char -> Bool
matchesPrefix prefix chars =
    case ( prefix, chars ) of
        ( [], _ ) ->
            True

        ( p :: ps, c :: cs ) ->
            p == c && matchesPrefix ps cs

        ( _, [] ) ->
            False
