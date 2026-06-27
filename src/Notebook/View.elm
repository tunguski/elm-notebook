module Notebook.View exposing (Config, notebook, suggestionsPanel, valueHtml, markdownHtml)

{-| The HTML view of a notebook: syntax-highlighted, auto-growing editors, outputs rendered as
scalars / records / tables (recursively, so nested tables nest) / headerless 2-D grids / errors,
a small live Markdown renderer (with nested lists), and the suggestions side-panel.

Editing uses the vendored [`CodeEditor`](CodeEditor) widget (a transparent `<textarea>` over a
highlighted `<pre>` — the same technique as elm-editor), which grows to its content so no cell
ever shows a scrollbar. The view is decoupled from any application via a [`Config`](#Config) of
message constructors.

@docs Config, notebook, suggestionsPanel, valueHtml, markdownHtml

-}

import CodeEditor
import Highlight
import Html exposing (Html, a, button, div, h2, h3, h4, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as HA
import Html.Events as HE
import Lang exposing (Value(..))
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Output(..))
import Notebook.Doc exposing (Doc)
import Notebook.Suggest exposing (Suggestion)
import Notebook.Value as Value


{-| The callbacks the host wires up for the notebook's interactive controls. -}
type alias Config msg =
    { onEdit : Int -> String -> Int -> msg
    , onRun : Int -> msg
    , onDelete : Int -> msg
    , onMoveUp : Int -> msg
    , onMoveDown : Int -> msg
    , onConvert : Int -> CellKind -> msg
    , onInsert : Suggestion -> msg
    , caretOf : Int -> Int
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
            [ CodeEditor.view
                { source = cell.source
                , caret = config.caretOf cell.id
                , gutter = True
                , highlight = Highlight.segments
                , onChange = config.onEdit cell.id
                }
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
            , CodeEditor.view
                { source = cell.source
                , caret = config.caretOf cell.id
                , gutter = False
                , highlight = plainSegments
                , onChange = config.onEdit cell.id
                }
            , cellToolbar config cell
            ]
        ]


plainSegments : String -> List ( String, String )
plainSegments s =
    [ ( "", s ) ]


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



-- VALUE RENDERING (recursive) ------------------------------------------------


{-| Render a value. Tables become grids (with a header row), 2-D arrays headerless grids,
records key/value tables, and any nested record/list inside a cell renders as a nested table —
all the way down.
-}
valueHtml : Value -> Html msg
valueHtml value =
    if Value.isTable value then
        tableHtml value

    else if Value.is2D value then
        grid2D value

    else
        case value of
            VRecord fields ->
                recordHtml fields

            VList items ->
                if List.all isScalar items then
                    scalarSpan value

                else
                    stackHtml items

            VTup items ->
                if List.all isScalar items then
                    scalarSpan value

                else
                    tupleHtml items

            _ ->
                scalarSpan value


tableHtml : Value -> Html msg
tableHtml value =
    let
        cols =
            Value.tableColumns value

        rows =
            itemsOf value
    in
    wrapTable
        [ thead [] [ tr [] (List.map (\c -> th [] [ text c ]) cols) ]
        , tbody [] (List.map (tableRow cols) rows)
        ]


tableRow : List String -> Value -> Html msg
tableRow cols row =
    tr [] (List.map (\c -> td [] [ cellHtml (Value.fieldOf c row) ]) cols)


cellHtml : Maybe Value -> Html msg
cellHtml maybeValue =
    case maybeValue of
        Just value ->
            valueHtml value

        Nothing ->
            text ""


grid2D : Value -> Html msg
grid2D value =
    wrapTable
        [ tbody []
            (List.map
                (\cells -> tr [] (List.map (\c -> td [] [ valueHtml c ]) cells))
                (Value.rows2D value)
            )
        ]


recordHtml : List ( String, Value ) -> Html msg
recordHtml fields =
    wrapTable
        [ tbody []
            (List.map
                (\( k, v ) -> tr [] [ th [] [ text k ], td [] [ valueHtml v ] ])
                fields
            )
        ]


tupleHtml : List Value -> Html msg
tupleHtml items =
    wrapTable [ tbody [] [ tr [] (List.map (\v -> td [] [ valueHtml v ]) items) ] ]


stackHtml : List Value -> Html msg
stackHtml items =
    div [ HA.class "nb-stack" ] (List.map (\v -> div [ HA.class "nb-stack-item" ] [ valueHtml v ]) items)


wrapTable : List (Html msg) -> Html msg
wrapTable inner =
    div [ HA.class "nb-table-wrap" ] [ table [ HA.class "nb-table" ] inner ]


scalarSpan : Value -> Html msg
scalarSpan value =
    span [ HA.class (scalarClass value) ] [ text (scalarText value) ]


scalarText : Value -> String
scalarText value =
    case value of
        VStr s ->
            s

        _ ->
            Value.inlineValue value


scalarClass : Value -> String
scalarClass value =
    case value of
        VNum _ ->
            "nb-v nb-v-num"

        VStr _ ->
            "nb-v nb-v-str"

        VBool _ ->
            "nb-v nb-v-bool"

        VChar _ ->
            "nb-v nb-v-str"

        _ ->
            "nb-v nb-v-other"


isScalar : Value -> Bool
isScalar value =
    case value of
        VNum _ ->
            True

        VBool _ ->
            True

        VStr _ ->
            True

        VChar _ ->
            True

        VCtor _ [] ->
            True

        _ ->
            False


itemsOf : Value -> List Value
itemsOf value =
    case value of
        VList items ->
            items

        _ ->
            []



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



-- MINIMAL MARKDOWN (headings, nested lists, paragraphs; **bold**, `code`) -----


{-| Render a small Markdown subset used by prose cells: headings, nested bullet lists,
paragraphs, and inline `**bold**` / `` `code` ``. -}
markdownHtml : String -> Html msg
markdownHtml source =
    div [ HA.class "nb-md" ]
        (blocksToHtml (groupBlocks (List.map classifyLine (String.lines source)) []))


type Line
    = LHead Int String
    | LItem Int String
    | LText String
    | LBlank


type Block
    = BHead Int String
    | BPara (List String)
    | BList (List ( Int, String ))


classifyLine : String -> Line
classifyLine raw =
    let
        line =
            String.trimRight raw

        trimmedLeft =
            String.trimLeft line

        indent =
            String.length line - String.length trimmedLeft
    in
    if String.startsWith "#" trimmedLeft then
        LHead (countHashes trimmedLeft) (String.trimLeft (String.dropLeft (countHashes trimmedLeft) trimmedLeft))

    else if String.startsWith "- " trimmedLeft then
        LItem indent (String.dropLeft 2 trimmedLeft)

    else if String.trim line == "" then
        LBlank

    else
        LText line


countHashes : String -> Int
countHashes line =
    String.toList line |> takeWhileCount ((==) '#')


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


groupBlocks : List Line -> List Line -> List Block
groupBlocks lines pending =
    case lines of
        [] ->
            flushPending pending

        (LHead level body) :: rest ->
            flushPending pending ++ (BHead level body :: groupBlocks rest [])

        LBlank :: rest ->
            flushPending pending ++ groupBlocks rest []

        ((LItem _ _) as item) :: rest ->
            if isItemBuffer pending then
                groupBlocks rest (pending ++ [ item ])

            else
                flushPending pending ++ groupBlocks rest [ item ]

        ((LText _) as txt) :: rest ->
            if isTextBuffer pending then
                groupBlocks rest (pending ++ [ txt ])

            else
                flushPending pending ++ groupBlocks rest [ txt ]


isItemBuffer : List Line -> Bool
isItemBuffer pending =
    case pending of
        (LItem _ _) :: _ ->
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

        (LItem _ _) :: _ ->
            [ BList (List.filterMap itemPair pending) ]

        _ ->
            [ BPara (List.filterMap textText pending) ]


itemPair : Line -> Maybe ( Int, String )
itemPair line =
    case line of
        LItem indent s ->
            Just ( indent, s )

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
            ul [] (nestItems items)


{-| Build (possibly nested) `<li>`s: items more indented than the current one become a nested
`<ul>` inside the preceding `<li>`. -}
nestItems : List ( Int, String ) -> List (Html msg)
nestItems items =
    case items of
        [] ->
            []

        ( indent, body ) :: rest ->
            let
                ( children, remaining ) =
                    spanWhile (\( i, _ ) -> i > indent) rest

                childHtml =
                    if List.isEmpty children then
                        []

                    else
                        [ ul [] (nestItems children) ]
            in
            li [] (inline body ++ childHtml) :: nestItems remaining


spanWhile : (a -> Bool) -> List a -> ( List a, List a )
spanWhile pred xs =
    case xs of
        x :: rest ->
            if pred x then
                let
                    ( taken, remaining ) =
                        spanWhile pred rest
                in
                ( x :: taken, remaining )

            else
                ( [], xs )

        [] ->
            ( [], [] )


headingTag : Int -> List (Html msg) -> Html msg
headingTag level children =
    if level <= 1 then
        h2 [ HA.class "nb-h1" ] children

    else if level == 2 then
        h3 [ HA.class "nb-h2" ] children

    else
        h4 [ HA.class "nb-h3" ] children


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
