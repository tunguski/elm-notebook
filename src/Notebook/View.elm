module Notebook.View exposing (Config, notebook, suggestionsPanel, variablesPanel, errorsPanel, outlinePanel, overviewPanel, valueHtml, markdownHtml, interpolate)

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
import Html exposing (Html, a, button, div, h2, h3, h4, input, li, p, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as HA
import Html.Events as HE
import Lang exposing (Value(..))
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Control(..), Output(..))
import Notebook.Chart as Chart exposing (ChartKind)
import Notebook.Complete as Complete
import Notebook.Correlation as Correlation
import Notebook.Doc exposing (Doc)
import Notebook.Filter as Filter
import Notebook.Format as Format
import Notebook.GroupBy as GroupBy
import Notebook.Heatmap as Heatmap
import Notebook.Math as Math
import Notebook.Outline exposing (Heading)
import Notebook.Overview as Overview
import Notebook.Pivot as Pivot
import Notebook.Profile as Profile
import Notebook.Sparkline as Sparkline
import Notebook.Suggest exposing (Suggestion)
import Notebook.Value as Value
import Set exposing (Set)


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
    , chartOf : Int -> Maybe ChartKind
    , onChart : Int -> Maybe ChartKind -> msg
    , colOf : Int -> Maybe String
    , onCol : Int -> String -> msg
    , onInputValue : Int -> String -> msg
    , onInputName : Int -> String -> msg
    , onInputControl : Int -> String -> msg
    , commentsVisible : Bool
    , commentCountOf : Int -> Int
    , exportCell : Cell -> Html msg
    , isStale : Int -> Bool
    , errorFix : Cell -> Maybe { label : String, fixed : String }
    , onFix : Int -> String -> msg
    , tableSort : Int -> Maybe ( String, Bool )
    , onSort : Int -> String -> msg
    , tableFilter : Int -> String
    , onFilter : Int -> String -> msg
    , tableExpanded : Int -> Bool
    , onExpand : Int -> Bool -> msg
    , evalInline : String -> Maybe String
    , isProfile : Int -> Bool
    , onProfile : Int -> Bool -> msg
    , pivotOf : Int -> Maybe Pivot.Spec
    , onPivot : Int -> Bool -> msg
    , onPivotRow : Int -> String -> msg
    , onPivotColumn : Int -> String -> msg
    , onPivotValue : Int -> String -> msg
    , onPivotAgg : Int -> String -> msg
    , isCorr : Int -> Bool
    , onCorr : Int -> Bool -> msg
    , report : Bool
    , activeCell : Maybe Int
    , scopeNames : List String
    , findQuery : Maybe String
    , isFolded : Int -> Bool
    , onFold : Int -> msg
    , onDuplicate : Int -> msg
    , onInsertAbove : Int -> msg
    , onInsertBelow : Int -> msg
    , onRunAbove : Int -> msg
    , onRunBelow : Int -> msg
    , heatOn : Int -> Bool
    , onHeat : Int -> Bool -> msg
    , footerOn : Int -> Bool
    , onFooter : Int -> Bool -> msg
    , hiddenCols : Int -> Set String
    , onToggleCol : Int -> String -> msg
    , groupOf : Int -> Maybe GroupBy.Spec
    , onGroup : Int -> Bool -> msg
    , onGroupKey : Int -> String -> msg
    , onGroupValue : Int -> String -> msg
    , onGroupAgg : Int -> String -> msg
    , numFormat : Int -> Format.Format
    , onNumFormat : Int -> msg
    , colFiltersOf : Int -> List Filter.Clause
    , onAddFilter : Int -> msg
    , onRemoveFilter : Int -> Int -> msg
    , onFilterCol : Int -> Int -> String -> msg
    , onFilterOp : Int -> Int -> String -> msg
    , onFilterValue : Int -> Int -> String -> msg
    , barsOn : Int -> Bool
    , onBars : Int -> Bool -> msg
    , sectionFolded : Int -> Bool
    , onFoldSection : Int -> msg
    }


{-| Does this cell's source contain the active find query? -}
cellMatches : Config msg -> Cell -> Bool
cellMatches config cell =
    case config.findQuery of
        Just q ->
            q /= "" && String.contains q cell.source

        Nothing ->
            False


matchClass : Config msg -> Cell -> String
matchClass config cell =
    if cellMatches config cell then
        " nb-cell-match"

    else
        ""



-- NOTEBOOK -------------------------------------------------------------------


{-| Render the whole notebook: every cell, in order — skipping cells inside a collapsed section. -}
notebook : Config msg -> Doc -> Html msg
notebook config doc =
    let
        hidden =
            sectionHidden config doc.cells
    in
    div [ HA.class "nb-cells" ]
        (List.filterMap
            (\c ->
                if Set.member c.id hidden then
                    Nothing

                else
                    Just (cellView config c)
            )
            doc.cells
        )


{-| The ids of cells inside a collapsed section: those after a folded heading, up to the next
heading of the same or higher level. -}
sectionHidden : Config msg -> List Cell -> Set Int
sectionHidden config cells =
    sectionScan config cells 0 Set.empty


sectionScan : Config msg -> List Cell -> Int -> Set Int -> Set Int
sectionScan config cells active acc =
    case cells of
        [] ->
            acc

        c :: rest ->
            let
                lvl =
                    headingLevel c
            in
            if active > 0 && not (lvl > 0 && lvl <= active) then
                sectionScan config rest active (Set.insert c.id acc)

            else
                let
                    nextActive =
                        if lvl > 0 && config.sectionFolded c.id then
                            lvl

                        else
                            0
                in
                sectionScan config rest nextActive acc


{-| The heading level a Markdown cell opens with (1–6), or 0 if it isn't a heading cell. -}
headingLevel : Cell -> Int
headingLevel cell =
    if Cell.isMarkdown cell then
        case List.head (List.filter (\l -> String.startsWith "#" (String.trimLeft l)) (String.lines cell.source)) of
            Just line ->
                countHashes (String.trimLeft line)

            Nothing ->
                0

    else
        0


cellView : Config msg -> Cell -> Html msg
cellView config cell =
    if config.report then
        reportCellView config cell

    else if config.isFolded cell.id then
        foldedCellView config cell

    else
        case cell.kind of
            Code ->
                codeCellView config cell

            Markdown ->
                markdownCellView config cell

            Input ->
                inputCellView config cell


{-| A collapsed cell: a one-line summary; click the chevron to expand. -}
foldedCellView : Config msg -> Cell -> Html msg
foldedCellView config cell =
    div [ HA.id (cellDomId cell.id), HA.class "nb-cell nb-cell-folded" ]
        [ div [ HA.class "nb-gutter" ] [ span [ HA.class "nb-prompt" ] [ text (foldTag cell) ] ]
        , div [ HA.class "nb-body nb-folded-body" ]
            [ button [ HA.class "nb-btn nb-btn-ghost", HA.title "Expand", HE.onClick (config.onFold cell.id) ]
                [ Html.i [ HA.class "bi bi-chevron-right" ] [] ]
            , span [ HA.class "nb-fold-summary" ] [ text (foldSummary cell) ]
            ]
        ]


foldTag : Cell -> String
foldTag cell =
    case cell.kind of
        Markdown ->
            "md"

        Input ->
            "in"

        Code ->
            promptLabel "In" cell.count


foldSummary : Cell -> String
foldSummary cell =
    let
        line =
            firstLine cell.source
    in
    if String.length line > 90 then
        String.left 89 line ++ "…"

    else if String.trim line == "" then
        "(empty " ++ foldTag cell ++ " cell)"

    else
        line


{-| A cell in report mode: prose renders, a code cell shows only its output, an input keeps its
widget — no editors, prompts or toolbars. -}
reportCellView : Config msg -> Cell -> Html msg
reportCellView config cell =
    case cell.kind of
        Markdown ->
            div [ HA.id (cellDomId cell.id), HA.class "nb-cell nb-cell-md nb-cell-report" ]
                [ markdownHtml (interpolate config.evalInline cell.source) ]

        Input ->
            case cell.input of
                Just spec ->
                    div [ HA.class "nb-cell nb-cell-input nb-cell-report" ]
                        [ div [ HA.class "nb-input-row" ]
                            [ span [ HA.class "nb-input-name" ] [ text spec.name ]
                            , span [ HA.class "nb-input-eq" ] [ text "=" ]
                            , controlWidget config cell spec
                            , span [ HA.class "nb-input-val" ] [ text (litText spec) ]
                            ]
                        ]

                Nothing ->
                    text ""

        Code ->
            case cell.output of
                OutNone ->
                    text ""

                OutError message ->
                    div [ HA.class "nb-cell nb-cell-report" ] [ div [ HA.class "nb-error" ] [ text message ] ]

                OutValue value ->
                    div [ HA.class "nb-cell nb-cell-report" ]
                        [ div [ HA.class "nb-value" ] [ renderOutput config cell value ] ]


inputCellView : Config msg -> Cell -> Html msg
inputCellView config cell =
    case cell.input of
        Nothing ->
            codeCellView config cell

        Just spec ->
            div [ HA.class "nb-cell nb-cell-input" ]
                [ div [ HA.class "nb-gutter" ]
                    [ span [ HA.class "nb-prompt nb-prompt-out" ] [ text (promptLabel "In" cell.count) ] ]
                , div [ HA.class "nb-body" ]
                    [ div [ HA.class "nb-input-row" ]
                        [ input
                            [ HA.class "nb-input-name"
                            , HA.value spec.name
                            , HA.attribute "spellcheck" "false"
                            , HE.onInput (config.onInputName cell.id)
                            ]
                            []
                        , span [ HA.class "nb-input-eq" ] [ text "=" ]
                        , controlWidget config cell spec
                        , span [ HA.class "nb-input-val" ] [ text (litText spec) ]
                        , controlSelect config cell spec
                        ]
                    , cellToolbar config cell
                    ]
                ]


controlSelect : Config msg -> Cell -> Cell.InputSpec -> Html msg
controlSelect config cell spec =
    Html.select
        [ HA.class "nb-input-type", HE.onInput (config.onInputControl cell.id) ]
        [ controlOption "slider" "Slider" (isSlider spec.control)
        , controlOption "number" "Number" (spec.control == NumberBox)
        , controlOption "text" "Text" (spec.control == TextBox)
        , controlOption "checkbox" "Checkbox" (spec.control == Checkbox)
        ]


controlOption : String -> String -> Bool -> Html msg
controlOption val lbl selected =
    Html.option [ HA.value val, HA.selected selected ] [ text lbl ]


isSlider : Control -> Bool
isSlider control =
    case control of
        Slider _ _ _ ->
            True

        _ ->
            False


controlWidget : Config msg -> Cell -> Cell.InputSpec -> Html msg
controlWidget config cell spec =
    case spec.control of
        Slider mn mx st ->
            input
                [ HA.type_ "range"
                , HA.class "nb-input-range"
                , HA.value spec.value
                , HA.attribute "min" (String.fromFloat mn)
                , HA.attribute "max" (String.fromFloat mx)
                , HA.attribute "step" (String.fromFloat st)
                , HE.onInput (config.onInputValue cell.id)
                ]
                []

        NumberBox ->
            input
                [ HA.type_ "number", HA.class "nb-input-box", HA.value spec.value, HE.onInput (config.onInputValue cell.id) ]
                []

        TextBox ->
            input
                [ HA.type_ "text", HA.class "nb-input-box", HA.value spec.value, HE.onInput (config.onInputValue cell.id) ]
                []

        Checkbox ->
            button
                [ HA.class ("nb-chip" ++ boolOn spec.value)
                , HE.onClick (config.onInputValue cell.id (toggleBool spec.value))
                ]
                [ text spec.value ]


boolOn : String -> String
boolOn value =
    if value == "True" then
        " nb-chip-on"

    else
        ""


toggleBool : String -> String
toggleBool value =
    if value == "True" then
        "False"

    else
        "True"


litText : Cell.InputSpec -> String
litText spec =
    case spec.control of
        TextBox ->
            "\"" ++ spec.value ++ "\""

        _ ->
            spec.value


codeCellView : Config msg -> Cell -> Html msg
codeCellView config cell =
    let
        stale =
            config.isStale cell.id
    in
    div
        [ HA.class
            ("nb-cell nb-cell-code"
                ++ (if stale then
                        " nb-cell-stale"

                    else
                        ""
                   )
                ++ matchClass config cell
            )
        ]
        [ div [ HA.class "nb-gutter" ]
            [ span [ HA.class "nb-prompt" ] [ text (promptLabel "In" cell.count) ]
            , if stale then
                span [ HA.class "nb-stale-dot", HA.title "Stale — an upstream cell changed; Run to refresh" ] [ text "●" ]

              else
                text ""
            ]
        , div [ HA.class "nb-body" ]
            [ CodeEditor.view
                { source = cell.source
                , caret = config.caretOf cell.id
                , gutter = True
                , highlight = Highlight.segments
                , onChange = config.onEdit cell.id
                , onKey = Just (codeKeys config cell)
                }
            , completionBar config cell
            , cellToolbar config cell
            , outputArea config cell
            ]
        ]


{-| The completion bar shown under the active code cell: the in-scope names that extend the token at
the caret; clicking one inserts it. -}
completionBar : Config msg -> Cell -> Html msg
completionBar config cell =
    if config.activeCell == Just cell.id then
        case Complete.completions cell.source (config.caretOf cell.id) config.scopeNames of
            [] ->
                text ""

            names ->
                div [ HA.class "nb-complete" ]
                    (List.map
                        (\name -> button [ HA.class "nb-complete-item", HE.onClick (applyCompletion config cell name) ] [ text name ])
                        names
                    )

    else
        text ""


applyCompletion : Config msg -> Cell -> String -> msg
applyCompletion config cell name =
    let
        ( source, caret ) =
            Complete.apply cell.source (config.caretOf cell.id) name
    in
    config.onEdit cell.id source caret


markdownCellView : Config msg -> Cell -> Html msg
markdownCellView config cell =
    div [ HA.id (cellDomId cell.id), HA.class ("nb-cell nb-cell-md" ++ matchClass config cell) ]
        [ div [ HA.class "nb-gutter" ] [ span [ HA.class "nb-prompt nb-prompt-md" ] [ text "md" ] ]
        , div [ HA.class "nb-body" ]
            [ markdownHtml (interpolate config.evalInline cell.source)
            , CodeEditor.view
                { source = cell.source
                , caret = config.caretOf cell.id
                , gutter = False
                , highlight = plainSegments
                , onChange = config.onEdit cell.id
                , onKey = Just (moveKeys config cell)
                }
            , cellToolbar config cell
            ]
        ]


{-| Keyboard chords for a code cell: Shift/Ctrl/Cmd+Enter runs it; Tab accepts the top completion;
Alt+↑/↓ moves it. -}
codeKeys : Config msg -> Cell -> CodeEditor.Chord -> Maybe msg
codeKeys config cell chord =
    if chord.key == "Enter" && (chord.shift || chord.ctrl || chord.meta) then
        Just (config.onRun cell.id)

    else if chord.key == "Tab" && not chord.shift && not chord.ctrl && not chord.meta then
        case Complete.completions cell.source (config.caretOf cell.id) config.scopeNames of
            top :: _ ->
                Just (applyCompletion config cell top)

            [] ->
                Nothing

    else
        moveKeys config cell chord


{-| Alt+↑/↓ moves the cell (used by every cell kind). -}
moveKeys : Config msg -> Cell -> CodeEditor.Chord -> Maybe msg
moveKeys config cell chord =
    if chord.alt && chord.key == "ArrowUp" then
        Just (config.onMoveUp cell.id)

    else if chord.alt && chord.key == "ArrowDown" then
        Just (config.onMoveDown cell.id)

    else
        Nothing


plainSegments : String -> List ( String, String )
plainSegments s =
    [ ( "", s ) ]


{-| Substitute every `{{ elm expression }}` in a markdown source with `evalExpr`'s rendering of it,
so prose can quote live notebook values. An expression that doesn't evaluate (`evalExpr` returns
`Nothing`) is left as the literal `{{ … }}`. -}
interpolate : (String -> Maybe String) -> String -> String
interpolate evalExpr source =
    case String.split "{{" source of
        first :: rest ->
            first ++ String.concat (List.map (interpolateChunk evalExpr) rest)

        [] ->
            source


interpolateChunk : (String -> Maybe String) -> String -> String
interpolateChunk evalExpr chunk =
    case String.split "}}" chunk of
        expr :: rest ->
            if List.isEmpty rest then
                "{{" ++ chunk

            else
                (evalExpr (String.trim expr) |> Maybe.withDefault ("{{" ++ expr ++ "}}"))
                    ++ String.join "}}" rest

        [] ->
            chunk


cellToolbar : Config msg -> Cell -> Html msg
cellToolbar config cell =
    let
        convertButton =
            case cell.kind of
                Code ->
                    toolButton "To text" (config.onConvert cell.id Markdown)

                Markdown ->
                    toolButton "To code" (config.onConvert cell.id Code)

                Input ->
                    text ""

        runGroup =
            case cell.kind of
                Code ->
                    [ button [ HA.class "nb-btn nb-btn-run", HE.onClick (config.onRun cell.id) ] [ text "▶ Run" ]
                    , iconBtn "bi-chevron-bar-up" "Run the cells above" (config.onRunAbove cell.id)
                    , iconBtn "bi-chevron-bar-down" "Run from here down" (config.onRunBelow cell.id)
                    ]

                _ ->
                    []
    in
    div [ HA.class "nb-toolbar" ]
        (runGroup
            ++ [ commentMarker config cell
               , div [ HA.class "nb-toolbar-spacer" ] []
               , sectionFoldButton config cell
               , iconBtn "bi-dash-square" "Collapse" (config.onFold cell.id)
               , iconBtn "bi-files" "Duplicate" (config.onDuplicate cell.id)
               , iconBtn "bi-text-indent-left" "Insert cell above" (config.onInsertAbove cell.id)
               , iconBtn "bi-text-indent-right" "Insert cell below" (config.onInsertBelow cell.id)
               , toolButton "↑" (config.onMoveUp cell.id)
               , toolButton "↓" (config.onMoveDown cell.id)
               , convertButton
               , toolButton "✕" (config.onDelete cell.id)
               ]
        )


{-| For a heading (Markdown) cell, a chevron that folds/unfolds the whole section beneath it. -}
sectionFoldButton : Config msg -> Cell -> Html msg
sectionFoldButton config cell =
    if headingLevel cell > 0 then
        if config.sectionFolded cell.id then
            iconBtn "bi-chevron-right" "Expand section" (config.onFoldSection cell.id)

        else
            iconBtn "bi-chevron-down" "Collapse section" (config.onFoldSection cell.id)

    else
        text ""


commentMarker : Config msg -> Cell -> Html msg
commentMarker config cell =
    let
        n =
            config.commentCountOf cell.id
    in
    if config.commentsVisible && n > 0 then
        span [ HA.class "nb-comment-marker", HA.title "This cell has comments" ]
            [ Html.i [ HA.class "bi bi-chat-dots" ] [], text (" " ++ String.fromInt n) ]

    else
        text ""


toolButton : String -> msg -> Html msg
toolButton label msg =
    button [ HA.class "nb-btn nb-btn-ghost", HE.onClick msg ] [ text label ]


iconBtn : String -> String -> msg -> Html msg
iconBtn icon title msg =
    button [ HA.class "nb-btn nb-btn-ghost nb-btn-icon", HA.title title, HE.onClick msg ] [ Html.i [ HA.class ("bi " ++ icon) ] [] ]


outputArea : Config msg -> Cell -> Html msg
outputArea config cell =
    case cell.output of
        OutNone ->
            text ""

        OutError message ->
            div [ HA.class "nb-out nb-out-error" ]
                [ span [ HA.class "nb-prompt nb-prompt-err" ] [ text (promptLabel "Out" cell.count) ]
                , div [ HA.class "nb-error" ]
                    [ text message
                    , case config.errorFix cell of
                        Just fix ->
                            button
                                [ HA.class "nb-fix"
                                , HA.title "Replace it and re-run"
                                , HE.onClick (config.onFix cell.id fix.fixed)
                                ]
                                [ Html.i [ HA.class "bi bi-magic" ] [], text (" " ++ fix.label) ]

                        Nothing ->
                            text ""
                    ]
                ]

        OutValue value ->
            div [ HA.class "nb-out" ]
                [ span [ HA.class "nb-prompt nb-prompt-out" ] [ text (promptLabel "Out" cell.count) ]
                , div [ HA.class "nb-value" ]
                    [ div [ HA.class "nb-out-tools" ]
                        [ chartToggle config cell value
                        , config.exportCell cell
                        ]
                    , renderOutput config cell value
                    ]
                ]


renderOutput : Config msg -> Cell -> Value -> Html msg
renderOutput config cell value =
    if Value.isTable value then
        case config.pivotOf cell.id of
            Just spec ->
                pivotView config cell value spec

            Nothing ->
                case config.groupOf cell.id of
                    Just gspec ->
                        groupView config cell value gspec

                    Nothing ->
                        if config.isCorr cell.id then
                            correlationView value

                        else if config.isProfile cell.id then
                            profilePanel value

                        else
                            chartOrTable config cell value

    else
        chartOrTable config cell value


chartOrTable : Config msg -> Cell -> Value -> Html msg
chartOrTable config cell value =
    case config.chartOf cell.id of
        Just kind ->
            if Chart.chartable value then
                div [ HA.class "nb-chart-box" ] [ Chart.view kind (config.colOf cell.id) value ]

            else
                valueHtml value

        Nothing ->
            if Value.isTable value then
                interactiveTable config cell value

            else
                valueHtml value


{-| A pivot of the table: field pickers plus the cross-tab grid. -}
pivotView : Config msg -> Cell -> Value -> Pivot.Spec -> Html msg
pivotView config cell value spec =
    let
        cols =
            Value.tableColumns value

        grid =
            Pivot.pivot spec value

        pick labelText options selected onPick =
            Html.label [ HA.class "nb-pivot-field" ]
                [ span [ HA.class "nb-pivot-label" ] [ text labelText ]
                , Html.select [ HA.class "nb-chart-col", HE.onInput onPick ]
                    (List.map (\o -> Html.option [ HA.value o, HA.selected (o == selected) ] [ text o ]) options)
                ]
    in
    div [ HA.class "nb-pivot" ]
        [ div [ HA.class "nb-pivot-controls" ]
            [ pick "Rows" cols spec.row (config.onPivotRow cell.id)
            , pick "Columns" cols spec.column (config.onPivotColumn cell.id)
            , pick "Value" cols spec.value (config.onPivotValue cell.id)
            , pick "" (List.map Pivot.aggLabel Pivot.aggs) (Pivot.aggLabel spec.agg) (config.onPivotAgg cell.id)
            ]
        , wrapTable
            [ thead []
                [ tr []
                    (th [ HA.class "nb-pivot-corner" ] [ text (spec.row ++ " ╲ " ++ spec.column) ]
                        :: List.map (\c -> th [] [ text c ]) grid.columns
                    )
                ]
            , tbody []
                (List.map
                    (\r ->
                        tr []
                            (th [ HA.class "nb-pivot-rowhead" ] [ text r.label ]
                                :: List.map (\c -> td [ HA.class "nb-prof-n" ] [ text c ]) r.cells
                            )
                    )
                    grid.rows
                )
            ]
        ]


{-| A one-dimensional group-by of the table: key / value / aggregation pickers plus the grouped
summary grid (one row per distinct key, with its count and the aggregate). -}
groupView : Config msg -> Cell -> Value -> GroupBy.Spec -> Html msg
groupView config cell value spec =
    let
        cols =
            Value.tableColumns value

        grid =
            GroupBy.group spec value

        pick labelText options selected onPick =
            Html.label [ HA.class "nb-pivot-field" ]
                [ span [ HA.class "nb-pivot-label" ] [ text labelText ]
                , Html.select [ HA.class "nb-chart-col", HE.onInput onPick ]
                    (List.map (\o -> Html.option [ HA.value o, HA.selected (o == selected) ] [ text o ]) options)
                ]
    in
    div [ HA.class "nb-pivot" ]
        [ div [ HA.class "nb-pivot-controls" ]
            [ pick "Group by" cols spec.key (config.onGroupKey cell.id)
            , pick "Value" cols spec.value (config.onGroupValue cell.id)
            , pick "" (List.map Pivot.aggLabel Pivot.aggs) (Pivot.aggLabel spec.agg) (config.onGroupAgg cell.id)
            ]
        , wrapTable
            [ thead [] [ tr [] (List.map (\h -> th [] [ text h ]) grid.columns) ]
            , tbody []
                (List.map
                    (\r -> tr [] (List.map (\cellTxt -> td [ HA.class "nb-prof-n" ] [ text cellTxt ]) r))
                    grid.rows
                )
            ]
        ]


{-| The correlation matrix of the table's numeric columns, colour-graded blue (positive) to red. -}
correlationView : Value -> Html msg
correlationView value =
    let
        m =
            Correlation.matrix value
    in
    if List.isEmpty m.columns then
        p [ HA.class "nb-vars-empty" ] [ text "Needs a table with numeric columns." ]

    else
        wrapTable
            [ thead []
                [ tr [] (th [] [] :: List.map (\c -> th [] [ text c ]) m.columns) ]
            , tbody []
                (List.map2
                    (\name row -> tr [] (th [ HA.class "nb-pivot-rowhead" ] [ text name ] :: List.map corrCell row))
                    m.columns
                    m.rows
                )
            ]


corrCell : Maybe Float -> Html msg
corrCell maybeR =
    case maybeR of
        Just r ->
            td [ HA.class "nb-corr-cell", HA.style "background" (corrColor r) ] [ text (round2 r) ]

        Nothing ->
            td [ HA.class "nb-corr-cell" ] [ text "—" ]


corrColor : Float -> String
corrColor r =
    let
        alpha =
            String.fromFloat (0.1 + 0.75 * abs r)
    in
    if r >= 0 then
        "rgba(91, 110, 245, " ++ alpha ++ ")"

    else
        "rgba(217, 48, 37, " ++ alpha ++ ")"


round2 : Float -> String
round2 r =
    Value.numberToString (toFloat (round (r * 100)) / 100)


{-| A per-column overview of a table: type, count, distinct, and numeric min / max / mean. -}
profilePanel : Value -> Html msg
profilePanel value =
    let
        num maybe =
            maybe |> Maybe.map Value.numberToString |> Maybe.withDefault "—"

        spark col =
            case columnNums col.name (itemsOf value) of
                [] ->
                    text ""

                nums ->
                    Sparkline.svg nums

        row col =
            tr []
                [ td [ HA.class "nb-prof-name" ] [ text col.name ]
                , td [] [ span [ HA.class "nb-prof-kind" ] [ text col.kind ] ]
                , td [ HA.class "nb-prof-n" ] [ text (String.fromInt col.count) ]
                , td [ HA.class "nb-prof-n" ] [ text (String.fromInt col.distinct) ]
                , td [ HA.class "nb-prof-n" ] [ text (num col.min) ]
                , td [ HA.class "nb-prof-n" ] [ text (num col.max) ]
                , td [ HA.class "nb-prof-n" ] [ text (num col.mean) ]
                , td [ HA.class "nb-prof-spark" ] [ spark col ]
                ]
    in
    div [ HA.class "nb-table-wrap" ]
        [ table [ HA.class "nb-table nb-profile" ]
            [ thead []
                [ tr []
                    (List.map (\h -> th [] [ text h ])
                        [ "column", "type", "count", "distinct", "min", "max", "mean", "trend" ]
                    )
                ]
            , tbody [] (List.map row (Profile.columns value))
            ]
        ]


{-| The top-level table output, with sortable column headers, a row filter and a row cap — the
notebook's interactive data grid. Nested tables (inside a cell) stay static. -}
interactiveTable : Config msg -> Cell -> Value -> Html msg
interactiveTable config cell value =
    let
        cols =
            Value.tableColumns value

        filter =
            config.tableFilter cell.id

        clauses =
            config.colFiltersOf cell.id

        textFiltered =
            if String.trim filter == "" then
                itemsOf value

            else
                List.filter (rowMatches (String.toLower filter)) (itemsOf value)

        filtered =
            Filter.apply clauses textFiltered

        sorted =
            case config.tableSort cell.id of
                Just ( col, desc ) ->
                    sortRows col desc filtered

                Nothing ->
                    filtered

        total =
            List.length sorted

        expanded =
            config.tableExpanded cell.id

        shown =
            if expanded then
                sorted

            else
                List.take tableCap sorted

        sortMark col =
            case config.tableSort cell.id of
                Just ( c, desc ) ->
                    if c == col then
                        if desc then
                            " ▾"

                        else
                            " ▴"

                    else
                        ""

                _ ->
                    ""

        hidden =
            config.hiddenCols cell.id

        fmt =
            config.numFormat cell.id

        visibleCols =
            List.filter (\c -> not (Set.member c hidden)) cols

        numCols =
            List.filter (\c -> not (Set.member c hidden)) (Chart.numericColumns value)

        header =
            thead []
                [ tr []
                    (List.map
                        (\c -> th [ HA.class "nb-th-sort", HA.title "Sort by this column", HE.onClick (config.onSort cell.id c) ] [ text (c ++ sortMark c) ])
                        visibleCols
                    )
                ]

        heat =
            config.heatOn cell.id

        bars =
            config.barsOn cell.id

        foot =
            config.footerOn cell.id

        ranges =
            if heat || bars then
                List.filterMap
                    (\c -> Heatmap.range (columnNums c sorted) |> Maybe.map (\rg -> ( c, rg )))
                    numCols

            else
                []

        footRows =
            if foot then
                [ summaryFooter fmt numCols visibleCols sorted ]

            else
                []
    in
    div [ HA.class "nb-table-x" ]
        [ div [ HA.class "nb-table-controls" ]
            [ input
                [ HA.class "nb-table-filter"
                , HA.placeholder "Filter rows…"
                , HA.value filter
                , HA.attribute "spellcheck" "false"
                , HE.onInput (config.onFilter cell.id)
                ]
                []
            , span [ HA.class "nb-table-count" ] [ text (countLabel (List.length shown) total) ]
            , if List.isEmpty numCols then
                text ""

              else
                tableChip "Heat" heat (config.onHeat cell.id (not heat))
            , if List.isEmpty numCols then
                text ""

              else
                tableChip "Bars" bars (config.onBars cell.id (not bars))
            , if List.isEmpty numCols then
                text ""

              else
                tableChip "Σ Summary" foot (config.onFooter cell.id (not foot))
            , if List.isEmpty numCols then
                text ""

              else
                tableChip (Format.label fmt) (fmt /= Format.Auto) (config.onNumFormat cell.id)
            ]
        , columnToggles config cell cols hidden
        , filterBuilder config cell cols clauses
        , wrapTable ([ header, tbody [] (List.map (dataRow fmt heat bars ranges visibleCols) shown) ] ++ footRows)
        , if total > tableCap then
            button [ HA.class "nb-table-more", HE.onClick (config.onExpand cell.id (not expanded)) ]
                [ text
                    (if expanded then
                        "Show fewer"

                     else
                        "Show all " ++ String.fromInt total ++ " rows"
                    )
                ]

          else
            text ""
        ]


tableCap : Int
tableCap =
    20


{-| A small on/off chip in the table controls (heat, summary). -}
tableChip : String -> Bool -> msg -> Html msg
tableChip lbl active msg =
    button
        [ HA.class
            ("nb-chip nb-table-chip"
                ++ (if active then
                        " nb-chip-on"

                    else
                        ""
                   )
            )
        , HE.onClick msg
        ]
        [ text lbl ]


{-| A row of chips, one per column, to hide/show it — a hidden column reads struck-through. Shown
only when a table has more than one column. -}
columnToggles : Config msg -> Cell -> List String -> Set String -> Html msg
columnToggles config cell cols hidden =
    if List.length cols <= 1 then
        text ""

    else
        div [ HA.class "nb-col-toggles" ]
            (span [ HA.class "nb-col-label" ] [ text "columns:" ]
                :: List.map
                    (\c ->
                        button
                            [ HA.class
                                ("nb-col-chip"
                                    ++ (if Set.member c hidden then
                                            " nb-col-off"

                                        else
                                            ""
                                       )
                                )
                            , HA.title
                                (if Set.member c hidden then
                                    "Show this column"

                                 else
                                    "Hide this column"
                                )
                            , HE.onClick (config.onToggleCol cell.id c)
                            ]
                            [ text c ]
                    )
                    cols
            )


{-| The column-filter builder: one row per clause (column · operator · value · remove) plus an
"+ filter" button. Clauses AND together and combine with the text filter. -}
filterBuilder : Config msg -> Cell -> List String -> List Filter.Clause -> Html msg
filterBuilder config cell cols clauses =
    div [ HA.class "nb-filters" ]
        (List.indexedMap (filterClause config cell cols) clauses
            ++ [ button [ HA.class "nb-chip nb-table-chip", HE.onClick (config.onAddFilter cell.id) ]
                    [ Html.i [ HA.class "bi bi-funnel" ] [], text " filter" ]
               ]
        )


filterClause : Config msg -> Cell -> List String -> Int -> Filter.Clause -> Html msg
filterClause config cell cols i clause =
    div [ HA.class "nb-filter-row" ]
        [ Html.select [ HA.class "nb-chart-col", HE.onInput (config.onFilterCol cell.id i) ]
            (Html.option [ HA.value "", HA.selected (clause.col == "") ] [ text "column…" ]
                :: List.map (\c -> Html.option [ HA.value c, HA.selected (c == clause.col) ] [ text c ]) cols
            )
        , Html.select [ HA.class "nb-chart-col", HE.onInput (config.onFilterOp cell.id i) ]
            (List.map (\o -> Html.option [ HA.value (Filter.opLabel o), HA.selected (o == clause.op) ] [ text (Filter.opLabel o) ]) Filter.ops)
        , input
            [ HA.class "nb-filter-value", HA.placeholder "value", HA.value clause.value, HA.attribute "spellcheck" "false", HE.onInput (config.onFilterValue cell.id i) ]
            []
        , button [ HA.class "nb-action nb-action-icon", HA.title "Remove filter", HE.onClick (config.onRemoveFilter cell.id i) ]
            [ Html.i [ HA.class "bi bi-x" ] [] ]
        ]


{-| A data row whose numeric cells are formatted (per the table's number format), heat-shaded
(when `heat` is on) or drawn as in-cell data bars (when `bars` is on), by their column's range. -}
dataRow : Format.Format -> Bool -> Bool -> List ( String, ( Float, Float ) ) -> List String -> Value -> Html msg
dataRow fmt heat bars ranges cols row =
    tr [] (List.map (heatCell fmt heat bars ranges row) cols)


heatCell : Format.Format -> Bool -> Bool -> List ( String, ( Float, Float ) ) -> Value -> String -> Html msg
heatCell fmt heat bars ranges row col =
    case Value.fieldOf col row of
        Just (VNum n) ->
            td (numCellAttrs heat bars ranges col n) [ text (Format.format fmt n) ]

        maybeOther ->
            td [] [ cellHtml maybeOther ]


numCellAttrs : Bool -> Bool -> List ( String, ( Float, Float ) ) -> String -> Float -> List (Html.Attribute msg)
numCellAttrs heat bars ranges col n =
    case lookupRange col ranges of
        Just rg ->
            if bars then
                [ HA.class "nb-num-cell", HA.style "background" (dataBarBg rg n) ]

            else if heat then
                [ HA.class "nb-num-cell", HA.style "background" (Heatmap.color rg n) ]

            else
                [ HA.class "nb-num-cell" ]

        Nothing ->
            [ HA.class "nb-num-cell" ]


{-| A left-anchored data bar filling the cell to the value's position in its column's range. -}
dataBarBg : ( Float, Float ) -> Float -> String
dataBarBg ( lo, hi ) v =
    let
        pct =
            if hi == lo then
                100

            else
                Basics.max 0 (Basics.min 100 ((v - lo) / (hi - lo) * 100))

        edge =
            String.fromFloat pct ++ "%"
    in
    "linear-gradient(90deg, rgba(91, 110, 245, 0.22) " ++ edge ++ ", transparent " ++ edge ++ ")"


lookupRange : String -> List ( String, ( Float, Float ) ) -> Maybe ( Float, Float )
lookupRange col ranges =
    case ranges of
        ( c, rg ) :: rest ->
            if c == col then
                Just rg

            else
                lookupRange col rest

        [] ->
            Nothing


{-| The numeric values of a column across the given rows. -}
columnNums : String -> List Value -> List Float
columnNums col rows =
    List.filterMap
        (\row ->
            case Value.fieldOf col row of
                Just (VNum n) ->
                    Just n

                _ ->
                    Nothing
        )
        rows


{-| A two-line table footer: the sum (Σ) and mean (x̄) of every numeric column, in the table's format. -}
summaryFooter : Format.Format -> List String -> List String -> List Value -> Html msg
summaryFooter fmt numCols cols rows =
    Html.tfoot []
        [ footerRow fmt "Σ" List.sum numCols cols rows
        , footerRow fmt "x̄" meanOf numCols cols rows
        ]


footerRow : Format.Format -> String -> (List Float -> Float) -> List String -> List String -> List Value -> Html msg
footerRow fmt lbl agg numCols cols rows =
    let
        firstNumeric =
            List.head numCols

        cellFor col =
            if List.member col numCols then
                let
                    nums =
                        columnNums col rows

                    shown =
                        if firstNumeric == Just col then
                            lbl ++ " " ++ Format.format fmt (agg nums)

                        else
                            Format.format fmt (agg nums)
                in
                td [ HA.class "nb-prof-n nb-tfoot-n" ] [ text shown ]

            else
                td [ HA.class "nb-tfoot-n" ] []
    in
    tr [ HA.class "nb-tfoot" ] (List.map cellFor cols)


meanOf : List Float -> Float
meanOf xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)


rowMatches : String -> Value -> Bool
rowMatches needle row =
    case row of
        VRecord fields ->
            List.any (\( _, v ) -> String.contains needle (String.toLower (Value.inlineValue v))) fields

        _ ->
            False


sortRows : String -> Bool -> List Value -> List Value
sortRows col desc rows =
    let
        ascending =
            List.sortWith (compareField col) rows
    in
    if desc then
        List.reverse ascending

    else
        ascending


compareField : String -> Value -> Value -> Order
compareField col a b =
    case ( Value.fieldOf col a, Value.fieldOf col b ) of
        ( Just (VNum x), Just (VNum y) ) ->
            compare x y

        ( Just va, Just vb ) ->
            compare (Value.inlineValue va) (Value.inlineValue vb)

        ( Just _, Nothing ) ->
            GT

        ( Nothing, Just _ ) ->
            LT

        ( Nothing, Nothing ) ->
            EQ


countLabel : Int -> Int -> String
countLabel shown total =
    if shown == total then
        String.fromInt total ++ " rows"

    else
        "showing " ++ String.fromInt shown ++ " of " ++ String.fromInt total


chartToggle : Config msg -> Cell -> Value -> Html msg
chartToggle config cell value =
    if Chart.chartable value || Value.isTable value then
        let
            profileOn =
                config.isProfile cell.id

            pivotOn =
                config.pivotOf cell.id /= Nothing

            corrOn =
                config.isCorr cell.id

            groupOn =
                config.groupOf cell.id /= Nothing

            special =
                profileOn || pivotOn || corrOn || groupOn

            current =
                if special then
                    Nothing

                else
                    config.chartOf cell.id

            chip lbl active msg =
                button
                    [ HA.class
                        ("nb-chip"
                            ++ (if active then
                                    " nb-chip-on"

                                else
                                    ""
                               )
                        )
                    , HE.onClick msg
                    ]
                    [ text lbl ]

            kindChips =
                List.map
                    (\k -> chip (Chart.label k) (current == Just k) (config.onChart cell.id (Just k)))
                    (Chart.chartableKinds value)

            tableChips =
                if Value.isTable value then
                    [ chip "Profile" profileOn (config.onProfile cell.id (not profileOn))
                    , chip "Group" groupOn (config.onGroup cell.id (not groupOn))
                    , chip "Pivot" pivotOn (config.onPivot cell.id (not pivotOn))
                    , chip "Corr" corrOn (config.onCorr cell.id (not corrOn))
                    ]

                else
                    []
        in
        div [ HA.class "nb-chart-toggle" ]
            (chip "Table" (current == Nothing && not special) (config.onChart cell.id Nothing)
                :: kindChips
                ++ tableChips
                ++ [ columnPicker config cell value current ]
            )

    else
        text ""


{-| A picker for which numeric column a chart plots — shown only when a chart is active on a table
that has more than one numeric column. -}
columnPicker : Config msg -> Cell -> Value -> Maybe Chart.ChartKind -> Html msg
columnPicker config cell value current =
    let
        cols =
            Chart.numericColumns value
    in
    case current of
        Just kind ->
            if List.length cols >= 2 && kind /= Chart.MultiLine then
                let
                    selected =
                        config.colOf cell.id |> Maybe.withDefault (Maybe.withDefault "" (Chart.defaultColumn value))
                in
                Html.select [ HA.class "nb-chart-col", HE.onInput (config.onCol cell.id) ]
                    (List.map (\c -> Html.option [ HA.value c, HA.selected (c == selected) ] [ text c ]) cols)

            else
                text ""

        Nothing ->
            text ""


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

        Input ->
            "input"



-- VARIABLES INSPECTOR --------------------------------------------------------


{-| The DOM id of a cell's element, so the outline can link to it. -}
cellDomId : Int -> String
cellDomId id =
    "nb-cell-" ++ String.fromInt id


{-| A compact "Notebook" summary card: cell / variable / error counts and a rough reading time. -}
overviewPanel : Doc -> Html msg
overviewPanel doc =
    let
        s =
            Overview.of_ doc
    in
    div [ HA.class "nb-overview" ]
        [ h3 [ HA.class "nb-overview-title" ] [ text "Notebook" ]
        , overviewRow "Cells" (String.fromInt s.cells)
        , overviewRow "Code" (String.fromInt s.code)
        , overviewRow "Text" (String.fromInt s.text)
        , overviewRow "Variables" (String.fromInt s.variables)
        , overviewRow "Errors" (String.fromInt s.errors)
        , overviewRow "Words" (String.fromInt s.words)
        , overviewRow "Read" (String.fromInt s.readMins ++ " min")
        ]


overviewRow : String -> String -> Html msg
overviewRow label value =
    div [ HA.class "nb-overview-row" ]
        [ span [ HA.class "nb-overview-label" ] [ text label ]
        , span [ HA.class "nb-overview-val" ] [ text value ]
        ]


{-| The outline: the notebook's Markdown headings as jump links, indented by level. Hidden when
there are none. -}
outlinePanel : List Heading -> Html msg
outlinePanel hs =
    if List.isEmpty hs then
        text ""

    else
        div [ HA.class "nb-outline" ]
            (h3 [ HA.class "nb-outline-title" ] [ text "Outline" ]
                :: List.map outlineLink hs
            )


outlineLink : Heading -> Html msg
outlineLink heading =
    a
        [ HA.class ("nb-outline-link nb-outline-l" ++ String.fromInt heading.level)
        , HA.href ("#" ++ cellDomId heading.cellId)
        ]
        [ text heading.text ]


{-| A panel listing the user's kernel bindings (name · type · value preview). Clicking one inserts
a cell that references it.
-}
variablesPanel : (String -> msg) -> List ( String, Value ) -> Html msg
variablesPanel onPick vars =
    div [ HA.class "nb-vars" ]
        [ h3 [ HA.class "nb-vars-title" ] [ text "Variables" ]
        , if List.isEmpty vars then
            p [ HA.class "nb-vars-empty" ] [ text "Names you define with = appear here." ]

          else
            div [ HA.class "nb-vars-list" ] (List.map (variableRow onPick) vars)
        ]


variableRow : (String -> msg) -> ( String, Value ) -> Html msg
variableRow onPick ( name, value ) =
    button [ HA.class "nb-var", HA.title ("Insert a cell for " ++ name), HE.onClick (onPick name) ]
        [ span [ HA.class "nb-var-name" ] [ text name ]
        , span [ HA.class "nb-var-type" ] [ text (Value.typeName value) ]
        , span [ HA.class "nb-var-val" ] [ text (preview value) ]
        ]


{-| A panel summarising the cells currently in error (their `In [n]` and the first line of the
message). Clicking one re-runs that cell. Hidden when there are no errors. -}
errorsPanel : (Int -> msg) -> List ( Int, Int, String ) -> Html msg
errorsPanel onRun errors =
    if List.isEmpty errors then
        text ""

    else
        div [ HA.class "nb-errors" ]
            (h3 [ HA.class "nb-errors-title" ]
                [ Html.i [ HA.class "bi bi-exclamation-triangle" ] []
                , text (" " ++ String.fromInt (List.length errors) ++ " in error")
                ]
                :: List.map (errorRow onRun) errors
            )


errorRow : (Int -> msg) -> ( Int, Int, String ) -> Html msg
errorRow onRun ( id, count, message ) =
    button [ HA.class "nb-error-row", HA.title "Re-run this cell", HE.onClick (onRun id) ]
        [ span [ HA.class "nb-error-prompt" ] [ text (promptLabel "In" (Just count)) ]
        , span [ HA.class "nb-error-msg" ] [ text (firstLine message) ]
        ]


preview : Value -> String
preview value =
    let
        s =
            Value.inlineValue value
    in
    if String.length s > 30 then
        String.left 29 s ++ "…"

    else
        s


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
        (blocksToHtml (groupBlocks (classifyAll (String.lines source)) []))


{-| Classify every line, but first lift fenced ```` ``` ```` code spans into a single `LCode` line. -}
classifyAll : List String -> List Line
classifyAll raws =
    case raws of
        [] ->
            []

        r :: rest ->
            if isFence r then
                let
                    ( codeLines, after ) =
                        takeUntilFence rest []
                in
                LCode (fenceLang r) (String.join "\n" codeLines) :: classifyAll after

            else
                classifyLine r :: classifyAll rest


isFence : String -> Bool
isFence line =
    String.startsWith "```" (String.trimLeft line)


fenceLang : String -> String
fenceLang line =
    String.trim (String.dropLeft 3 (String.trimLeft line))


takeUntilFence : List String -> List String -> ( List String, List String )
takeUntilFence raws acc =
    case raws of
        [] ->
            ( List.reverse acc, [] )

        r :: rest ->
            if isFence r then
                ( List.reverse acc, rest )

            else
                takeUntilFence rest (r :: acc)


type Line
    = LHead Int String
    | LItem Int String
    | LQuote String
    | LTableRow String
    | LCode String String
    | LHr
    | LText String
    | LBlank


type Block
    = BHead Int String
    | BPara (List String)
    | BList (List ( Int, String ))
    | BQuote (List String)
    | BTable (List String)
    | BCode String String
    | BHr


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

    else if String.startsWith ">" trimmedLeft then
        LQuote (String.trimLeft (String.dropLeft 1 trimmedLeft))

    else if String.startsWith "|" trimmedLeft then
        LTableRow trimmedLeft

    else if isHrLine trimmedLeft then
        LHr

    else if String.trim line == "" then
        LBlank

    else
        LText line


{-| A horizontal rule: a line of three or more `-`, `*` or `_` (and nothing else). -}
isHrLine : String -> Bool
isHrLine s =
    let
        t =
            String.trim s
    in
    String.length t >= 3 && (String.all (\c -> c == '-') t || String.all (\c -> c == '*') t || String.all (\c -> c == '_') t)


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

        (LCode lang code) :: rest ->
            flushPending pending ++ (BCode lang code :: groupBlocks rest [])

        LHr :: rest ->
            flushPending pending ++ (BHr :: groupBlocks rest [])

        LBlank :: rest ->
            flushPending pending ++ groupBlocks rest []

        ((LItem _ _) as item) :: rest ->
            if isItemBuffer pending then
                groupBlocks rest (pending ++ [ item ])

            else
                flushPending pending ++ groupBlocks rest [ item ]

        ((LQuote _) as q) :: rest ->
            if isQuoteBuffer pending then
                groupBlocks rest (pending ++ [ q ])

            else
                flushPending pending ++ groupBlocks rest [ q ]

        ((LTableRow _) as tr_) :: rest ->
            if isTableBuffer pending then
                groupBlocks rest (pending ++ [ tr_ ])

            else
                flushPending pending ++ groupBlocks rest [ tr_ ]

        ((LText _) as txt) :: rest ->
            if isTextBuffer pending then
                groupBlocks rest (pending ++ [ txt ])

            else
                flushPending pending ++ groupBlocks rest [ txt ]


isQuoteBuffer : List Line -> Bool
isQuoteBuffer pending =
    case pending of
        (LQuote _) :: _ ->
            True

        _ ->
            False


isTableBuffer : List Line -> Bool
isTableBuffer pending =
    case pending of
        (LTableRow _) :: _ ->
            True

        _ ->
            False


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

        (LQuote _) :: _ ->
            [ BQuote (List.filterMap quoteText pending) ]

        (LTableRow _) :: _ ->
            [ BTable (List.filterMap tableText pending) ]

        _ ->
            [ BPara (List.filterMap textText pending) ]


itemPair : Line -> Maybe ( Int, String )
itemPair line =
    case line of
        LItem indent s ->
            Just ( indent, s )

        _ ->
            Nothing


quoteText : Line -> Maybe String
quoteText line =
    case line of
        LQuote s ->
            Just s

        _ ->
            Nothing


tableText : Line -> Maybe String
tableText line =
    case line of
        LTableRow s ->
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
            ul [] (nestItems items)

        BQuote lines ->
            calloutHtml lines

        BTable lines ->
            tableBlock lines

        BCode lang code ->
            codeBlock lang code

        BHr ->
            Html.hr [ HA.class "nb-md-hr" ] []


{-| A fenced code block — highlighted with the editor's Elm highlighter for `elm` (or no language),
else shown verbatim. -}
codeBlock : String -> String -> Html msg
codeBlock lang code =
    if lang == "" || lang == "elm" then
        Html.pre [ HA.class "nb-md-code" ]
            [ Html.code [] (List.map highlightSeg (Highlight.segments code)) ]

    else
        Html.pre [ HA.class "nb-md-code" ] [ Html.code [] [ text code ] ]


highlightSeg : ( String, String ) -> Html msg
highlightSeg ( cls, txt ) =
    span [ HA.class ("ce-" ++ cls) ] [ text txt ]


{-| A Markdown pipe table: the first row is the header, a `|---|` separator row is skipped, the rest
are body rows. Cells are `|`-delimited and rendered with inline Markdown. -}
tableBlock : List String -> Html msg
tableBlock lines =
    case lines of
        header :: rest ->
            table [ HA.class "nb-table nb-md-table" ]
                [ thead [] [ tr [] (List.map (\c -> th [] (inline c)) (rowCells header)) ]
                , tbody []
                    (List.filterMap
                        (\l ->
                            if isSeparatorRow l then
                                Nothing

                            else
                                Just (tr [] (List.map (\c -> td [] (inline c)) (rowCells l)))
                        )
                        rest
                    )
                ]

        [] ->
            text ""


{-| The cells of a `| a | b |` row: split on `|`, trim, and drop the empty ends. -}
rowCells : String -> List String
rowCells line =
    String.split "|" line |> List.map String.trim |> dropEmptyEnds


dropEmptyEnds : List String -> List String
dropEmptyEnds xs =
    let
        noLead =
            case xs of
                "" :: rest ->
                    rest

                _ ->
                    xs
    in
    case List.reverse noLead of
        "" :: rest ->
            List.reverse rest

        _ ->
            noLead


isSeparatorRow : String -> Bool
isSeparatorRow line =
    let
        body =
            String.filter (\c -> c /= '|' && c /= ' ') line
    in
    body /= "" && String.all (\c -> c == '-' || c == ':') body


{-| A blockquote, or — when its first line is a `[!note]` / `[!tip]` / `[!warning]` /
`[!important]` / `[!caution]` marker (GitHub-style) — a titled, coloured callout box. -}
calloutHtml : List String -> Html msg
calloutHtml lines =
    case lines of
        first :: rest ->
            case calloutKind first of
                Just ( kind, title ) ->
                    div [ HA.class ("nb-callout nb-callout-" ++ kind) ]
                        [ div [ HA.class "nb-callout-title" ] [ text title ]
                        , div [ HA.class "nb-callout-body" ] [ p [] (inline (String.join " " rest)) ]
                        ]

                Nothing ->
                    Html.blockquote [ HA.class "nb-quote" ] [ p [] (inline (String.join " " lines)) ]

        [] ->
            text ""


{-| If a callout's first line is a `[!kind] optional title` marker, the kind and a display title. -}
calloutKind : String -> Maybe ( String, String )
calloutKind line =
    let
        trimmed =
            String.trim line
    in
    if String.startsWith "[!" trimmed then
        case readUntil [ ']' ] (String.toList (String.dropLeft 2 trimmed)) [] of
            Just ( tag, after ) ->
                let
                    kind =
                        String.toLower (String.trim tag)

                    custom =
                        String.trim (String.fromList after)

                    title =
                        if custom == "" then
                            defaultCalloutTitle kind

                        else
                            custom
                in
                if List.member kind [ "note", "tip", "warning", "important", "caution" ] then
                    Just ( kind, title )

                else
                    Nothing

            Nothing ->
                Nothing

    else
        Nothing


defaultCalloutTitle : String -> String
defaultCalloutTitle kind =
    case kind of
        "tip" ->
            "Tip"

        "warning" ->
            "Warning"

        "important" ->
            "Important"

        "caution" ->
            "Caution"

        _ ->
            "Note"


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
            li (itemAttrs body) (itemContent body ++ childHtml) :: nestItems remaining


{-| A list item's body: a `[ ]` / `[x]` prefix becomes a (read-only) task checkbox, else inline Markdown. -}
itemContent : String -> List (Html msg)
itemContent body =
    if String.startsWith "[ ] " body then
        taskCheckbox False (String.dropLeft 4 body)

    else if String.startsWith "[x] " body || String.startsWith "[X] " body then
        taskCheckbox True (String.dropLeft 4 body)

    else
        inline body


itemAttrs : String -> List (Html.Attribute msg)
itemAttrs body =
    if String.startsWith "[ ] " body || String.startsWith "[x] " body || String.startsWith "[X] " body then
        [ HA.class "nb-task-item" ]

    else
        []


taskCheckbox : Bool -> String -> List (Html msg)
taskCheckbox done rest =
    input [ HA.type_ "checkbox", HA.checked done, HA.disabled True, HA.class "nb-task" ] []
        :: inline rest


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

        '~' :: '~' :: rest ->
            case readUntil [ '~', '~' ] rest [] of
                Just ( inner, after ) ->
                    inlineScan after [] (Html.del [] [ text inner ] :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('~' :: '~' :: textRev) nodes

        '=' :: '=' :: rest ->
            case readUntil [ '=', '=' ] rest [] of
                Just ( inner, after ) ->
                    inlineScan after [] (Html.mark [] [ text inner ] :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('=' :: '=' :: textRev) nodes

        '$' :: rest ->
            case readUntil [ '$' ] rest [] of
                Just ( inner, after ) ->
                    inlineScan after [] (Math.inline inner :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('$' :: textRev) nodes

        '!' :: '[' :: rest ->
            case readLinkParts rest of
                Just ( altText, url, after ) ->
                    inlineScan after [] (Html.img [ HA.src url, HA.alt altText, HA.class "nb-md-img" ] [] :: flushText textRev nodes)

                Nothing ->
                    inlineScan ('[' :: rest) ('!' :: textRev) nodes

        '[' :: rest ->
            case readLinkParts rest of
                Just ( label, url, after ) ->
                    inlineScan after [] (a [ HA.href url, HA.target "_blank", HA.class "nb-md-link" ] [ text label ] :: flushText textRev nodes)

                Nothing ->
                    inlineScan rest ('[' :: textRev) nodes

        c :: rest ->
            inlineScan rest (c :: textRev) nodes


{-| Read a `label](url)` (the body of a `[label](url)` link or `![alt](url)` image) — the label up
to `]`, then a parenthesised URL — returning the remaining characters. -}
readLinkParts : List Char -> Maybe ( String, String, List Char )
readLinkParts chars =
    case readUntil [ ']' ] chars [] of
        Just ( label, afterLabel ) ->
            case afterLabel of
                '(' :: rest ->
                    case readUntil [ ')' ] rest [] of
                        Just ( url, after ) ->
                            Just ( label, url, after )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


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
