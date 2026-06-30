module Notebook.Export exposing (valueToTable, cellLinks, notebookLinks, toMarkdown, toElm, toHtml)

{-| Turn a cell's [`Lang.Value`](Lang#Value) into the workspace's neutral
[`Table`](Workspace-Types#Table), so any step's data can be **exported** to CSV / JSON (and, with a
backend, Excel). A table-shaped value (a list of records) becomes a header + rows; a plain list
becomes a single `value` column; anything else is not exportable.

The whole notebook can also be exported: [`toMarkdown`](#toMarkdown) renders it as a Markdown report
(prose, fenced code, and each step's output as a value or a Markdown table) and [`toElm`](#toElm)
as a runnable-style `.elm` module (declarations kept, bare expressions named, prose as comments).
[`cellLinks`](#cellLinks) / [`notebookLinks`](#notebookLinks) render the in-place download links
(data-URI anchors — the JS backend has no file-download port, so a `data:` link with a `download`
attribute is how a step or notebook is saved).

@docs valueToTable, cellLinks, notebookLinks, toMarkdown, toElm

-}

import Html exposing (Html, a, span, text)
import Html.Attributes as HA
import Lang exposing (Value(..))
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Output(..))
import Notebook.Doc exposing (Doc)
import Notebook.Value as Value
import Parser
import Url
import Workspace.Table as Table
import Workspace.Types exposing (Table)


{-| A cell value as a table, if it has tabular shape. -}
valueToTable : Value -> Maybe Table
valueToTable value =
    if Value.isTable value then
        let
            cols =
                Value.tableColumns value

            rows =
                case value of
                    VList records ->
                        List.map (rowOf cols) records

                    _ ->
                        []
        in
        Just { headers = cols, rows = rows }

    else
        case value of
            VList [] ->
                Nothing

            VList items ->
                Just { headers = [ "value" ], rows = List.map (\v -> [ Value.displayValue v ]) items }

            _ ->
                Nothing


rowOf : List String -> Value -> List String
rowOf cols record =
    List.map
        (\c -> Value.fieldOf c record |> Maybe.map Value.displayValue |> Maybe.withDefault "")
        cols


{-| The CSV / JSON download links for a cell's value (empty if the value is not tabular). `name` is
the base filename. -}
cellLinks : String -> Value -> Html msg
cellLinks name value =
    case valueToTable value of
        Just table ->
            span [ HA.class "nb-export" ]
                [ downloadLink "CSV" (name ++ ".csv") "text/csv" (Table.toCsv table)
                , downloadLink "JSON" (name ++ ".json") "application/json" (Table.toJson table)
                ]

        Nothing ->
            text ""


downloadLink : String -> String -> String -> String -> Html msg
downloadLink labelText filename mime content =
    a
        [ HA.class "nb-dl"
        , HA.href ("data:" ++ mime ++ ";charset=utf-8," ++ Url.percentEncode content)
        , HA.attribute "download" filename
        ]
        [ Html.i [ HA.class "bi bi-download" ] [], text (" " ++ labelText) ]


{-| Download links for the whole notebook: a Markdown report, an Elm script and an HTML report. -}
notebookLinks : Doc -> Html msg
notebookLinks doc =
    span [ HA.class "nb-export" ]
        [ downloadLink "Markdown" "notebook.md" "text/markdown" (toMarkdown doc)
        , downloadLink "Elm" "notebook.elm" "text/plain" (toElm doc)
        , downloadLink "HTML" "notebook.html" "text/html" (toHtml doc)
        ]



-- NOTEBOOK → HTML ------------------------------------------------------------


{-| The notebook as a **self-contained HTML report**: prose cells get light Markdown rendering,
code cells become `<pre>` blocks, and each output renders as a value or a table. Inline CSS keeps it
a single openable file. -}
toHtml : Doc -> String
toHtml doc =
    htmlHead
        ++ (doc.cells |> List.map cellHtml |> String.join "\n")
        ++ "\n</main>"


{-| The HTML preamble. NOTE: this is a *minimal* document — no `<head>`/`<title>`/`<body>` wrapper
and an unquoted `charset` — on purpose. This string is compiled into the host page's inlined JS, and
the page's build step post-processes the first `</head>`, `<title>`, `<meta charset="utf-8">`,
`</body>` and `</html>`; if those appeared here verbatim the build would mangle the script. A bare
`<!doctype html>` + `<style>` + `<main>` still renders as a self-contained file. -}
htmlHead : String
htmlHead =
    "<!doctype html><meta charset=utf-8><style>"
        ++ "body{font:15px/1.6 system-ui,sans-serif;color:#1f2733;max-width:820px;margin:32px auto;padding:0 18px}"
        ++ "h1,h2,h3{line-height:1.25}pre{background:#1e2330;color:#e7ecf6;padding:11px 14px;border-radius:8px;overflow-x:auto;font:13px/1.5 ui-monospace,Menlo,Consolas,monospace}"
        ++ ".out{background:#f5f7fb;color:#1f2733}.err{background:#fdecea;color:#b3261e}"
        ++ "table{border-collapse:collapse;margin:8px 0}th,td{border:1px solid #e2e8f2;padding:4px 10px;text-align:left}thead th{background:#eef0fe}"
        ++ "</style><main>"


cellHtml : Cell -> String
cellHtml cell =
    case cell.kind of
        Markdown ->
            proseHtml cell.source

        _ ->
            "<pre class=\"code\">" ++ escapeHtml cell.source ++ "</pre>" ++ outputHtml cell.output


proseHtml : String -> String
proseHtml source =
    source |> String.lines |> List.map lineHtml |> String.join "\n"


lineHtml : String -> String
lineHtml line =
    let
        t =
            String.trimLeft line
    in
    if String.startsWith "## " t then
        "<h3>" ++ escapeHtml (String.dropLeft 3 t) ++ "</h3>"

    else if String.startsWith "# " t then
        "<h2>" ++ escapeHtml (String.dropLeft 2 t) ++ "</h2>"

    else if String.trim line == "" then
        ""

    else
        "<p>" ++ escapeHtml line ++ "</p>"


outputHtml : Output -> String
outputHtml output =
    case output of
        OutValue value ->
            case valueToTable value of
                Just table ->
                    htmlTable table

                Nothing ->
                    "<pre class=\"out\">" ++ escapeHtml (Value.inlineValue value) ++ "</pre>"

        OutError message ->
            "<pre class=\"err\">⚠ " ++ escapeHtml message ++ "</pre>"

        OutNone ->
            ""


htmlTable : Table -> String
htmlTable table =
    let
        cells tag row =
            row |> List.map (\c -> "<" ++ tag ++ ">" ++ escapeHtml c ++ "</" ++ tag ++ ">") |> String.concat
    in
    "<table><thead><tr>"
        ++ cells "th" table.headers
        ++ "</tr></thead><tbody>"
        ++ (table.rows |> List.map (\r -> "<tr>" ++ cells "td" r ++ "</tr>") |> String.concat)
        ++ "</tbody></table>"


escapeHtml : String -> String
escapeHtml s =
    s
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"



-- NOTEBOOK → MARKDOWN --------------------------------------------------------


{-| The notebook as a Markdown report: prose cells verbatim, code cells as fenced `elm` blocks
followed by their rendered output (a value, a Markdown table, or an error note). -}
toMarkdown : Doc -> String
toMarkdown doc =
    doc.cells |> List.map cellMarkdown |> String.join "\n\n"


cellMarkdown : Cell -> String
cellMarkdown cell =
    case cell.kind of
        Markdown ->
            cell.source

        _ ->
            "```elm\n" ++ cell.source ++ "\n```" ++ outputMarkdown cell.output


outputMarkdown : Output -> String
outputMarkdown output =
    case output of
        OutValue value ->
            case valueToTable value of
                Just table ->
                    "\n\n" ++ markdownTable table

                Nothing ->
                    "\n\n`" ++ Value.inlineValue value ++ "`"

        OutError message ->
            "\n\n> ⚠ " ++ message

        OutNone ->
            ""


markdownTable : Table -> String
markdownTable table =
    let
        rowLine cells =
            "| " ++ String.join " | " cells ++ " |"
    in
    String.join "\n"
        (rowLine table.headers
            :: rowLine (List.map (\_ -> "---") table.headers)
            :: List.map rowLine table.rows
        )



-- NOTEBOOK → ELM -------------------------------------------------------------


{-| The notebook as an `.elm` module: declaration cells kept verbatim, bare-expression cells named
`out1`, `out2`, … (so the file is a valid module of definitions), and prose cells as `--` comments. -}
toElm : Doc -> String
toElm doc =
    let
        render ( index, cell ) =
            case cell.kind of
                Markdown ->
                    commentBlock cell.source

                _ ->
                    if isDeclaration cell.source then
                        cell.source

                    else
                        "out" ++ String.fromInt index ++ " =\n" ++ indent cell.source
    in
    "module Notebook exposing (..)\n\n-- Exported from elm-notebook.\n\n"
        ++ (doc.cells
                |> List.indexedMap (\i c -> ( i + 1, c ))
                |> List.map render
                |> String.join "\n\n\n"
           )
        ++ "\n"


isDeclaration : String -> Bool
isDeclaration source =
    case Parser.parseModule source of
        Ok (_ :: _) ->
            True

        _ ->
            False


commentBlock : String -> String
commentBlock source =
    source |> String.lines |> List.map (\line -> "-- " ++ line) |> String.join "\n"


indent : String -> String
indent source =
    source |> String.lines |> List.map (\line -> "    " ++ line) |> String.join "\n"
