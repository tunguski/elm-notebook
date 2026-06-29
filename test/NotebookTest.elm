module NotebookTest exposing (suite)

{-| The elm-notebook test suite.

The kernel runs **real Elm** through the vendored elm-in-elm interpreter, and it is pure (a cell
parses to an expression and evaluates to a value with no side effects), so the whole stack is
checked headlessly: the evaluator and standard library, the prelude (`mean`/`groupBy`/`unique`),
the stateful kernel (cross-cell bindings, the `_` result, execution counts), the notebook
document operations, the value helpers and the suggestion engine. As a strong end-to-end check,
every shipped lesson is executed through a real kernel and asserted to run clean.

-}

import Expect exposing (Expectation)
import Lang exposing (Value(..))
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Output(..))
import Notebook.Complete as Complete
import Notebook.Correlation as Correlation
import Notebook.Csv as Csv
import Notebook.Deps as Deps
import Notebook.Doc as Doc
import Notebook.Heatmap as Heatmap
import Notebook.Hint as Hint
import Notebook.Import as Import
import Set
import Notebook.Export as Export
import Notebook.Kernel as Kernel
import Notebook.Math as Math
import Notebook.Pivot as Pivot
import Notebook.Profile as Profile
import Notebook.Serialize as Serialize
import Notebook.Share as Share
import Notebook.Slides as Slides
import Notebook.Sparkline as Sparkline
import Notebook.Suggest as Suggest
import Notebook.Templates as Templates
import Notebook.Value as Value
import Notebook.View as NbView
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-notebook"
        [ arithmeticTests
        , listTests
        , higherOrderTests
        , recordTests
        , tableTests
        , preludeTests
        , analysisTests
        , depsTests
        , hintTests
        , interpolateTests
        , profileTests
        , pivotTests
        , correlationTests
        , heatmapTests
        , sparklineTests
        , slidesTests
        , shareTests
        , mathTests
        , templatesTests
        , completeTests
        , controlFlowTests
        , scopeTests
        , parsingTests
        , stringTests
        , valueHelperTests
        , errorTests
        , kernelTests
        , docTests
        , suggestTests
        , csvTests
        , importTests
        , serializeTests
        , exportTests
        , lessonTests
        ]


exportTests : Test
exportTests =
    describe "export (cell value → workspace Table)"
        [ test "a table value becomes headers + rows" <|
            \_ ->
                Export.valueToTable
                    (VList
                        [ VRecord [ ( "name", VStr "Ada" ), ( "age", VNum 36 ) ]
                        , VRecord [ ( "name", VStr "Bob" ), ( "age", VNum 41 ) ]
                        ]
                    )
                    |> Expect.equal
                        (Just { headers = [ "name", "age" ], rows = [ [ "Ada", "36" ], [ "Bob", "41" ] ] })
        , test "a plain list becomes a single value column" <|
            \_ ->
                Export.valueToTable (VList [ VNum 1, VNum 2, VNum 3 ])
                    |> Expect.equal (Just { headers = [ "value" ], rows = [ [ "1" ], [ "2" ], [ "3" ] ] })
        , test "a scalar is not exportable" <|
            \_ ->
                Export.valueToTable (VNum 42) |> Expect.equal Nothing
        , test "notebook → Markdown has prose, fenced code and the output value" <|
            \_ ->
                let
                    md =
                        Export.toMarkdown
                            (Doc.empty |> Doc.append Markdown "# Title" |> Doc.append Code "1 + 2" |> Doc.runAll)
                in
                Expect.equal True
                    (String.contains "# Title" md && String.contains "```elm" md && String.contains "`3`" md)
        , test "notebook → Elm keeps declarations and names bare expressions" <|
            \_ ->
                let
                    elm =
                        Export.toElm (Doc.empty |> Doc.append Code "x = 5" |> Doc.append Code "x + 1")
                in
                Expect.equal True
                    (String.contains "module Notebook" elm && String.contains "x = 5" elm && String.contains "out2 =" elm)
        ]


serializeTests : Test
serializeTests =
    describe "serialization (save / load)"
        [ test "round-trips cells through JSON" <|
            \_ ->
                let
                    doc =
                        Doc.empty
                            |> Doc.append Markdown "# hi"
                            |> Doc.append Code "x = 5"
                            |> Doc.appendInput { name = "t", control = Cell.Slider 0 10 1, value = "3" }

                    restored =
                        Serialize.decode (Serialize.encode doc)
                in
                case restored of
                    Ok back ->
                        Expect.equal
                            (List.map .source back.cells)
                            [ "# hi", "x = 5", "t = 3" ]

                    Err message ->
                        Expect.fail message
        , test "preserves an input widget's control" <|
            \_ ->
                let
                    doc =
                        Doc.empty |> Doc.appendInput { name = "n", control = Cell.NumberBox, value = "7" }

                    restored =
                        Serialize.decode (Serialize.encode doc)
                in
                case restored |> Result.map (\d -> List.filterMap .input d.cells) of
                    Ok [ spec ] ->
                        Expect.equal ( spec.name, spec.control, spec.value ) ( "n", Cell.NumberBox, "7" )

                    _ ->
                        Expect.fail "expected one input spec"
        ]


csvTests : Test
csvTests =
    describe "CSV import"
        [ test "generates a runnable table with typed columns" <|
            \_ ->
                case Csv.toElm "people" "name, age\nAda, 36\nGrace, 41" of
                    Ok source ->
                        evalOk source
                            (vlist
                                [ VRecord [ ( "name", s "Ada" ), ( "age", n 36 ) ]
                                , VRecord [ ( "name", s "Grace" ), ( "age", n 41 ) ]
                                ]
                            )

                    Err message ->
                        Expect.fail message
        , test "auto-detects tab separation" <|
            \_ ->
                case Csv.toElm "t" "a\tb\n1\t2" of
                    Ok source ->
                        evalOk source (vlist [ VRecord [ ( "a", n 1 ), ( "b", n 2 ) ] ])

                    Err message ->
                        Expect.fail message
        , test "sanitises a messy header into a valid field" <|
            \_ ->
                case Csv.toElm "t" "First Name\nAda" of
                    Ok source ->
                        evalOk source (vlist [ VRecord [ ( "first_name", s "Ada" ) ] ])

                    Err message ->
                        Expect.fail message
        , test "rejects input with no data rows" <|
            \_ ->
                case Csv.toElm "t" "only,a,header" of
                    Ok _ ->
                        Expect.fail "expected an error"

                    Err _ ->
                        Expect.pass
        ]



importTests : Test
importTests =
    describe "data import (auto-detect JSON / CSV)"
        [ test "imports a JSON array of objects as a typed table" <|
            \_ ->
                case Import.toElm "people" "[{\"name\":\"Ada\",\"age\":36},{\"name\":\"Bob\",\"age\":41}]" of
                    Ok source ->
                        evalOk source
                            (vlist
                                [ VRecord [ ( "name", s "Ada" ), ( "age", n 36 ) ]
                                , VRecord [ ( "name", s "Bob" ), ( "age", n 41 ) ]
                                ]
                            )

                    Err message ->
                        Expect.fail message
        , test "falls back to CSV when the text isn't JSON" <|
            \_ ->
                case Import.toElm "t" "a,b\n1,2" of
                    Ok source ->
                        evalOk source (vlist [ VRecord [ ( "a", n 1 ), ( "b", n 2 ) ] ])

                    Err message ->
                        Expect.fail message
        , test "looksLikeJson detects an array vs CSV" <|
            \_ -> Expect.equal ( True, False ) ( Import.looksLikeJson "  [ 1 ]", Import.looksLikeJson "a,b\n1,2" )
        , test "sanitises JSON keys into valid field names" <|
            \_ ->
                case Import.toElm "t" "[{\"First Name\":\"Ada\"}]" of
                    Ok source ->
                        evalOk source (vlist [ VRecord [ ( "first_name", s "Ada" ) ] ])

                    Err message ->
                        Expect.fail message
        ]


-- HELPERS --------------------------------------------------------------------


n : Float -> Value
n =
    VNum


s : String -> Value
s =
    VStr


vlist : List Value -> Value
vlist =
    VList


{-| Evaluate one source string against a fresh kernel (prelude + the real stdlib). -}
evalOnce : String -> Output
evalOnce src =
    Tuple.first (Kernel.run src Kernel.empty)


evalOk : String -> Value -> Expectation
evalOk src expected =
    case evalOnce src of
        OutValue value ->
            if Value.equalValue value expected then
                Expect.pass

            else
                Expect.fail (src ++ "  ⇒  " ++ Value.inlineValue value ++ "  (expected " ++ Value.inlineValue expected ++ ")")

        OutError message ->
            Expect.fail (src ++ "  ⇒  error: " ++ message)

        OutNone ->
            Expect.fail (src ++ "  ⇒  no output")


evalErr : String -> Expectation
evalErr src =
    case evalOnce src of
        OutError _ ->
            Expect.pass

        OutValue value ->
            Expect.fail (src ++ " unexpectedly succeeded with " ++ Value.inlineValue value)

        OutNone ->
            Expect.fail (src ++ " produced no output")


{-| Assert a numeric result within a small epsilon — for ratios (correlation) where exact
Float equality is brittle. -}
evalApprox : String -> Float -> Expectation
evalApprox src expected =
    case evalOnce src of
        OutValue (VNum x) ->
            if abs (x - expected) < 1.0e-6 then
                Expect.pass

            else
                Expect.fail (src ++ "  ⇒  " ++ String.fromFloat x ++ "  (expected ≈ " ++ String.fromFloat expected ++ ")")

        other ->
            Expect.fail (src ++ "  ⇒  not a number: " ++ Maybe.withDefault "?" (inlineOf other))


check : String -> String -> Value -> Test
check name src expected =
    test name (\_ -> evalOk src expected)


inlineOf : Output -> Maybe String
inlineOf output =
    case output of
        OutValue v ->
            Just (Value.inlineValue v)

        _ ->
            Nothing


isError : Output -> Bool
isError output =
    case output of
        OutError _ ->
            True

        _ ->
            False



-- a sample table reused across tests --------------------------------------------


peopleSrc : String
peopleSrc =
    "[ { name = \"Ada\", dept = \"Eng\", salary = 100 }"
        ++ ", { name = \"Lin\", dept = \"Design\", salary = 80 }"
        ++ ", { name = \"Sam\", dept = \"Eng\", salary = 120 } ]"



-- ARITHMETIC -----------------------------------------------------------------


arithmeticTests : Test
arithmeticTests =
    describe "arithmetic & precedence"
        [ check "add" "1 + 2" (n 3)
        , check "mul before add" "1 + 2 * 3" (n 7)
        , check "parens" "(1 + 2) * 3" (n 9)
        , check "power right assoc" "2 ^ 3 ^ 2" (n 512)
        , check "division" "9 / 2" (n 4.5)
        , check "modBy" "modBy 5 17" (n 2)
        , check "comparison" "3 > 2" (VBool True)
        , check "boolean" "True && (1 < 2)" (VBool True)
        , check "float exact" "0.5 + 0.25" (n 0.75)
        ]



-- LISTS ----------------------------------------------------------------------


listTests : Test
listTests =
    describe "lists & the List library"
        [ check "range" "List.range 1 5" (vlist [ n 1, n 2, n 3, n 4, n 5 ])
        , check "sum" "List.sum (List.range 1 10)" (n 55)
        , check "length" "List.length [ 10, 20, 30 ]" (n 3)
        , check "maximum" "List.maximum [ 3, 9, 2 ]" (VCtor "Just" [ n 9 ])
        , check "reverse" "List.reverse [ 1, 2, 3 ]" (vlist [ n 3, n 2, n 1 ])
        , check "sort" "List.sort [ 5, 1, 3 ]" (vlist [ n 1, n 3, n 5 ])
        , check "take" "List.take 2 (List.range 1 9)" (vlist [ n 1, n 2 ])
        , check "append" "[ 1, 2 ] ++ [ 3 ]" (vlist [ n 1, n 2, n 3 ])
        , check "member" "List.member 2 [ 1, 2, 3 ]" (VBool True)
        ]



-- HIGHER ORDER ---------------------------------------------------------------


higherOrderTests : Test
higherOrderTests =
    describe "map · filter · fold"
        [ check "map" "List.map (\\x -> x * x) [ 1, 2, 3 ]" (vlist [ n 1, n 4, n 9 ])
        , check "filter" "List.filter (\\x -> modBy 2 x == 0) (List.range 1 6)" (vlist [ n 2, n 4, n 6 ])
        , check "foldl" "List.foldl (\\x acc -> x + acc) 0 (List.range 1 5)" (n 15)
        , check "sortBy" "List.sortBy (\\x -> negate x) [ 1, 3, 2 ]" (vlist [ n 3, n 2, n 1 ])
        , check "pipe chain" "List.range 1 10 |> List.filter (\\x -> x > 5) |> List.sum" (n 40)
        ]



-- RECORDS --------------------------------------------------------------------


recordTests : Test
recordTests =
    describe "records"
        [ check "field access" "{ a = 1, b = 2 }.b" (n 2)
        , check "nested field" "{ p = { x = 9 } }.p.x" (n 9)
        , check "record equality ignores order" "{ a = 1, b = 2 } == { b = 2, a = 1 }" (VBool True)
        , check "lambda accessor in map" "List.map (\\r -> r.x) [ { x = 1 }, { x = 2 } ]" (vlist [ n 1, n 2 ])
        ]



-- TABLES ---------------------------------------------------------------------


tableTests : Test
tableTests =
    describe "tables (lists of records)"
        [ check "column via map"
            ("List.map (\\r -> r.salary) " ++ peopleSrc)
            (vlist [ n 100, n 80, n 120 ])
        , check "filter rows"
            ("List.length (List.filter (\\r -> r.salary > 90) " ++ peopleSrc ++ ")")
            (n 2)
        , check "sortBy a column"
            ("List.map (\\r -> r.salary) (List.sortBy (\\r -> r.salary) " ++ peopleSrc ++ ")")
            (vlist [ n 80, n 100, n 120 ])
        ]



-- PRELUDE --------------------------------------------------------------------


preludeTests : Test
preludeTests =
    describe "prelude (mean · groupBy · unique)"
        [ check "mean" "mean [ 2, 4, 6 ]" (n 4)
        , check "mean of a column"
            ("mean (List.map (\\r -> r.salary) " ++ peopleSrc ++ ")")
            (n 100)
        , check "unique" "unique [ 1, 1, 2, 3, 3, 3 ]" (vlist [ n 1, n 2, n 3 ])
        , check "median odd" "median [ 3, 1, 2 ]" (n 2)
        , check "median even" "median [ 1, 2, 3, 4 ]" (n 2.5)
        , check "stdev of constants" "stdev [ 5, 5, 5 ]" (n 0)
        , check "describe count" "(describe [ 2, 4, 6 ]).count" (n 3)
        , check "describe mean" "(describe [ 2, 4, 6 ]).max" (n 6)
        , check "groupBy keys"
            ("List.map (\\g -> g.key) (groupBy (\\r -> r.dept) " ++ peopleSrc ++ ")")
            (vlist [ s "Eng", s "Design" ])
        , check "groupBy counts"
            ("List.map (\\g -> g.count) (groupBy (\\r -> r.dept) " ++ peopleSrc ++ ")")
            (vlist [ n 2, n 1 ])
        ]



-- ANALYSIS PRELUDE -----------------------------------------------------------


analysisTests : Test
analysisTests =
    describe "analysis prelude (corr · linfit · quantile · summarize)"
        [ check "minOf" "minOf [ 3, 1, 4, 1, 5 ]" (n 1)
        , check "maxOf" "maxOf [ 3, 1, 4, 1, 5 ]" (n 5)
        , check "spread" "spread [ 3, 1, 4, 1, 5 ]" (n 4)
        , check "cumSum" "cumSum [ 1, 2, 3, 4 ]" (vlist [ n 1, n 3, n 6, n 10 ])
        , check "normalize" "normalize [ 0, 5, 10 ]" (vlist [ n 0, n 0.5, n 1 ])
        , check "zip" "zip [ 1, 2 ] [ 3, 4 ]" (vlist [ VTup [ n 1, n 3 ], VTup [ n 2, n 4 ] ])
        , check "sortDesc" "sortDesc [ 3, 1, 2 ]" (vlist [ n 3, n 2, n 1 ])
        , check "quantile median" "quantile 0.5 [ 1, 2, 3 ]" (n 2)
        , check "percentile max" "percentile 100 [ 1, 2, 3, 4 ]" (n 4)
        , check "linfit slope" "(linfit [ 1, 2, 3 ] [ 2, 4, 6 ]).slope" (n 2)
        , check "linfit intercept" "(linfit [ 1, 2, 3 ] [ 2, 4, 6 ]).intercept" (n 0)
        , check "predict" "predict (linfit [ 1, 2, 3 ] [ 2, 4, 6 ]) 4" (n 8)
        , test "corr of a perfect line is 1" (\_ -> evalApprox "corr [ 1, 2, 3 ] [ 2, 4, 6 ]" 1.0)
        , test "cov of constants is 0" (\_ -> evalApprox "cov [ 1, 2, 3 ] [ 5, 5, 5 ]" 0.0)
        , check "summarize sum"
            ("(nth 0 (summarize (\\r -> r.dept) (\\r -> r.salary) " ++ peopleSrc ++ ")).sum")
            (n 220)
        , check "summarize mean"
            ("(nth 0 (summarize (\\r -> r.dept) (\\r -> r.salary) " ++ peopleSrc ++ ")).mean")
            (n 110)
        , check "countBy Eng count"
            ("(nth 0 (countBy (\\r -> r.dept) " ++ peopleSrc ++ ")).count")
            (n 2)
        , check "linspace endpoints and spacing" "linspace 0 10 5" (vlist [ n 0, n 2.5, n 5, n 7.5, n 10 ])
        , check "plot samples 50 points" "List.length (plot (\\x -> x) 0 1)" (n 50)
        , check "plotPoints carries x and y" "(nth 0 (plotPoints (\\x -> x + 100) 0 10)).y" (n 100)
        ]


-- DEPENDENCY ANALYSIS (reactive execution) -----------------------------------


depsDoc : Doc.Doc
depsDoc =
    Doc.empty
        |> Doc.append Code "a = 1"
        |> Doc.append Code "b = a + 1"
        |> Doc.append Code "c = 99"
        |> Doc.append Code "d = b * 2"


hasOutput : Int -> Doc.Doc -> Bool
hasOutput id doc =
    case Doc.find id doc of
        Just cell ->
            case cell.output of
                OutNone ->
                    False

                _ ->
                    True

        Nothing ->
            False


depsTests : Test
depsTests =
    describe "dependency analysis (reactive execution)"
        [ test "defines: declaration names plus _" <|
            \_ -> Expect.equal [ "_", "x" ] (Deps.defines (Cell.code 7 "x = 5"))
        , test "defines: a bare expression binds only _" <|
            \_ -> Expect.equal [ "_" ] (Deps.defines (Cell.code 7 "1 + 2"))
        , test "defines: markdown binds nothing" <|
            \_ -> Expect.equal [] (Deps.defines (Cell.markdown 7 "# hi"))
        , test "refs: collects identifiers but not field names" <|
            \_ ->
                Expect.equal ( True, False )
                    ( List.member "x" (Deps.refs (Cell.code 7 "x + y"))
                    , List.member "salary" (Deps.refs (Cell.code 7 "r.salary"))
                    )
        , test "affected: a cell plus its transitive downstream" <|
            \_ -> Expect.equal [ 1, 2, 4 ] (Set.toList (Deps.affected 1 depsDoc))
        , test "affected: an independent cell affects only itself" <|
            \_ -> Expect.equal [ 3 ] (Set.toList (Deps.affected 3 depsDoc))
        , test "runAffected refreshes only the given cells, not their unrelated siblings" <|
            \_ ->
                let
                    ran =
                        Doc.runAffected (Set.fromList [ 1, 2 ]) depsDoc
                in
                Expect.equal ( True, False ) ( hasOutput 2 ran, hasOutput 4 ran )
        ]


-- SMART ERROR HELP (Notebook.Hint) -------------------------------------------


hintTests : Test
hintTests =
    describe "smart error help (did-you-mean)"
        [ test "edit distance: one substitution" <|
            \_ -> Expect.equal 1 (Hint.distance "mean" "men")
        , test "edit distance: identical" <|
            \_ -> Expect.equal 0 (Hint.distance "groupBy" "groupBy")
        , test "edit distance: insertions" <|
            \_ -> Expect.equal 2 (Hint.distance "ab" "abcd")
        , test "unboundName pulls the name out of an undefined-variable message" <|
            \_ -> Expect.equal (Just "maen") (Hint.unboundName "undefined variable: maen")
        , test "unboundName ignores unrelated errors" <|
            \_ -> Expect.equal Nothing (Hint.unboundName "type mismatch in argument")
        , test "closest finds the near in-scope name" <|
            \_ -> Expect.equal (Just "mean") (Hint.closest "maen" [ "mean", "median", "groupBy" ])
        , test "closest stays silent when nothing is near" <|
            \_ -> Expect.equal Nothing (Hint.closest "xyzzy" [ "mean", "total" ])
        ]


-- PIVOT TABLES ---------------------------------------------------------------


pivotTests : Test
pivotTests =
    let
        sales =
            VList
                [ VRecord [ ( "region", s "W" ), ( "product", s "A" ), ( "units", n 10 ) ]
                , VRecord [ ( "region", s "W" ), ( "product", s "B" ), ( "units", n 5 ) ]
                , VRecord [ ( "region", s "E" ), ( "product", s "A" ), ( "units", n 7 ) ]
                ]

        spec =
            { row = "region", column = "product", value = "units", agg = Pivot.Sum }

        grid =
            Pivot.pivot spec sales
    in
    describe "pivot tables"
        [ test "columns are the distinct values of the column field" <|
            \_ -> Expect.equal [ "A", "B" ] grid.columns
        , test "rows are the distinct values of the row field" <|
            \_ -> Expect.equal [ "W", "E" ] (List.map .label grid.rows)
        , test "cells aggregate the value field, blank where empty" <|
            \_ -> Expect.equal [ [ "10", "5" ], [ "7", "" ] ] (List.map .cells grid.rows)
        , test "mean aggregation" <|
            \_ ->
                Expect.equal [ [ "7.5" ] ]
                    (List.map .cells (Pivot.pivot { row = "region", column = "product", value = "units", agg = Pivot.Mean } onePair).rows)
        , test "defaultSpec picks a text row, a distinct column and a numeric value" <|
            \_ ->
                let
                    d =
                        Pivot.defaultSpec sales
                in
                Expect.equal ( "region", "product", "units" ) ( d.row, d.column, d.value )
        ]


onePair : Value
onePair =
    VList
        [ VRecord [ ( "region", s "W" ), ( "product", s "A" ), ( "units", n 10 ) ]
        , VRecord [ ( "region", s "W" ), ( "product", s "A" ), ( "units", n 5 ) ]
        ]


-- CORRELATION ----------------------------------------------------------------


correlationTests : Test
correlationTests =
    let
        tbl =
            VList
                [ VRecord [ ( "a", n 1 ), ( "b", n 2 ) ]
                , VRecord [ ( "a", n 2 ), ( "b", n 4 ) ]
                , VRecord [ ( "a", n 3 ), ( "b", n 6 ) ]
                ]

        m =
            Correlation.matrix tbl

        rounded =
            List.map (List.map (Maybe.map (\r -> round (r * 100)))) m.rows
    in
    describe "correlation matrix"
        [ test "columns are the numeric columns" <|
            \_ -> Expect.equal [ "a", "b" ] m.columns
        , test "perfectly-correlated columns give r = 1 everywhere" <|
            \_ -> Expect.equal [ [ Just 100, Just 100 ], [ Just 100, Just 100 ] ] rounded
        , test "a constant column has undefined correlation" <|
            \_ ->
                Correlation.matrix
                    (VList [ VRecord [ ( "k", n 5 ) ], VRecord [ ( "k", n 5 ) ] ])
                    |> .rows
                    |> Expect.equal [ [ Nothing ] ]
        ]


-- CODE COMPLETION ------------------------------------------------------------


completeTests : Test
completeTests =
    describe "code completion"
        [ test "currentToken: the identifier left of the caret" <|
            \_ -> Expect.equal "me" (Complete.currentToken "x = me" 6)
        , test "currentToken: a dot ends the token" <|
            \_ -> Expect.equal "ma" (Complete.currentToken "List.ma" 7)
        , test "completions: prefix match, sorted, excluding the exact token" <|
            \_ ->
                Expect.equal [ "mean", "median" ]
                    (Complete.completions "me" 2 [ "median", "mean", "total", "me" ])
        , test "completions: nothing when there is no token" <|
            \_ -> Expect.equal [] (Complete.completions "x = " 4 [ "mean" ])
        , test "apply: splices the chosen name and returns the new caret" <|
            \_ -> Expect.equal ( "x = mean", 8 ) (Complete.apply "x = me" 6 "mean")
        ]


-- DATA PROFILING -------------------------------------------------------------


profileTests : Test
profileTests =
    let
        table =
            VList
                [ VRecord [ ( "dept", s "Eng" ), ( "salary", n 100 ) ]
                , VRecord [ ( "dept", s "Eng" ), ( "salary", n 120 ) ]
                , VRecord [ ( "dept", s "Design" ), ( "salary", n 80 ) ]
                ]

        cols =
            Profile.columns table

        colNamed name =
            List.filter (\c -> c.name == name) cols |> List.head
    in
    describe "data profiling"
        [ test "profiles every column" <|
            \_ -> Expect.equal [ "dept", "salary" ] (List.map .name cols)
        , test "a text column: kind, count, distinct, no numeric stats" <|
            \_ ->
                case colNamed "dept" of
                    Just c ->
                        Expect.equal ( "text", 3, 2, Nothing ) ( c.kind, c.count, c.distinct, c.mean )

                    Nothing ->
                        Expect.fail "no dept column"
        , test "a numeric column: min / max / mean" <|
            \_ ->
                case colNamed "salary" of
                    Just c ->
                        -- mean is computed, so compare it through a string to dodge the Float-literal gotcha
                        Expect.equal ( "number", Just 80, Just 120, Just "100" )
                            ( c.kind, c.min, c.max, Maybe.map String.fromFloat c.mean )

                    Nothing ->
                        Expect.fail "no salary column"
        , test "a non-table value profiles to nothing" <|
            \_ -> Expect.equal [] (Profile.columns (VNum 5))
        ]


-- MARKDOWN INTERPOLATION -----------------------------------------------------


interpolateTests : Test
interpolateTests =
    describe "markdown {{ expr }} interpolation"
        [ test "substitutes an evaluated expression" <|
            \_ -> Expect.equal "a x! b" (NbView.interpolate (\e -> Just (e ++ "!")) "a {{ x }} b")
        , test "trims the expression before evaluating" <|
            \_ -> Expect.equal "=mean=" (NbView.interpolate (\e -> Just ("=" ++ e ++ "=")) "{{   mean   }}")
        , test "leaves an unevaluable span as a literal" <|
            \_ -> Expect.equal "{{x}} y" (NbView.interpolate (\_ -> Nothing) "{{x}} y")
        , test "passes prose without braces through unchanged" <|
            \_ -> Expect.equal "no braces here" (NbView.interpolate (\_ -> Just "!") "no braces here")
        , test "handles several spans" <|
            \_ -> Expect.equal "1 and 2" (NbView.interpolate (\e -> Just e) "{{1}} and {{2}}")
        ]


-- CONTROL FLOW ---------------------------------------------------------------


controlFlowTests : Test
controlFlowTests =
    describe "if · case · let"
        [ check "if true" "if 2 > 1 then \"yes\" else \"no\"" (s "yes")
        , check "let" "let x = 10 in x + 5" (n 15)
        , check "case on Maybe"
            "case List.head [ 7, 8 ] of\n    Just v ->\n        v\n\n    Nothing ->\n        0"
            (n 7)
        , check "case on Nothing"
            "case List.head [] of\n    Just v ->\n        v\n\n    Nothing ->\n        0"
            (n 0)
        ]



-- SCOPE · SHADOWING · CLOSURES -----------------------------------------------


{-| The local environment is a `Dict String Value` (was a linear assoc-list). These pin the
behaviour that change must preserve: newer bindings shadow older ones (Dict.insert overwrites,
matching the old prepend-then-first-match), closures capture their defining env persistently and
independently, and recursive (VRec) bindings still see themselves. -}
scopeTests : Test
scopeTests =
    describe "scope · shadowing · closures"
        [ check "inner let shadows outer" "let x = 1 in let x = 2 in x" (n 2)
        , check "lambda param shadows binding" "let x = 1 in (\\x -> x) 5" (n 5)
        , check "outer binding visible after shadowing lambda" "let a = 1 in (\\a -> a) 2 + a" (n 3)
        , check "closure captures its defining env" "let a = 10 in let f = \\b -> a + b in f 5" (n 15)
        , check "recursion sees the bound name (VRec)"
            "let fac = \\m -> if m <= 1 then 1 else m * fac (m - 1) in fac 5"
            (n 120)
        , check "closures keep independent captures"
            "let mk = \\a -> (\\b -> a + b) in mk 100 1 + mk 200 1"
            (n 302)
        , check "case binding shadows but outer stays visible"
            "let x = 1 in\ncase 9 of\n    y ->\n        y + x"
            (n 10)

        -- A `let` binding whose value is a `case`, FOLLOWED by a sibling binding. The layout lexer
        -- renders both case-branch and let-binding separators as a bare `;`, so the case parser used
        -- to swallow the sibling binding's `;` and fail ("expected '->' in case branch") — which, in
        -- the single-module prelude, silently wiped every helper. The case must stop at the `;` that
        -- belongs to the enclosing `let`.
        , check "case-valued binding then sibling binding"
            "let\n    a =\n        case 0 of\n            0 -> 1\n            _ -> 2\n    b = 10\nin\na + b"
            (n 11)
        , check "two case-valued bindings"
            "let\n    a =\n        case 0 of\n            0 -> 1\n            _ -> 2\n    b =\n        case 1 of\n            1 -> 100\n            _ -> 200\nin\na + b"
            (n 101)
        ]



-- PARSING (element order) ----------------------------------------------------


{-| The tuple/record/list/branch/pattern parsers accumulate with cons + a final List.reverse (was
`acc ++ [x]`); these pin that the reversal preserves element order in multi-element literals and
patterns — the one behaviour such a rewrite can get wrong. -}
parsingTests : Test
parsingTests =
    describe "literal & pattern element order"
        [ check "list literal order" "[ 3, 1, 2 ]" (vlist [ n 3, n 1, n 2 ])
        , check "long list order" "[ 10, 20, 30, 40, 50 ]" (vlist [ n 10, n 20, n 30, n 40, n 50 ])
        , check "tuple order" "( 1, 2, 3 )" (VTup [ n 1, n 2, n 3 ])
        , check "record field order"
            "{ a = 1, b = 2, c = 3 }"
            (VRecord [ ( "a", n 1 ), ( "b", n 2 ), ( "c", n 3 ) ])
        , check "multi-branch case picks the right arm"
            "case 3 of\n    1 ->\n        10\n\n    2 ->\n        20\n\n    _ ->\n        30"
            (n 30)
        , check "tuple pattern binds in order"
            "case ( 1, 2, 3 ) of\n    ( a, b, c ) ->\n        a * 100 + b * 10 + c"
            (n 123)
        , check "function pattern-args order"
            "let\n    f a b c =\n        a + b * 10 + c * 100\nin\nf 1 2 3"
            (n 321)
        , check "record pattern fields"
            "(\\{ a, b } -> a - b) { a = 5, b = 2 }"
            (n 3)
        ]



-- STRINGS --------------------------------------------------------------------


stringTests : Test
stringTests =
    describe "the String library"
        [ check "concat" "\"foo\" ++ \"bar\"" (s "foobar")
        , check "toUpper" "String.toUpper \"abc\"" (s "ABC")
        , check "words" "String.words \"a b  c\"" (vlist [ s "a", s "b", s "c" ])
        , check "join" "String.join \"-\" [ \"a\", \"b\", \"c\" ]" (s "a-b-c")
        , check "contains" "String.contains \"ell\" \"hello\"" (VBool True)
        , check "fromInt" "String.fromInt 42" (s "42")

        -- String-literal decoding (lexer takeString / triple-string escaping) — these pin the
        -- escape handling that the linear-time accumulator rewrite touched.
        , check "escape newline" "String.length \"a\\nb\"" (n 3)
        , check "escape backslash" "String.length \"a\\\\b\"" (n 3)
        , check "escape quote" "\"q\\\"q\"" (s "q\"q")
        , check "unicode escape" "\"\\u{41}\"" (s "A")
        , check "triple string plain" "\"\"\"hi\"\"\"" (s "hi")
        , check "triple string raw quote" "\"\"\"a\"b\"\"\"" (s "a\"b")
        ]



-- VALUE HELPERS --------------------------------------------------------------


valueHelperTests : Test
valueHelperTests =
    describe "value helpers"
        [ test "isTable recognises a list of records" <|
            \_ ->
                Expect.equal
                    (Value.isTable (VList [ VRecord [ ( "a", n 1 ) ], VRecord [ ( "a", n 2 ) ] ]))
                    True
        , test "isTable rejects a list of numbers" <|
            \_ ->
                Expect.equal (Value.isTable (vlist [ n 1, n 2 ])) False
        , test "is2D recognises a list of lists" <|
            \_ ->
                Expect.equal (Value.is2D (VList [ vlist [ n 1 ], vlist [ n 2 ] ])) True
        , test "tableColumns reads the first row's fields" <|
            \_ ->
                Expect.equal
                    (Value.tableColumns (VList [ VRecord [ ( "x", n 1 ), ( "y", n 2 ) ] ]))
                    [ "x", "y" ]
        , test "inlineValue renders a record" <|
            \_ ->
                Expect.equal (Value.inlineValue (VRecord [ ( "a", n 1 ) ])) "{ a = 1 }"
        , test "equalValue is false across types" <|
            \_ ->
                Expect.equal (Value.equalValue (n 1) (s "1")) False
        ]



-- ERRORS ---------------------------------------------------------------------


errorTests : Test
errorTests =
    describe "errors"
        [ test "undefined name" (\_ -> evalErr "nope")
        , test "type error" (\_ -> evalErr "1 + \"a\"")
        , test "missing field" (\_ -> evalErr "{ a = 1 }.b")
        , test "parse error" (\_ -> evalErr "1 +")
        ]



-- KERNEL ---------------------------------------------------------------------


kernelTests : Test
kernelTests =
    describe "kernel (stateful cells)"
        [ test "binding persists to the next cell" <|
            \_ ->
                let
                    ( _, k1 ) =
                        Kernel.run "x = 21" Kernel.empty

                    ( out, _ ) =
                        Kernel.run "x * 2" k1
                in
                Expect.equal (inlineOf out) (Just "42")
        , test "function definitions are reusable" <|
            \_ ->
                let
                    ( _, k1 ) =
                        Kernel.run "double x = x * 2" Kernel.empty

                    ( out, _ ) =
                        Kernel.run "List.map double [ 1, 2, 3 ]" k1
                in
                Expect.equal (inlineOf out) (Just "[2, 4, 6]")
        , test "underscore is the previous result" <|
            \_ ->
                let
                    ( _, k1 ) =
                        Kernel.run "10 + 5" Kernel.empty

                    ( out, _ ) =
                        Kernel.run "_ + 1" k1
                in
                Expect.equal (inlineOf out) (Just "16")
        , test "execution count advances" <|
            \_ ->
                let
                    ( _, k1 ) =
                        Kernel.run "1" Kernel.empty

                    ( _, k2 ) =
                        Kernel.run "2" k1
                in
                Expect.equal k2.count 2
        , test "count advances even on error" <|
            \_ ->
                let
                    ( out, k1 ) =
                        Kernel.run "boom" Kernel.empty
                in
                Expect.equal ( isError out, k1.count ) ( True, 1 )
        ]



-- DOCUMENT -------------------------------------------------------------------


docTests : Test
docTests =
    describe "notebook document"
        [ test "fromSpec then runAll computes outputs" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "a = 5" ), ( Code, "a * a" ) ] |> Doc.runAll
                in
                case Doc.lastValue doc of
                    Just value ->
                        if Value.equalValue value (n 25) then
                            Expect.pass

                        else
                            Expect.fail (Value.inlineValue value)

                    Nothing ->
                        Expect.fail "no value produced"
        , test "markdown cells carry no output" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Markdown, "# hi" ), ( Code, "1 + 1" ) ] |> Doc.runAll
                in
                Expect.equal
                    (List.head doc.cells |> Maybe.map (\c -> c.output == OutNone))
                    (Just True)
        , test "editing a cell clears its stale output" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "1 + 1" ) ] |> Doc.runAll

                    cellId =
                        List.head doc.cells |> Maybe.map .id |> Maybe.withDefault 0

                    edited =
                        Doc.setSource cellId "2 + 2" doc
                in
                Expect.equal
                    (List.head edited.cells |> Maybe.map (\c -> c.output == OutNone))
                    (Just True)
        , test "moveDown reorders cells" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "first" ), ( Code, "second" ) ]

                    firstId =
                        List.head doc.cells |> Maybe.map .id |> Maybe.withDefault 0

                    moved =
                        Doc.moveDown firstId doc
                in
                Expect.equal
                    (List.head moved.cells |> Maybe.map .source)
                    (Just "second")
        ]



-- SUGGESTIONS ----------------------------------------------------------------


suggestTests : Test
suggestTests =
    describe "suggestions"
        [ test "a table suggests grouping by its text column" <|
            \_ ->
                let
                    table =
                        VList
                            [ VRecord [ ( "dept", s "Eng" ), ( "salary", n 100 ) ]
                            , VRecord [ ( "dept", s "Design" ), ( "salary", n 80 ) ]
                            ]

                    sources =
                        Suggest.suggestNext (Just table) |> List.map .source
                in
                Expect.equal (List.any (String.contains "groupBy (\\r -> r.dept)") sources) True
        , test "a number list suggests sum and mean" <|
            \_ ->
                let
                    sources =
                        Suggest.suggestNext (Just (vlist [ n 1, n 2, n 3 ])) |> List.map .source
                in
                Expect.equal
                    ( List.member "List.sum _" sources, List.member "mean _" sources )
                    ( True, True )
        , test "no value yields starter suggestions" <|
            \_ ->
                Expect.equal (List.isEmpty (Suggest.suggestNext Nothing)) False
        , test "every context includes a markdown note prompt" <|
            \_ ->
                Expect.equal
                    (List.member Markdown (Suggest.suggestNext (Just (n 5)) |> List.map .kind))
                    True
        ]



-- LESSONS (end to end) -------------------------------------------------------


lessonTests : Test
lessonTests =
    describe "lessons run clean end-to-end"
        (runsClean "starter" Suggest.starter
            :: List.map (\lesson -> runsClean lesson.id lesson.cells) Suggest.lessons
        )


heatmapTests : Test
heatmapTests =
    describe "heatmap (conditional formatting)"
        [ test "range is the column's min and max" <|
            \_ -> Heatmap.range [ 3, 1, 2 ] |> Expect.equal (Just ( 1, 3 ))
        , test "an empty column has no range" <|
            \_ -> Heatmap.range [] |> Expect.equal Nothing
        , test "the minimum value is faint" <|
            \_ -> Expect.equal True (String.contains "0.07)" (Heatmap.color ( 0, 10 ) 0))
        , test "the maximum value is strong" <|
            \_ -> Expect.equal True (String.contains "0.7)" (Heatmap.color ( 0, 10 ) 10))
        , test "a flat column shades at the mid tone" <|
            \_ -> Expect.equal True (String.contains "0.385)" (Heatmap.color ( 5, 5 ) 5))
        ]


sparklineTests : Test
sparklineTests =
    describe "sparkline geometry"
        [ test "an empty series has no points" <|
            \_ -> Sparkline.points [] |> Expect.equal []
        , test "one point per value" <|
            \_ -> List.length (Sparkline.points [ 1, 2, 3, 4 ]) |> Expect.equal 4
        , test "a single value sits at the centre" <|
            \_ -> List.length (Sparkline.points [ 5 ]) |> Expect.equal 1
        , test "larger values are plotted higher (smaller y)" <|
            \_ ->
                case List.map Tuple.second (Sparkline.points [ 0, 10 ]) of
                    [ a, b ] ->
                        Expect.equal True (a > b)

                    _ ->
                        Expect.fail "expected two points"
        ]


slidesTests : Test
slidesTests =
    let
        deck =
            Doc.empty
                |> Doc.append Markdown "# Intro\n\nwelcome"
                |> Doc.append Code "1 + 1"
                |> Doc.append Markdown "## Details"
                |> Doc.append Code "2 + 2"
                |> Slides.slides
    in
    describe "slides (presentation mode)"
        [ test "a deck splits at top-level headings" <|
            \_ -> List.length deck |> Expect.equal 2
        , test "slide titles come from their headings" <|
            \_ -> List.map .title deck |> Expect.equal [ "Intro", "Details" ]
        , test "cells travel with their heading" <|
            \_ -> List.map (\s -> List.length s.cells) deck |> Expect.equal [ 2, 2 ]
        ]


shareTests : Test
shareTests =
    let
        doc =
            Doc.empty
                |> Doc.append Markdown "# Shared"
                |> Doc.append Code "mean [ 1, 2, 3 ]"
    in
    describe "share by link"
        [ test "encode → decode round-trips the notebook" <|
            \_ ->
                Share.decode (Share.encode doc)
                    |> Maybe.map Serialize.encode
                    |> Expect.equal (Just (Serialize.encode doc))
        , test "a link carries the encoded token in a #nb= fragment" <|
            \_ -> Expect.equal True (String.startsWith "#nb=" (Share.link "" doc))
        , test "garbage decodes to Nothing" <|
            \_ -> Share.decode "" |> Expect.equal Nothing
        ]


mathTests : Test
mathTests =
    describe "inline math symbol substitution"
        [ test "Greek letters" <|
            \_ -> Math.replaceSymbols "\\alpha + \\beta" |> Expect.equal "α + β"
        , test "longer macros win over their prefixes (\\leq before \\le)" <|
            \_ -> Math.replaceSymbols "\\leq \\le \\geq \\ge" |> Expect.equal "≤ ≤ ≥ ≥"
        , test "operators and big symbols" <|
            \_ -> Math.replaceSymbols "\\sum \\sqrt \\pi \\times" |> Expect.equal "∑ √ π ×"
        ]


templatesTests : Test
templatesTests =
    describe "starter templates"
        ([ test "there are several templates" <|
            \_ -> Expect.equal True (List.length Templates.all >= 4)
         , test "byId finds a template by its id" <|
            \_ -> Templates.byId "sales" |> Maybe.map .title |> Expect.equal (Just "Sales analysis")
         , test "an unknown id is Nothing" <|
            \_ -> Templates.byId "nope" |> Expect.equal Nothing
         ]
            ++ List.map (\t -> runsClean ("template " ++ t.id) t.cells) Templates.all
        )


runsClean : String -> List ( CellKind, String ) -> Test
runsClean name spec =
    test name <|
        \_ ->
            let
                doc =
                    Doc.fromSpec spec |> Doc.runAll

                failures =
                    doc.cells |> List.filter Cell.hasError |> List.map describeFailure
            in
            if List.isEmpty failures then
                Expect.pass

            else
                Expect.fail (String.join "\n" failures)


describeFailure : Cell -> String
describeFailure cell =
    case cell.output of
        OutError message ->
            cell.source ++ "  ⇒  " ++ message

        _ ->
            cell.source
