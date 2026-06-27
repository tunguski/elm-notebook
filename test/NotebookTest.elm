module NotebookTest exposing (suite)

{-| The elm-notebook test suite.

The kernel is pure — a code cell parses to an expression and evaluates to a value with no
side effects — so the whole stack is checked headlessly: the lexer/parser, the evaluator and
its standard library, the stateful kernel (cross-cell bindings, the `_` result, execution
counts), the notebook document operations, and the suggestion/lesson engine. As a strong
end-to-end check, every shipped lesson is executed through a real kernel and asserted to run
clean.

-}

import Expect exposing (Expectation)
import Notebook.Cell as Cell exposing (Cell, CellKind(..), Output(..))
import Notebook.Doc as Doc
import Notebook.Eval as Eval
import Notebook.Kernel as Kernel
import Notebook.Suggest as Suggest
import Notebook.Value as Value exposing (Value(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "elm-notebook"
        [ literalTests
        , arithmeticTests
        , comparisonTests
        , stringTests
        , listTests
        , higherOrderTests
        , recordTests
        , tableTests
        , controlFlowTests
        , mathStatTests
        , errorTests
        , kernelTests
        , docTests
        , suggestTests
        , lessonTests
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


{-| Assert that source evaluates (in the standard environment) to the expected value. -}
evalOk : String -> Value -> Expectation
evalOk src expected =
    case Eval.evalString Eval.defaultEnv src of
        Ok value ->
            if Value.equalValue value expected then
                Expect.pass

            else
                Expect.fail
                    (src ++ "  ⇒  " ++ Value.toInline value ++ "  (expected " ++ Value.toInline expected ++ ")")

        Err message ->
            Expect.fail (src ++ "  ⇒  error: " ++ message)


{-| Assert that source fails to evaluate. -}
evalErr : String -> Expectation
evalErr src =
    case Eval.evalString Eval.defaultEnv src of
        Ok value ->
            Expect.fail (src ++ " unexpectedly succeeded with " ++ Value.toInline value)

        Err _ ->
            Expect.pass


check : String -> String -> Value -> Test
check name src expected =
    test name (\_ -> evalOk src expected)



-- LITERALS -------------------------------------------------------------------


literalTests : Test
literalTests =
    describe "literals & parsing"
        [ check "integer" "42" (n 42)
        , check "float" "3.5" (n 3.5)
        , check "string" "\"hi\"" (s "hi")
        , check "true" "True" (VBool True)
        , check "false" "False" (VBool False)
        , check "empty list" "[]" (vlist [])
        , check "list" "[1, 2, 3]" (vlist [ n 1, n 2, n 3 ])
        , check "nested list" "[[1], [2, 3]]" (vlist [ vlist [ n 1 ], vlist [ n 2, n 3 ] ])
        , check "record" "{ a = 1, b = 2 }" (VRecord [ ( "a", n 1 ), ( "b", n 2 ) ])
        , check "line comment ignored" "1 + 2 -- adds them\n" (n 3)
        , check "constant pi" "round (pi * 100)" (n 314)
        ]



-- ARITHMETIC -----------------------------------------------------------------


arithmeticTests : Test
arithmeticTests =
    describe "arithmetic & precedence"
        [ check "add" "1 + 2" (n 3)
        , check "mul before add" "1 + 2 * 3" (n 7)
        , check "parens" "(1 + 2) * 3" (n 9)
        , check "subtract left assoc" "10 - 3 - 2" (n 5)
        , check "power right assoc" "2 ^ 3 ^ 2" (n 512)
        , check "unary minus" "-5 + 8" (n 3)
        , check "unary tighter than power" "-2 ^ 2" (n 4)
        , check "division" "9 / 2" (n 4.5)
        , check "float math" "0.5 + 0.25" (n 0.75)
        ]



-- COMPARISON & BOOLEAN -------------------------------------------------------


comparisonTests : Test
comparisonTests =
    describe "comparison & boolean"
        [ check "eq numbers" "2 == 2" (VBool True)
        , check "neq" "2 /= 3" (VBool True)
        , check "lt" "2 < 3" (VBool True)
        , check "ge" "3 >= 3" (VBool True)
        , check "string compare" "\"a\" < \"b\"" (VBool True)
        , check "list equality" "[1, 2] == [1, 2]" (VBool True)
        , check "record equality ignores field order" "{ a = 1, b = 2 } == { b = 2, a = 1 }" (VBool True)
        , check "and" "True && False" (VBool False)
        , check "or" "False || True" (VBool True)
        , check "not" "not False" (VBool True)
        , check "and short-circuits" "False && (1 / 0 > 0)" (VBool False)
        ]



-- STRINGS --------------------------------------------------------------------


stringTests : Test
stringTests =
    describe "strings"
        [ check "concat" "\"foo\" ++ \"bar\"" (s "foobar")
        , check "upper" "toUpper \"abc\"" (s "ABC")
        , check "lower" "toLower \"ABC\"" (s "abc")
        , check "trim" "trim \"  hi  \"" (s "hi")
        , check "words" "words \"a b  c\"" (vlist [ s "a", s "b", s "c" ])
        , check "split" "split \",\" \"a,b,c\"" (vlist [ s "a", s "b", s "c" ])
        , check "join" "join \"-\" [\"a\", \"b\", \"c\"]" (s "a-b-c")
        , check "contains" "contains \"ell\" \"hello\"" (VBool True)
        , check "replace" "replace \"o\" \"0\" \"foo\"" (s "f00")
        , check "toText number" "toText 42" (s "42")
        , check "toNumber" "toNumber \"3.5\"" (n 3.5)
        , check "string length" "length \"hello\"" (n 5)
        ]



-- LISTS ----------------------------------------------------------------------


listTests : Test
listTests =
    describe "lists & ranges"
        [ check "range" "range 1 5" (vlist [ n 1, n 2, n 3, n 4, n 5 ])
        , check "length" "length [10, 20, 30]" (n 3)
        , check "sum" "sum (range 1 10)" (n 55)
        , check "product" "product [1, 2, 3, 4]" (n 24)
        , check "maximum" "maximum [3, 9, 2]" (n 9)
        , check "minimum" "minimum [3, 9, 2]" (n 2)
        , check "head" "head [7, 8, 9]" (n 7)
        , check "last" "last [7, 8, 9]" (n 9)
        , check "take" "take 2 (range 1 9)" (vlist [ n 1, n 2 ])
        , check "drop" "drop 7 (range 1 9)" (vlist [ n 8, n 9 ])
        , check "reverse" "reverse [1, 2, 3]" (vlist [ n 3, n 2, n 1 ])
        , check "sort numbers" "sort [5, 1, 3]" (vlist [ n 1, n 3, n 5 ])
        , check "sort strings" "sort [\"c\", \"a\", \"b\"]" (vlist [ s "a", s "b", s "c" ])
        , check "unique" "unique [1, 1, 2, 3, 3, 3]" (vlist [ n 1, n 2, n 3 ])
        , check "member" "member 2 [1, 2, 3]" (VBool True)
        , check "concat" "concat [[1], [2, 3]]" (vlist [ n 1, n 2, n 3 ])
        , check "append op" "[1, 2] ++ [3]" (vlist [ n 1, n 2, n 3 ])
        ]



-- HIGHER ORDER ---------------------------------------------------------------


higherOrderTests : Test
higherOrderTests =
    describe "map · filter · fold"
        [ check "map square" "map (\\x -> x * x) [1, 2, 3]" (vlist [ n 1, n 4, n 9 ])
        , check "filter even" "filter (\\x -> mod x 2 == 0) (range 1 6)" (vlist [ n 2, n 4, n 6 ])
        , check "foldl sum" "foldl (\\x acc -> x + acc) 0 (range 1 5)" (n 15)
        , check "sortBy negate" "sortBy (\\x -> negate x) [1, 3, 2]" (vlist [ n 3, n 2, n 1 ])
        , check "pipe chain" "range 1 10 |> filter (\\x -> x > 5) |> sum" (n 40)
        , check "left pipe" "sum <| range 1 4" (n 10)
        , check "partial application" "map (max 5) [1, 7, 3]" (vlist [ n 5, n 7, n 5 ])
        ]



-- RECORDS --------------------------------------------------------------------


recordTests : Test
recordTests =
    describe "records"
        [ check "field access" "{ a = 1, b = 2 }.b" (n 2)
        , check "nested field" "{ p = { x = 9 } }.p.x" (n 9)
        , check "get builtin" "get \"name\" { name = \"Ada\" }" (s "Ada")
        , check "keys" "keys { a = 1, b = 2 }" (vlist [ s "a", s "b" ])
        , check "values" "values { a = 1, b = 2 }" (vlist [ n 1, n 2 ])
        ]



-- TABLES ---------------------------------------------------------------------


sampleTable : String
sampleTable =
    "[ { name = \"Ada\", dept = \"Eng\", salary = 100 }"
        ++ ", { name = \"Lin\", dept = \"Design\", salary = 80 }"
        ++ ", { name = \"Sam\", dept = \"Eng\", salary = 120 } ]"


tableTests : Test
tableTests =
    describe "tables (lists of records)"
        [ check "column" ("column \"salary\" " ++ sampleTable) (vlist [ n 100, n 80, n 120 ])
        , check "mean of column" ("mean (column \"salary\" " ++ sampleTable ++ ")") (n 100)
        , check "filter rows"
            ("length (filter (\\r -> r.salary > 90) " ++ sampleTable ++ ")")
            (n 2)
        , check "select projects columns"
            ("head (select [\"name\"] " ++ sampleTable ++ ")")
            (VRecord [ ( "name", s "Ada" ) ])
        , check "sortByField"
            ("column \"salary\" (sortByField \"salary\" " ++ sampleTable ++ ")")
            (vlist [ n 80, n 100, n 120 ])
        , check "groupBy count"
            ("length (groupBy \"dept\" " ++ sampleTable ++ ")")
            (n 2)
        , check "groupBy key & count"
            ("get \"count\" (head (groupBy \"dept\" " ++ sampleTable ++ "))")
            (n 2)
        , test "groupBy keeps the group's rows" <|
            \_ ->
                evalOk
                    ("get \"key\" (head (groupBy \"dept\" " ++ sampleTable ++ "))")
                    (s "Eng")
        ]



-- CONTROL FLOW ---------------------------------------------------------------


controlFlowTests : Test
controlFlowTests =
    describe "if / let / lambda"
        [ check "if true" "if 2 > 1 then \"yes\" else \"no\"" (s "yes")
        , check "if false" "if 2 < 1 then \"yes\" else \"no\"" (s "no")
        , check "let single" "let x = 10 in x + 5" (n 15)
        , check "let multiple" "let a = 2 ; b = 3 in a * b" (n 6)
        , check "let sees earlier binding" "let a = 2 ; b = a + 1 in b" (n 3)
        , check "lambda two args" "(\\x y -> x + y) 3 4" (n 7)
        , check "closure captures" "let k = 10 in map (\\x -> x + k) [1, 2]" (vlist [ n 11, n 12 ])
        ]



-- MATH / STATS ---------------------------------------------------------------


mathStatTests : Test
mathStatTests =
    describe "math & stats"
        [ check "abs" "abs (-7)" (n 7)
        , check "sqrt" "sqrt 16" (n 4)
        , check "round" "round 2.6" (n 3)
        , check "floor" "floor 2.9" (n 2)
        , check "ceiling" "ceiling 2.1" (n 3)
        , check "min" "min 3 8" (n 3)
        , check "max" "max 3 8" (n 8)
        , check "clamp" "clamp 0 10 15" (n 10)
        , check "mod" "mod 17 5" (n 2)
        , check "mean" "mean [2, 4, 6]" (n 4)
        , check "median odd" "median [3, 1, 2]" (n 2)
        , check "median even" "median [1, 2, 3, 4]" (n 2.5)
        , check "stddev" "stddev [2, 2, 2]" (n 0)
        ]



-- ERRORS ---------------------------------------------------------------------


errorTests : Test
errorTests =
    describe "errors"
        [ test "undefined name" (\_ -> evalErr "nope")
        , test "type error in arithmetic" (\_ -> evalErr "1 + \"a\"")
        , test "head of empty" (\_ -> evalErr "head []")
        , test "missing field" (\_ -> evalErr "{ a = 1 }.b")
        , test "calling a non-function" (\_ -> evalErr "5 3")
        , test "parse error" (\_ -> evalErr "1 +")
        , test "unterminated string" (\_ -> evalErr "\"oops")
        , test "if needs bool" (\_ -> evalErr "if 1 then 2 else 3")
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
                Expect.equal (outValueInline out) (Just "42")
        , test "underscore is the previous result" <|
            \_ ->
                let
                    ( _, k1 ) =
                        Kernel.run "10 + 5" Kernel.empty

                    ( out, _ ) =
                        Kernel.run "_ + 1" k1
                in
                Expect.equal (outValueInline out) (Just "16")
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


outValueInline : Output -> Maybe String
outValueInline output =
    case output of
        OutValue v ->
            Just (Value.toInline v)

        _ ->
            Nothing


isError : Output -> Bool
isError output =
    case output of
        OutError _ ->
            True

        _ ->
            False



-- DOCUMENT -------------------------------------------------------------------


docTests : Test
docTests =
    describe "notebook document"
        [ test "fromSpec then runAll computes outputs" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "a = 5" ), ( Code, "a * a" ) ]
                            |> Doc.runAll
                in
                case Doc.lastValue doc of
                    Just value ->
                        if Value.equalValue value (n 25) then
                            Expect.pass

                        else
                            Expect.fail (Value.toInline value)

                    Nothing ->
                        Expect.fail "no value produced"
        , test "markdown cells carry no output" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Markdown, "# hi" ), ( Code, "1 + 1" ) ]
                            |> Doc.runAll

                    markdownHasNoOutput =
                        case List.head doc.cells of
                            Just cell ->
                                cell.output == OutNone

                            Nothing ->
                                False
                in
                Expect.equal markdownHasNoOutput True
        , test "append adds a cell" <|
            \_ ->
                Expect.equal
                    (Doc.codeCount (Doc.append Code "1" Doc.empty))
                    1
        , test "remove deletes a cell by id" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "1" ), ( Code, "2" ) ]

                    firstId =
                        List.head doc.cells |> Maybe.map .id |> Maybe.withDefault 0
                in
                Expect.equal (Doc.codeCount (Doc.remove firstId doc)) 1
        , test "moveDown reorders cells" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "first" ), ( Code, "second" ) ]

                    firstId =
                        List.head doc.cells |> Maybe.map .id |> Maybe.withDefault 0

                    moved =
                        Doc.moveDown firstId doc

                    topSource =
                        List.head moved.cells |> Maybe.map .source |> Maybe.withDefault ""
                in
                Expect.equal topSource "second"
        , test "editing a cell clears its stale output" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "1 + 1" ) ] |> Doc.runAll

                    cellId =
                        List.head doc.cells |> Maybe.map .id |> Maybe.withDefault 0

                    edited =
                        Doc.setSource cellId "2 + 2" doc

                    cleared =
                        case List.head edited.cells of
                            Just cell ->
                                cell.output == OutNone

                            Nothing ->
                                False
                in
                Expect.equal cleared True
        , test "runThrough stops after the target cell" <|
            \_ ->
                let
                    doc =
                        Doc.fromSpec [ ( Code, "1" ), ( Code, "2" ), ( Code, "3" ) ]

                    secondId =
                        doc.cells |> List.drop 1 |> List.head |> Maybe.map .id |> Maybe.withDefault 0

                    ran =
                        Doc.runThrough secondId doc

                    thirdRan =
                        ran.cells
                            |> List.drop 2
                            |> List.head
                            |> Maybe.map (\c -> c.output /= OutNone)
                            |> Maybe.withDefault True
                in
                Expect.equal thirdRan False
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
                Expect.equal (List.any (String.contains "groupBy \"dept\"") sources) True
        , test "a table suggests averaging its numeric column" <|
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
                Expect.equal (List.any (String.contains "column \"salary\"") sources) True
        , test "a number list suggests sum and mean" <|
            \_ ->
                let
                    sources =
                        Suggest.suggestNext (Just (vlist [ n 1, n 2, n 3 ])) |> List.map .source
                in
                Expect.equal
                    ( List.member "sum _" sources, List.member "mean _" sources )
                    ( True, True )
        , test "no value yields starter suggestions" <|
            \_ ->
                Expect.equal (List.isEmpty (Suggest.suggestNext Nothing)) False
        , test "every suggestion always includes a note prompt" <|
            \_ ->
                let
                    kinds =
                        Suggest.suggestNext (Just (n 5)) |> List.map .kind
                in
                Expect.equal (List.member Markdown kinds) True
        ]



-- LESSONS (end to end) -------------------------------------------------------


lessonTests : Test
lessonTests =
    describe "lessons run clean end-to-end"
        (runsClean "starter" Suggest.starter
            :: List.map (\lesson -> runsClean lesson.id lesson.cells) Suggest.lessons
        )


{-| Load a lesson's cells into a notebook, run them all, and assert no code cell errored. -}
runsClean : String -> List ( CellKind, String ) -> Test
runsClean name spec =
    test name <|
        \_ ->
            let
                doc =
                    Doc.fromSpec spec |> Doc.runAll

                failures =
                    doc.cells
                        |> List.filter Cell.hasError
                        |> List.map describeFailure
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
