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
import Notebook.Doc as Doc
import Notebook.Kernel as Kernel
import Notebook.Suggest as Suggest
import Notebook.Value as Value
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
        , controlFlowTests
        , stringTests
        , valueHelperTests
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
        , check "groupBy keys"
            ("List.map (\\g -> g.key) (groupBy (\\r -> r.dept) " ++ peopleSrc ++ ")")
            (vlist [ s "Eng", s "Design" ])
        , check "groupBy counts"
            ("List.map (\\g -> g.count) (groupBy (\\r -> r.dept) " ++ peopleSrc ++ ")")
            (vlist [ n 2, n 1 ])
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
