module Notebook.Suggest exposing
    ( Lesson, Suggestion
    , lessons, starter
    , suggestNext
    )

{-| The teaching layer: ready-made **lessons** and **context-aware suggestions**.

[`lessons`](#lessons) are guided notebooks (markdown explanations interleaved with runnable
real Elm) the user loads with one click. [`suggestNext`](#suggestNext) inspects the value the
last cell produced and proposes concrete, insert-ready next steps — average this column, group
by that field, filter these rows — so exploring is a matter of picking the next move. All code is
idiomatic Elm (`List.map`/`List.filter`/`List.sortBy`, `\r -> r.field` accessors, and the
prelude's `mean`/`groupBy`); suggestions reference `_`, the kernel's binding for the last result.

@docs Lesson, Suggestion
@docs lessons, starter
@docs suggestNext

-}

import Lang exposing (Value(..))
import Notebook.Cell exposing (CellKind(..))
import Notebook.Value as Value


{-| A guided notebook: a title, a one-line blurb, and the cells to load. -}
type alias Lesson =
    { id : String
    , title : String
    , blurb : String
    , cells : List ( CellKind, String )
    }


{-| A proposed next step: a short label, a sentence of detail, and the cell it would add. -}
type alias Suggestion =
    { label : String
    , detail : String
    , kind : CellKind
    , source : String
    }



-- STARTER & LESSONS ----------------------------------------------------------


{-| The notebook the site opens with: a short, self-explaining tour ending on a dataset, so the
suggestion panel immediately has something to work with.
-}
starter : List ( CellKind, String )
starter =
    [ ( Markdown, "# Welcome to elm-notebook\n\nA Jupyter-style notebook for **data exploration in real Elm**. Each cell is either a note (like this) or a line of Elm the kernel runs. Edit a cell and press **Run** — names defined in one cell are visible in the next, just like a notebook kernel.\n\nStart anywhere, then follow the **suggested next steps** on the right." )
    , ( Code, "-- a code cell is one Elm expression\nList.range 1 10" )
    , ( Code, "-- `name = expr` publishes a value to every later cell\nnumbers = List.range 1 10" )
    , ( Code, "List.sum numbers" )
    , ( Markdown, "## A little dataset\n\nA *table* is just a `List` of records. The kernel renders it as a grid." )
    , ( Code, "people =\n    [ { name = \"Ada\", dept = \"Eng\", salary = 95 }\n    , { name = \"Grace\", dept = \"Eng\", salary = 110 }\n    , { name = \"Lin\", dept = \"Design\", salary = 80 }\n    , { name = \"Ravi\", dept = \"Design\", salary = 85 }\n    ]" )
    , ( Code, "-- the last result is `_`; the panel on the right suggests where to go next\nmean (List.map (\\r -> r.salary) people)" )
    ]


{-| The catalogue of guided lessons offered in the launcher. -}
lessons : List Lesson
lessons =
    [ Lesson "values"
        "Values & variables"
        "Numbers, text, booleans, and naming results so later cells can use them."
        [ ( Markdown, "# Values & variables\n\nEvery code cell evaluates **one expression**. A cell `name = expr` *names* its result, publishing it to the kernel so later cells can use it." )
        , ( Code, "1 + 2 * 3" )
        , ( Code, "radius = 5" )
        , ( Code, "area = 3.14159 * radius ^ 2" )
        , ( Code, "\"area is about \" ++ String.fromInt (round area)" )
        , ( Code, "if area > 50 then \"big\" else \"small\"" )
        ]
    , Lesson "lists"
        "Lists & ranges"
        "Build lists, summarise them, slice them, sort them."
        [ ( Markdown, "# Lists & ranges\n\n`List.range a b` builds an inclusive list. Lists are summarised with `List.sum`/`mean`/`List.maximum`/`List.length`, sliced with `List.take`/`List.drop`, reordered with `List.sort`/`List.reverse`." )
        , ( Code, "xs = List.range 1 20" )
        , ( Code, "List.sum xs" )
        , ( Code, "mean xs" )
        , ( Code, "List.take 5 (List.reverse xs)" )
        , ( Code, "List.sort [ 5, 3, 8, 1, 9, 2 ]" )
        ]
    , Lesson "transform"
        "map · filter · fold"
        "The three verbs of data processing, with lambdas and pipes."
        [ ( Markdown, "# map · filter · fold\n\nThe heart of functional data processing:\n\n- `List.map f xs` transforms every element\n- `List.filter pred xs` keeps the elements passing a test\n- `List.foldl f init xs` collapses a list to a single value\n\nA lambda is written `\\x -> …`; the pipe `|>` chains steps left to right." )
        , ( Code, "nums = List.range 1 10" )
        , ( Code, "List.map (\\x -> x * x) nums" )
        , ( Code, "List.filter (\\x -> modBy 2 x == 0) nums" )
        , ( Code, "List.foldl (\\x acc -> x + acc) 0 nums" )
        , ( Code, "nums\n    |> List.filter (\\x -> x > 5)\n    |> List.map (\\x -> x * 10)\n    |> List.sum" )
        ]
    , Lesson "records"
        "Records & tables"
        "Group fields into records; a list of records is a table."
        [ ( Markdown, "# Records & tables\n\nA **record** groups named fields: `{ name = \"Ada\", age = 36 }`, read back with `r.name`. A **list of records is a table** — the kernel draws it as a grid, and `List.sortBy` / `List.map` work over the rows." )
        , ( Code, "ada = { name = \"Ada\", age = 36 }" )
        , ( Code, "ada.name" )
        , ( Code, "cities =\n    [ { city = \"Oslo\", temp = -3 }\n    , { city = \"Cairo\", temp = 22 }\n    , { city = \"Tokyo\", temp = 9 }\n    ]" )
        , ( Code, "List.map (\\r -> r.temp) cities" )
        , ( Code, "List.sortBy (\\r -> r.temp) cities" )
        ]
    , Lesson "analyse"
        "Analysing a dataset"
        "Filter, transform, group and aggregate a small table end to end."
        [ ( Markdown, "# Analysing a dataset\n\nA full mini-analysis: from a table, **filter** rows, pull out a column with `List.map`, **group** by a field, and **aggregate** each group. `groupBy` returns one record per group carrying its `count` and its `items` (a nested table)." )
        , ( Code, "sales =\n    [ { product = \"Pen\", region = \"North\", units = 120 }\n    , { product = \"Pen\", region = \"South\", units = 80 }\n    , { product = \"Mug\", region = \"North\", units = 45 }\n    , { product = \"Mug\", region = \"South\", units = 70 }\n    , { product = \"Notebook\", region = \"North\", units = 200 }\n    ]" )
        , ( Code, "-- only the strong rows\nList.filter (\\r -> r.units > 75) sales" )
        , ( Code, "-- bucket by region (each group keeps its rows)\ngroupBy (\\r -> r.region) sales" )
        , ( Code, "-- the average order size\nmean (List.map (\\r -> r.units) sales)" )
        ]
    , Lesson "decisions"
        "Decisions: if & case"
        "Branch on conditions and pattern-match with case (and Maybe)."
        [ ( Markdown, "# Decisions\n\nElm chooses with `if … then … else …` and, more powerfully, `case … of` pattern matching. `List.head` returns a `Maybe`, matched as `Just x` or `Nothing`." )
        , ( Code, "grade n =\n    if n >= 90 then\n        \"A\"\n\n    else if n >= 80 then\n        \"B\"\n\n    else\n        \"C\"" )
        , ( Code, "List.map grade [ 95, 82, 71 ]" )
        , ( Code, "first = List.head (List.range 10 20)" )
        , ( Code, "case first of\n    Just n ->\n        n * 100\n\n    Nothing ->\n        0" )
        ]
    , Lesson "text"
        "Text & strings"
        "Slice, case, split and join text."
        [ ( Markdown, "# Text & strings\n\nText is processed with `String.toUpper`/`toLower`, `trim`, `split`, `join`, `words`, `contains`, and `++` for concatenation." )
        , ( Code, "phrase = \"  the quick brown fox  \"" )
        , ( Code, "String.trim phrase |> String.toUpper" )
        , ( Code, "String.words (String.trim phrase)" )
        , ( Code, "String.join \"-\" (String.words (String.trim phrase))" )
        , ( Code, "String.contains \"quick\" phrase" )
        ]
    ]



-- SUGGESTIONS ----------------------------------------------------------------


{-| Given the value the notebook last produced, propose insert-ready next steps. -}
suggestNext : Maybe Value -> List Suggestion
suggestNext maybeValue =
    let
        body =
            case maybeValue of
                Nothing ->
                    starterSuggestions

                Just value ->
                    forValue value
    in
    body ++ [ noteSuggestion ]


noteSuggestion : Suggestion
noteSuggestion =
    Suggestion "Add a note"
        "Document what you just found — good notebooks explain themselves."
        Markdown
        "## What I found\n\n…"


starterSuggestions : List Suggestion
starterSuggestions =
    [ Suggestion "Make a list" "A range of numbers to summarise and transform." Code "List.range 1 20"
    , Suggestion "Make a table" "A list of records is rendered as a grid." Code "rows =\n    [ { name = \"A\", value = 10 }\n    , { name = \"B\", value = 25 }\n    , { name = \"C\", value = 17 }\n    ]"
    , Suggestion "Name a value" "Bind a result so later cells can reuse it." Code "x = 42"
    ]


forValue : Value -> List Suggestion
forValue value =
    if Value.isTable value then
        tableSuggestions value

    else
        case value of
            VList items ->
                if not (List.isEmpty items) && List.all isNumber items then
                    numberListSuggestions value

                else
                    listSuggestions

            VNum _ ->
                numberSuggestions

            VStr _ ->
                stringSuggestions

            VRecord fields ->
                recordSuggestions fields

            _ ->
                starterSuggestions


tableSuggestions : Value -> List Suggestion
tableSuggestions value =
    let
        cols =
            Value.tableColumns value

        numCol =
            firstColumnOfType isNumber value cols
                |> Maybe.withDefault (Maybe.withDefault "value" (List.head cols))

        textCol =
            firstColumnOfType isText value cols
                |> Maybe.withDefault (Maybe.withDefault "name" (List.head cols))

        threshold =
            Value.numberToString (toFloat (round (columnMean numCol value)))
    in
    [ Suggestion "Pull out a column"
        ("Get the " ++ numCol ++ " of every row.")
        Code
        ("List.map (\\r -> r." ++ numCol ++ ") _")
    , Suggestion "Average a column"
        ("Mean of the " ++ numCol ++ " column.")
        Code
        ("mean (List.map (\\r -> r." ++ numCol ++ ") _)")
    , Suggestion "Filter rows"
        ("Keep rows whose " ++ numCol ++ " is above " ++ threshold ++ ".")
        Code
        ("List.filter (\\r -> r." ++ numCol ++ " > " ++ threshold ++ ") _")
    , Suggestion "Group by a field"
        ("Bucket the rows by " ++ textCol ++ " (each group carries its count and rows).")
        Code
        ("groupBy (\\r -> r." ++ textCol ++ ") _")
    , Suggestion "Sort the table"
        ("Order the rows by " ++ numCol ++ ".")
        Code
        ("List.sortBy (\\r -> r." ++ numCol ++ ") _")
    ]


numberListSuggestions : Value -> List Suggestion
numberListSuggestions value =
    let
        threshold =
            Value.numberToString (toFloat (round (listMean value)))
    in
    [ Suggestion "Total" "Add the numbers up." Code "List.sum _"
    , Suggestion "Average" "The mean of the list." Code "mean _"
    , Suggestion "Largest" "The maximum value." Code "List.maximum _"
    , Suggestion "Sort" "Order the values ascending." Code "List.sort _"
    , Suggestion "Keep the big ones"
        ("Filter to values above the mean (" ++ threshold ++ ").")
        Code
        ("List.filter (\\x -> x > " ++ threshold ++ ") _")
    , Suggestion "Transform each" "Double every element." Code "List.map (\\x -> x * 2) _"
    ]


listSuggestions : List Suggestion
listSuggestions =
    [ Suggestion "Count" "How many elements?" Code "List.length _"
    , Suggestion "First few" "Take the first three." Code "List.take 3 _"
    , Suggestion "Reverse" "Flip the order." Code "List.reverse _"
    , Suggestion "Unique" "Drop duplicates." Code "unique _"
    ]


numberSuggestions : List Suggestion
numberSuggestions =
    [ Suggestion "Square root" "The square root of the number." Code "sqrt _"
    , Suggestion "Double it" "Multiply by two." Code "_ * 2"
    , Suggestion "Count up to it" "Build a range ending here." Code "List.range 1 (round _)"
    ]


stringSuggestions : List Suggestion
stringSuggestions =
    [ Suggestion "Upper-case" "Shout it." Code "String.toUpper _"
    , Suggestion "Split into words" "Break on spaces." Code "String.words _"
    , Suggestion "Length" "How many characters?" Code "String.length _"
    , Suggestion "Contains?" "Test for a substring." Code "String.contains \"a\" _"
    ]


recordSuggestions : List ( String, Value ) -> List Suggestion
recordSuggestions fields =
    let
        firstKey =
            List.head fields |> Maybe.map Tuple.first |> Maybe.withDefault "name"
    in
    [ Suggestion "Read a field" ("Get the " ++ firstKey ++ " field.") Code ("_." ++ firstKey)
    , Suggestion "Wrap in a list" "Start a one-row table from it." Code "[ _ ]"
    ]



-- value introspection --------------------------------------------------------


isNumber : Value -> Bool
isNumber value =
    case value of
        VNum _ ->
            True

        _ ->
            False


isText : Value -> Bool
isText value =
    case value of
        VStr _ ->
            True

        _ ->
            False


firstRowFields : Value -> List ( String, Value )
firstRowFields value =
    case value of
        VList ((VRecord fields) :: _) ->
            fields

        _ ->
            []


firstColumnOfType : (Value -> Bool) -> Value -> List String -> Maybe String
firstColumnOfType pred value cols =
    let
        fields =
            firstRowFields value

        ofType name =
            case List.filter (\( k, _ ) -> k == name) fields of
                ( _, v ) :: _ ->
                    pred v

                [] ->
                    False
    in
    List.filter ofType cols |> List.head


columnMean : String -> Value -> Float
columnMean name value =
    average (columnNumbers name value)


columnNumbers : String -> Value -> List Float
columnNumbers name value =
    case value of
        VList rows ->
            List.filterMap (rowNumber name) rows

        _ ->
            []


rowNumber : String -> Value -> Maybe Float
rowNumber name row =
    case row of
        VRecord fields ->
            case List.filter (\( k, _ ) -> k == name) fields of
                ( _, VNum n ) :: _ ->
                    Just n

                _ ->
                    Nothing

        _ ->
            Nothing


listMean : Value -> Float
listMean value =
    case value of
        VList items ->
            average (List.filterMap numberOf items)

        _ ->
            0


numberOf : Value -> Maybe Float
numberOf value =
    case value of
        VNum n ->
            Just n

        _ ->
            Nothing


average : List Float -> Float
average xs =
    case xs of
        [] ->
            0

        _ ->
            List.sum xs / toFloat (List.length xs)
