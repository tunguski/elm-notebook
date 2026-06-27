module Notebook.Suggest exposing
    ( Lesson, Suggestion
    , lessons, starter
    , suggestNext
    )

{-| The bit that makes the notebook a teaching tool: ready-made **lessons** and
**context-aware suggestions** for the next step.

[`lessons`](#lessons) are guided notebooks (markdown explanations interleaved with runnable
code) the user can load with one click. [`suggestNext`](#suggestNext) looks at the value the
last cell produced and proposes concrete, insert-ready next steps — average this column,
group by that category, filter these rows — so exploring the data is a matter of picking the
next move rather than knowing the whole API up front. Suggestions reference `_`, the kernel's
binding for the most recent result.

@docs Lesson, Suggestion
@docs lessons, starter
@docs suggestNext

-}

import Notebook.Cell exposing (CellKind(..))
import Notebook.Value as Value exposing (Value(..))


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



-- LESSONS --------------------------------------------------------------------


{-| The notebook the site opens with: a tiny, self-explaining tour ending on a dataset, so
the suggestion panel immediately has something to work with.
-}
starter : List ( CellKind, String )
starter =
    [ ( Markdown, "# Welcome to elm-notebook\n\nA Jupyter-style notebook for **data exploration in Elm**. Each cell below is either a note (like this one) or a line of code the kernel runs. Edit a cell and press **Run** — values defined in one cell are visible in the next, just like a real notebook kernel.\n\nStart anywhere, then follow the **suggested next steps** on the right." )
    , ( Code, "-- a code cell is one expression; this one builds a list\nnumbers = range 1 10" )
    , ( Code, "-- `numbers` is now in scope for every cell below\nsum numbers" )
    , ( Markdown, "## A little dataset\n\nA *table* is just a list of records. The kernel renders it as a grid." )
    , ( Code, "people =\n  [ { name = \"Ada\",   dept = \"Eng\",    salary = 95 }\n  , { name = \"Grace\", dept = \"Eng\",    salary = 110 }\n  , { name = \"Lin\",   dept = \"Design\", salary = 80 }\n  , { name = \"Ravi\",  dept = \"Design\", salary = 85 }\n  ]" )
    , ( Code, "-- the result of `people` is now `_`; the panel on the right suggests where to go next\nmean (column \"salary\" people)" )
    ]


{-| The catalogue of guided lessons offered in the launcher. -}
lessons : List Lesson
lessons =
    [ Lesson "values"
        "Values & variables"
        "Numbers, text, booleans, and naming results so later cells can use them."
        [ ( Markdown, "# Values & variables\n\nEvery code cell evaluates **one expression**. A cell of the form `name = expr` *names* its result, publishing it to the kernel so every later cell can use it." )
        , ( Code, "1 + 2 * 3" )
        , ( Code, "radius = 5" )
        , ( Code, "area = pi * radius ^ 2" )
        , ( Code, "-- text joins with ++, and `_` is always the previous result\n\"area is about \" ++ toText (round area)" )
        , ( Code, "if area > 50 then \"big\" else \"small\"" )
        ]
    , Lesson "lists"
        "Lists & ranges"
        "Build lists, summarise them, slice them, sort them."
        [ ( Markdown, "# Lists & ranges\n\n`range a b` builds an inclusive list. Lists are summarised with `sum`, `mean`, `maximum`, `length`, sliced with `take`/`drop`, and reordered with `sort`/`reverse`." )
        , ( Code, "xs = range 1 20" )
        , ( Code, "sum xs" )
        , ( Code, "mean xs" )
        , ( Code, "take 5 (reverse xs)" )
        , ( Code, "sort [5, 3, 8, 1, 9, 2]" )
        ]
    , Lesson "transform"
        "map · filter · fold"
        "The three verbs of data processing, with lambdas."
        [ ( Markdown, "# map · filter · fold\n\nThese three higher-order functions are the heart of functional data processing.\n\n- `map f xs` transforms every element\n- `filter pred xs` keeps the elements passing a test\n- `foldl f init xs` collapses a list to a single value\n\nA lambda is written `\\x -> …`." )
        , ( Code, "nums = range 1 10" )
        , ( Code, "map (\\x -> x * x) nums" )
        , ( Code, "filter (\\x -> mod x 2 == 0) nums" )
        , ( Code, "foldl (\\x acc -> x + acc) 0 nums" )
        , ( Code, "-- piped, left to right\nnums |> filter (\\x -> x > 5) |> map (\\x -> x * 10) |> sum" )
        ]
    , Lesson "records"
        "Records & tables"
        "Group fields into records; a list of records is a table."
        [ ( Markdown, "# Records & tables\n\nA **record** groups named fields: `{ name = \"Ada\", age = 36 }`, read back with `r.name`. A **list of records is a table** — the kernel draws it as a grid." )
        , ( Code, "ada = { name = \"Ada\", age = 36 }" )
        , ( Code, "ada.name" )
        , ( Code, "table =\n  [ { city = \"Oslo\",   temp = -3 }\n  , { city = \"Cairo\",  temp = 22 }\n  , { city = \"Tokyo\",  temp = 9 }\n  ]" )
        , ( Code, "column \"temp\" table" )
        , ( Code, "sortByField \"temp\" table" )
        ]
    , Lesson "analyse"
        "Analysing a dataset"
        "Filter, project, group and aggregate a small table end to end."
        [ ( Markdown, "# Analysing a dataset\n\nA full mini-analysis: start from a table, then **filter** rows, **select** columns, **group** by a category and **aggregate** each group." )
        , ( Code, "sales =\n  [ { product = \"Pen\",    region = \"North\", units = 120 }\n  , { product = \"Pen\",    region = \"South\", units = 80 }\n  , { product = \"Mug\",    region = \"North\", units = 45 }\n  , { product = \"Mug\",    region = \"South\", units = 70 }\n  , { product = \"Notebook\", region = \"North\", units = 200 }\n  ]" )
        , ( Code, "-- only the strong rows\nfilter (\\row -> row.units > 75) sales" )
        , ( Code, "-- total units per region\ngroupBy \"region\" sales" )
        , ( Code, "-- the average order size\nmean (column \"units\" sales)" )
        ]
    , Lesson "text"
        "Text & strings"
        "Slice, case, split and join text."
        [ ( Markdown, "# Text & strings\n\nText is processed with `toUpper`/`toLower`, `trim`, `split`, `join`, `words`, `contains` and `++` for concatenation." )
        , ( Code, "phrase = \"  the quick brown fox  \"" )
        , ( Code, "trim phrase |> toUpper" )
        , ( Code, "words (trim phrase)" )
        , ( Code, "join \"-\" (words (trim phrase))" )
        , ( Code, "contains \"quick\" phrase" )
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
        "Document what you just learned — good notebooks explain themselves."
        Markdown
        "## What I found\n\n…"


starterSuggestions : List Suggestion
starterSuggestions =
    [ Suggestion "Make a list"
        "A range of numbers to summarise and transform."
        Code
        "range 1 20"
    , Suggestion "Make a table"
        "A list of records is rendered as a grid."
        Code
        "rows =\n  [ { name = \"A\", value = 10 }\n  , { name = \"B\", value = 25 }\n  , { name = \"C\", value = 17 }\n  ]"
    , Suggestion "Name a value"
        "Bind a result so later cells can reuse it."
        Code
        "x = 42"
    ]


forValue : Value -> List Suggestion
forValue value =
    if Value.isTable value then
        tableSuggestions value

    else
        case value of
            VList items ->
                if List.all isNumber items && not (List.isEmpty items) then
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

        firstTwo =
            List.take 2 cols |> List.map (\c -> "\"" ++ c ++ "\"")
    in
    [ Suggestion "Average a column"
        ("Mean of the " ++ numCol ++ " column.")
        Code
        ("mean (column \"" ++ numCol ++ "\" _)")
    , Suggestion "Filter rows"
        ("Keep rows whose " ++ numCol ++ " is above " ++ threshold ++ ".")
        Code
        ("filter (\\row -> row." ++ numCol ++ " > " ++ threshold ++ ") _")
    , Suggestion "Group by category"
        ("Bucket the rows by " ++ textCol ++ " (each group carries its count and rows).")
        Code
        ("groupBy \"" ++ textCol ++ "\" _")
    , Suggestion "Sort the table"
        ("Order the rows by " ++ numCol ++ ".")
        Code
        ("sortByField \"" ++ numCol ++ "\" _")
    , Suggestion "Pick columns"
        "Project the table down to a couple of columns."
        Code
        ("select [" ++ String.join ", " firstTwo ++ "] _")
    ]


numberListSuggestions : Value -> List Suggestion
numberListSuggestions value =
    let
        threshold =
            Value.numberToString (toFloat (round (listMean value)))
    in
    [ Suggestion "Total" "Add the numbers up." Code "sum _"
    , Suggestion "Average" "The mean of the list." Code "mean _"
    , Suggestion "Largest" "The maximum value." Code "maximum _"
    , Suggestion "Sort" "Order the values ascending." Code "sort _"
    , Suggestion "Keep the big ones"
        ("Filter to values above the mean (" ++ threshold ++ ").")
        Code
        ("filter (\\x -> x > " ++ threshold ++ ") _")
    , Suggestion "Transform each" "Double every element." Code "map (\\x -> x * 2) _"
    ]


listSuggestions : List Suggestion
listSuggestions =
    [ Suggestion "Count" "How many elements?" Code "length _"
    , Suggestion "First few" "Take the first three." Code "take 3 _"
    , Suggestion "Reverse" "Flip the order." Code "reverse _"
    , Suggestion "Unique" "Drop duplicates." Code "unique _"
    ]


numberSuggestions : List Suggestion
numberSuggestions =
    [ Suggestion "Square root" "The square root of the number." Code "sqrt _"
    , Suggestion "Double it" "Multiply by two." Code "_ * 2"
    , Suggestion "Count up to it" "Build a range ending here." Code "range 1 (round _)"
    ]


stringSuggestions : List Suggestion
stringSuggestions =
    [ Suggestion "Upper-case" "Shout it." Code "toUpper _"
    , Suggestion "Split into words" "Break on spaces." Code "words _"
    , Suggestion "Length" "How many characters?" Code "length _"
    , Suggestion "Contains?" "Test for a substring." Code "contains \"a\" _"
    ]


recordSuggestions : List ( String, Value ) -> List Suggestion
recordSuggestions fields =
    let
        firstKey =
            List.head fields |> Maybe.map Tuple.first |> Maybe.withDefault "name"
    in
    [ Suggestion "List its fields" "The record's field names." Code "keys _"
    , Suggestion "Read a field" ("Get the " ++ firstKey ++ " field.") Code ("get \"" ++ firstKey ++ "\" _")
    , Suggestion "All the values" "The record's values as a list." Code "values _"
    ]



-- value introspection helpers ------------------------------------------------


isNumber : Value -> Bool
isNumber v =
    case v of
        VNum _ ->
            True

        _ ->
            False


isText : Value -> Bool
isText v =
    case v of
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


columnValues : String -> Value -> List Float
columnValues name value =
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


columnMean : String -> Value -> Float
columnMean name value =
    average (columnValues name value)


listMean : Value -> Float
listMean value =
    case value of
        VList items ->
            average (List.filterMap numberOf items)

        _ ->
            0


numberOf : Value -> Maybe Float
numberOf v =
    case v of
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
