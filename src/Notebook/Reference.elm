module Notebook.Reference exposing (Entry, entries, search)

{-| A small **function reference** for the notebook: the data-exploration prelude plus the most-used
standard-library helpers, each with a one-line description and an insertable snippet. It is the
discovery counterpart to autocomplete — browse or search for "the one that groups rows", click, and
get a runnable template.

@docs Entry, entries, search

-}


{-| One catalog entry: the name, its signature, a one-line description, and the snippet inserted when
it's picked. -}
type alias Entry =
    { name : String, signature : String, doc : String, snippet : String }


{-| Filter the catalog by a query over names and descriptions (empty query → everything). -}
search : String -> List Entry
search query =
    let
        q =
            String.toLower (String.trim query)
    in
    if q == "" then
        entries

    else
        List.filter (\e -> String.contains q (String.toLower (e.name ++ " " ++ e.doc))) entries


{-| The whole catalog. -}
entries : List Entry
entries =
    [ -- summary statistics
      Entry "mean" "List Float -> Float" "Average of a list of numbers." "mean numbers"
    , Entry "median" "List Float -> Float" "Middle value of a list." "median numbers"
    , Entry "stdev" "List Float -> Float" "Population standard deviation." "stdev numbers"
    , Entry "variance" "List Float -> Float" "Population variance." "variance numbers"
    , Entry "describe" "List Float -> Record" "count/mean/min/max/median/stdev at once." "describe numbers"
    , Entry "quantile" "Float -> List Float -> Float" "The p-quantile (p in 0..1)." "quantile 0.5 numbers"
    , Entry "percentile" "Float -> List Float -> Float" "The p-th percentile (p in 0..100)." "percentile 90 numbers"
    , Entry "minOf" "List Float -> Float" "Smallest value (0 if empty)." "minOf numbers"
    , Entry "maxOf" "List Float -> Float" "Largest value (0 if empty)." "maxOf numbers"
    , Entry "spread" "List Float -> Float" "max − min." "spread numbers"

    -- relationships & modelling
    , Entry "corr" "List Float -> List Float -> Float" "Pearson correlation of two columns." "corr xs ys"
    , Entry "cov" "List Float -> List Float -> Float" "Covariance of two columns." "cov xs ys"
    , Entry "linfit" "List Float -> List Float -> Record" "Least-squares line {slope, intercept}." "linfit xs ys"
    , Entry "predict" "Model -> Float -> Float" "Predict y from a linfit model." "predict model x"

    -- grouping & shaping
    , Entry "groupBy" "(a -> b) -> List a -> List Group" "Group rows by a key function." "groupBy (\\r -> r.field) rows"
    , Entry "countBy" "(a -> b) -> List a -> List Record" "Count rows per group." "countBy (\\r -> r.field) rows"
    , Entry "summarize" "(a -> b) -> (a -> Float) -> List a -> List Record" "Group + sum + mean." "summarize (\\r -> r.key) (\\r -> r.value) rows"
    , Entry "unique" "List a -> List a" "Distinct values, order-preserving." "unique items"
    , Entry "normalize" "List Float -> List Float" "Scale a column to 0..1." "normalize numbers"
    , Entry "cumSum" "List Float -> List Float" "Running total." "cumSum numbers"
    , Entry "zip" "List a -> List b -> List ( a, b )" "Pair two lists elementwise." "zip xs ys"
    , Entry "sortDesc" "List comparable -> List comparable" "Sort descending." "sortDesc numbers"

    -- dates (ISO "YYYY-MM-DD" strings)
    , Entry "year" "String -> Int" "The year of an ISO date." "year \"2024-03-15\""
    , Entry "month" "String -> Int" "The month number (1–12) of an ISO date." "month \"2024-03-15\""
    , Entry "day" "String -> Int" "The day of the month of an ISO date." "day \"2024-03-15\""
    , Entry "monthName" "String -> String" "The English month name of an ISO date." "monthName \"2024-03-15\""
    , Entry "weekday" "String -> String" "The weekday name of an ISO date." "weekday \"2024-03-15\""
    , Entry "quarter" "String -> Int" "The calendar quarter (1–4) of an ISO date." "quarter \"2024-03-15\""
    , Entry "daysBetween" "String -> String -> Int" "Whole days from one ISO date to another." "daysBetween \"2024-01-01\" \"2024-03-01\""

    -- joining two tables
    , Entry "lookup" "(a -> k) -> k -> List a -> Maybe a" "The first row whose key matches." "lookup (\\r -> r.id) 3 rows"
    , Entry "joinWith" "(a -> b -> c) -> (a -> k) -> (b -> k) -> List a -> List b -> List c" "Inner-join two tables on a key, combining matches." "joinWith (\\o c -> { name = c.name }) (\\o -> o.cid) (\\c -> c.id) orders customers"
    , Entry "leftJoinWith" "(a -> b -> c) -> (a -> k) -> (b -> k) -> (a -> c) -> List a -> List b -> List c" "Left-join, keeping unmatched left rows via onMiss." "leftJoinWith (\\o c -> c) (\\o -> o.cid) (\\c -> c.id) (\\o -> o) orders customers"

    -- text
    , Entry "splitOn" "String -> String -> List String" "Split a string on a separator." "splitOn \",\" text"
    , Entry "extractNumbers" "String -> List Float" "Pull every number out of a string." "extractNumbers text"
    , Entry "wordCount" "String -> Int" "How many whitespace-separated words." "wordCount text"
    , Entry "wordFreq" "String -> List Record" "Word → count table (case-insensitive)." "wordFreq text"
    , Entry "titleCase" "String -> String" "Capitalise the first letter of each word." "titleCase text"

    -- window / running calculations
    , Entry "diff" "List Float -> List Float" "Successive differences." "diff numbers"
    , Entry "movingAvg" "Int -> List Float -> List Float" "Moving average over a window of n." "movingAvg 3 numbers"
    , Entry "rank" "List Float -> List Int" "Rank of each value (1 = smallest)." "rank numbers"
    , Entry "cumMax" "List Float -> List Float" "Running maximum." "cumMax numbers"
    , Entry "cumMin" "List Float -> List Float" "Running minimum." "cumMin numbers"

    -- function plots
    , Entry "linspace" "Float -> Float -> Int -> List Float" "n points from lo to hi." "linspace 0 10 50"
    , Entry "plot" "(Float -> Float) -> Float -> Float -> List Float" "Sample a function over a range." "plot (\\x -> x * x) 0 10"
    , Entry "plotPoints" "(Float -> Float) -> Float -> Float -> List Record" "{x, y} points of a function." "plotPoints (\\x -> x * x) 0 10"

    -- list library
    , Entry "List.map" "(a -> b) -> List a -> List b" "Transform every element." "List.map (\\x -> x) xs"
    , Entry "List.filter" "(a -> Bool) -> List a -> List a" "Keep elements that pass." "List.filter (\\x -> True) xs"
    , Entry "List.foldl" "(a -> b -> b) -> b -> List a -> b" "Reduce from the left." "List.foldl (\\x acc -> acc) init xs"
    , Entry "List.sum" "List number -> number" "Total of a list." "List.sum xs"
    , Entry "List.sortBy" "(a -> comparable) -> List a -> List a" "Sort by a derived key." "List.sortBy (\\r -> r.field) rows"
    , Entry "List.range" "Int -> Int -> List Int" "Integers from lo to hi." "List.range 1 10"
    , Entry "List.map2" "(a -> b -> c) -> List a -> List b -> List c" "Zip-with two lists." "List.map2 (\\a b -> a) xs ys"

    -- string library
    , Entry "String.split" "String -> String -> List String" "Split on a separator." "String.split \",\" text"
    , Entry "String.join" "String -> List String -> String" "Join with a separator." "String.join \", \" parts"
    , Entry "String.contains" "String -> String -> Bool" "Substring test." "String.contains \"x\" text"
    ]
