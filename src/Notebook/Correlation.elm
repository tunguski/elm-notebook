module Notebook.Correlation exposing (Matrix, matrix)

{-| The **correlation matrix** of a table's numeric columns: the Pearson correlation coefficient
between every pair, in [-1, 1] (or `Nothing` where a column has no variance). The host renders it as
a colour-graded grid — the quick "what moves with what?" view over a dataset.

@docs Matrix, matrix

-}

import Lang exposing (Value(..))
import Notebook.Value as Value


{-| The square matrix: the numeric column names and, for each, its correlation with every column
(`Nothing` when undefined). -}
type alias Matrix =
    { columns : List String, rows : List (List (Maybe Float)) }


{-| The correlation matrix of a table value's numeric columns (empty for non-tables / no numerics). -}
matrix : Value -> Matrix
matrix value =
    let
        cols =
            numericColumns value

        seriesOf name =
            List.filterMap (numAt name) (rowsOf value)

        seriesByCol =
            List.map (\c -> ( c, seriesOf c )) cols

        lookup name =
            seriesByCol |> List.filter (\( c, _ ) -> c == name) |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault []

        rowFor a =
            List.map (\b -> pearson (lookup a) (lookup b)) cols
    in
    { columns = cols, rows = List.map rowFor cols }


pearson : List Float -> List Float -> Maybe Float
pearson xs ys =
    let
        n =
            toFloat (List.length xs)
    in
    if n == 0 || List.length xs /= List.length ys then
        Nothing

    else
        let
            mx =
                List.sum xs / n

            my =
                List.sum ys / n

            cov =
                List.sum (List.map2 (\x y -> (x - mx) * (y - my)) xs ys)

            sx =
                sqrt (List.sum (List.map (\x -> (x - mx) * (x - mx)) xs))

            sy =
                sqrt (List.sum (List.map (\y -> (y - my) * (y - my)) ys))
        in
        if sx == 0 || sy == 0 then
            Nothing

        else
            Just (cov / (sx * sy))


numericColumns : Value -> List String
numericColumns value =
    List.filter (columnIsNum value) (Value.tableColumns value)


columnIsNum : Value -> String -> Bool
columnIsNum value name =
    case rowsOf value of
        first :: _ ->
            case Value.fieldOf name first of
                Just (VNum _) ->
                    True

                _ ->
                    False

        [] ->
            False


numAt : String -> Value -> Maybe Float
numAt name record =
    case Value.fieldOf name record of
        Just (VNum n) ->
            Just n

        _ ->
            Nothing


rowsOf : Value -> List Value
rowsOf value =
    case value of
        VList items ->
            items

        _ ->
            []
