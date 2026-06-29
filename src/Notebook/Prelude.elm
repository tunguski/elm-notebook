module Notebook.Prelude exposing (source)

{-| A standard prelude for the notebook, written in **real Elm** and loaded as the base
global scope every notebook starts with. The interpreter already provides the full `List`,
`String`, `Dict`, `Maybe`, `Result` and math libraries; this adds the data-exploration
conveniences they lack — summary stats (`mean`/`median`/`stdev`/`describe`), correlation and a
least-squares `linfit`/`predict`, `quantile`/`percentile`, `cumSum`/`normalize`/`zip`, and
group-and-aggregate (`groupBy`/`countBy`/`summarize`) — so a cell can write `mean (List.map .salary
people)`, `summarize .dept .salary people` or `linfit xs ys` directly.

Because it is ordinary Elm, every name here is also something the learner can read and imitate.

@docs source

-}


{-| The prelude source, parsed into the kernel's base `Globals`. -}
source : String
source =
    """
mean numbers =
    List.sum numbers / toFloat (List.length numbers)


total numbers =
    List.sum numbers


unique items =
    List.foldl
        (\\x seen -> if List.member x seen then seen else seen ++ [ x ])
        []
        items


nth i xs =
    case List.head (List.drop i xs) of
        Just v ->
            v

        Nothing ->
            0


median numbers =
    let
        sorted =
            List.sort numbers

        size =
            List.length numbers

        mid =
            size // 2
    in
    if size == 0 then
        0

    else if modBy 2 size == 1 then
        nth mid sorted

    else
        (nth (mid - 1) sorted + nth mid sorted) / 2


stdev numbers =
    let
        m =
            mean numbers

        size =
            List.length numbers
    in
    if size == 0 then
        0

    else
        sqrt (List.sum (List.map (\\x -> (x - m) * (x - m)) numbers) / toFloat size)


describe numbers =
    let
        sorted =
            List.sort numbers
    in
    { count = List.length numbers
    , mean = mean numbers
    , min = nth 0 sorted
    , max = nth (List.length numbers - 1) sorted
    , median = median numbers
    , stdev = stdev numbers
    }


groupBy keyFn items =
    List.foldl (\\x groups -> groupAdd (keyFn x) x groups) [] items


groupAdd key x groups =
    case groups of
        [] ->
            [ { key = key, count = 1, items = [ x ] } ]

        g :: rest ->
            if g.key == key then
                { key = g.key, count = g.count + 1, items = g.items ++ [ x ] } :: rest

            else
                g :: groupAdd key x rest


minOf numbers =
    case List.minimum numbers of
        Just v ->
            v

        Nothing ->
            0


maxOf numbers =
    case List.maximum numbers of
        Just v ->
            v

        Nothing ->
            0


spread numbers =
    maxOf numbers - minOf numbers


variance numbers =
    let
        m =
            mean numbers

        size =
            List.length numbers
    in
    if size == 0 then
        0

    else
        List.sum (List.map (\\x -> (x - m) * (x - m)) numbers) / toFloat size


cumScan running numbers =
    case numbers of
        [] ->
            []

        x :: rest ->
            (running + x) :: cumScan (running + x) rest


cumSum numbers =
    cumScan 0 numbers


normalize numbers =
    let
        lo =
            minOf numbers

        hi =
            maxOf numbers

        span =
            hi - lo
    in
    if span == 0 then
        List.map (\\value -> value - value) numbers

    else
        List.map (\\x -> (x - lo) / span) numbers


zip xs ys =
    List.map2 (\\x y -> ( x, y )) xs ys


cov xs ys =
    let
        mx =
            mean xs

        my =
            mean ys

        n =
            toFloat (List.length xs)
    in
    if n == 0 then
        0

    else
        List.sum (List.map2 (\\x y -> (x - mx) * (y - my)) xs ys) / n


corr xs ys =
    let
        sx =
            stdev xs

        sy =
            stdev ys
    in
    if sx == 0 || sy == 0 then
        0

    else
        cov xs ys / (sx * sy)


linfit xs ys =
    let
        mx =
            mean xs

        my =
            mean ys

        sxx =
            List.sum (List.map (\\x -> (x - mx) * (x - mx)) xs)

        sxy =
            List.sum (List.map2 (\\x y -> (x - mx) * (y - my)) xs ys)

        slope =
            if sxx == 0 then
                0

            else
                sxy / sxx
    in
    { slope = slope, intercept = my - slope * mx }


predict model x =
    model.intercept + model.slope * x


quantileAt sorted size p =
    let
        pos =
            p * toFloat (size - 1)

        lo =
            floor pos

        hi =
            ceiling pos
    in
    nth lo sorted + (nth hi sorted - nth lo sorted) * (pos - toFloat lo)


quantile p numbers =
    if List.length numbers == 0 then
        0

    else
        quantileAt (List.sort numbers) (List.length numbers) p


percentile p numbers =
    quantile (p / 100) numbers


sortDesc numbers =
    List.reverse (List.sort numbers)


countBy keyFn items =
    List.map (\\g -> { key = g.key, count = g.count }) (groupBy keyFn items)


sumPut k v groups =
    case groups of
        [] ->
            [ { key = k, count = 1, sum = v } ]

        g :: rest ->
            if g.key == k then
                { key = g.key, count = g.count + 1, sum = g.sum + v } :: rest

            else
                g :: sumPut k v rest


summarize keyFn valFn items =
    let
        grouped =
            List.foldl (\\x groups -> sumPut (keyFn x) (valFn x) groups) [] items
    in
    List.map
        (\\g -> { key = g.key, count = g.count, sum = g.sum, mean = g.sum / toFloat g.count })
        grouped


linspaceFrom lo step i n =
    if i >= n then
        []

    else
        (lo + step * toFloat i) :: linspaceFrom lo step (i + 1) n


linspace lo hi n =
    if n <= 1 then
        [ lo ]

    else
        linspaceFrom lo ((hi - lo) / toFloat (n - 1)) 0 n


plot f lo hi =
    List.map f (linspace lo hi 50)


plotPoints f lo hi =
    List.map (\\x -> { x = x, y = f x }) (linspace lo hi 50)


datePart i s =
    case List.head (List.drop i (String.split "-" s)) of
        Just part ->
            Maybe.withDefault 0 (String.toInt (String.trim part))

        Nothing ->
            0


year s =
    datePart 0 s


month s =
    datePart 1 s


day s =
    datePart 2 s


monthNames =
    [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ]


monthName s =
    nth (month s - 1) monthNames


quarter s =
    (month s - 1) // 3 + 1


julianDay s =
    let
        y =
            year s

        m =
            month s

        a =
            (14 - m) // 12

        yy =
            y + 4800 - a

        mm =
            m + 12 * a - 3
    in
    day s + (153 * mm + 2) // 5 + 365 * yy + yy // 4 - yy // 100 + yy // 400 - 32045


daysBetween a b =
    julianDay b - julianDay a


weekdayNames =
    [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]


weekday s =
    nth (modBy 7 (julianDay s)) weekdayNames


lookup keyFn key rows =
    case rows of
        [] ->
            Nothing

        r :: rest ->
            if keyFn r == key then
                Just r

            else
                lookup keyFn key rest


joinWith combine keyA keyB left right =
    List.concatMap
        (\\a -> List.filterMap (\\b -> if keyA a == keyB b then Just (combine a b) else Nothing) right)
        left


leftJoinRow combine keyA keyB onMiss right a =
    case lookup keyB (keyA a) right of
        Just b ->
            combine a b

        Nothing ->
            onMiss a


leftJoinWith combine keyA keyB onMiss left right =
    List.map (leftJoinRow combine keyA keyB onMiss right) left


splitOn separator s =
    String.split separator s


isNumChar c =
    Char.isDigit c || c == '.' || c == '-'


numericRuns s =
    String.words (String.map (\\c -> if isNumChar c then c else ' ') s)


extractNumbers s =
    List.filterMap String.toFloat (numericRuns s)


wordCount s =
    List.length (String.words s)


capWord w =
    String.toUpper (String.left 1 w) ++ String.dropLeft 1 w


titleCase s =
    String.join " " (List.map capWord (String.words s))


wordFreq s =
    List.map (\\g -> { word = g.key, count = g.count }) (groupBy (\\w -> w) (String.words (String.toLower s)))


diff numbers =
    List.map2 (\\a b -> b - a) numbers (List.drop 1 numbers)


movingAvg n numbers =
    if List.length numbers < n then
        []

    else
        mean (List.take n numbers) :: movingAvg n (List.drop 1 numbers)


rank numbers =
    List.map (\\x -> 1 + List.length (List.filter (\\y -> y < x) numbers)) numbers


cumMaxFrom running numbers =
    case numbers of
        [] ->
            []

        x :: rest ->
            max x running :: cumMaxFrom (max x running) rest


cumMax numbers =
    cumMaxFrom (-1.0e308) numbers


cumMinFrom running numbers =
    case numbers of
        [] ->
            []

        x :: rest ->
            min x running :: cumMinFrom (min x running) rest


cumMin numbers =
    cumMinFrom (1.0e308) numbers


standardize numbers =
    let
        m =
            mean numbers

        sd =
            stdev numbers
    in
    List.map (\\x -> if sd == 0 then 0 else (x - m) / sd) numbers


zscore x numbers =
    if stdev numbers == 0 then
        0

    else
        (x - mean numbers) / stdev numbers


iqr numbers =
    quantile 0.75 numbers - quantile 0.25 numbers


outliers numbers =
    let
        q1 =
            quantile 0.25 numbers

        q3 =
            quantile 0.75 numbers

        lo =
            q1 - 1.5 * (q3 - q1)

        hi =
            q3 + 1.5 * (q3 - q1)
    in
    List.filter (\\x -> x < lo || x > hi) numbers


rsquared xs ys =
    corr xs ys * corr xs ys


mode numbers =
    let
        grouped =
            groupBy (\\x -> x) numbers

        best =
            List.foldl (\\g acc -> if g.count > acc.count then g else acc) { key = 0, count = 0, items = [] } grouped
    in
    best.key
"""
