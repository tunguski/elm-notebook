module Notebook.Lexer exposing (Token(..), tokenize, show)

{-| A small hand-written tokenizer for the notebook expression language.

It turns source text into a flat list of [`Token`](#Token)s — numbers, strings,
identifiers, keywords and operators/punctuation — which [`Notebook.Parser`](Notebook-Parser)
then folds into an AST. `--` line comments are skipped so cells can be annotated.

@docs Token, tokenize, show

-}


{-| A lexical token. Punctuation and operators are all carried as `TOp` with their literal
symbol; keywords (`if then else let in True False`) are `TKw`.
-}
type Token
    = TNum Float
    | TStr String
    | TLower String
    | TUpper String
    | TKw String
    | TOp String


keywords : List String
keywords =
    [ "if", "then", "else", "let", "in", "True", "False" ]


{-| Two-character operators, matched before single characters. -}
twoCharOps : List String
twoCharOps =
    [ "->", "==", "/=", "<=", ">=", "++", "&&", "||", "|>", "<|" ]


{-| Tokenize a source string, or fail with a message. -}
tokenize : String -> Result String (List Token)
tokenize src =
    lex (String.toList src) []


lex : List Char -> List Token -> Result String (List Token)
lex chars acc =
    case chars of
        [] ->
            Ok (List.reverse acc)

        c :: rest ->
            if c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' then
                lex rest acc

            else if c == '-' && peekDash rest then
                -- `--` line comment: drop to end of line.
                lex (dropLine rest) acc

            else if c == '"' then
                case lexString rest [] of
                    Ok ( str, after ) ->
                        lex after (TStr str :: acc)

                    Err e ->
                        Err e

            else if Char.isDigit c then
                let
                    ( numStr, after ) =
                        spanNumber (c :: rest)
                in
                case String.toFloat numStr of
                    Just n ->
                        lex after (TNum n :: acc)

                    Nothing ->
                        Err ("bad number: " ++ numStr)

            else if isIdentStart c then
                let
                    ( name, after ) =
                        spanIdent (c :: rest)
                in
                lex after (classifyIdent name :: acc)

            else
                case matchOp (c :: rest) of
                    Just ( op, after ) ->
                        lex after (TOp op :: acc)

                    Nothing ->
                        Err ("unexpected character: '" ++ String.fromChar c ++ "'")


peekDash : List Char -> Bool
peekDash chars =
    case chars of
        d :: _ ->
            d == '-'

        _ ->
            False


dropLine : List Char -> List Char
dropLine chars =
    case chars of
        [] ->
            []

        '\n' :: rest ->
            rest

        _ :: rest ->
            dropLine rest


lexString : List Char -> List Char -> Result String ( String, List Char )
lexString chars acc =
    case chars of
        [] ->
            Err "unterminated string literal"

        '"' :: rest ->
            Ok ( String.fromList (List.reverse acc), rest )

        '\\' :: e :: rest ->
            let
                decoded =
                    case e of
                        'n' ->
                            '\n'

                        't' ->
                            '\t'

                        '"' ->
                            '"'

                        '\\' ->
                            '\\'

                        other ->
                            other
            in
            lexString rest (decoded :: acc)

        c :: rest ->
            lexString rest (c :: acc)


spanNumber : List Char -> ( String, List Char )
spanNumber chars =
    let
        ( intPart, afterInt ) =
            spanWhile Char.isDigit chars
    in
    case afterInt of
        '.' :: d :: rest ->
            if Char.isDigit d then
                let
                    ( fracPart, after ) =
                        spanWhile Char.isDigit (d :: rest)
                in
                ( String.fromList intPart ++ "." ++ String.fromList fracPart, after )

            else
                ( String.fromList intPart, afterInt )

        _ ->
            ( String.fromList intPart, afterInt )


spanIdent : List Char -> ( String, List Char )
spanIdent chars =
    let
        ( name, after ) =
            spanWhile isIdentPart chars
    in
    ( String.fromList name, after )


spanWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
spanWhile pred chars =
    case chars of
        c :: rest ->
            if pred c then
                let
                    ( taken, remaining ) =
                        spanWhile pred rest
                in
                ( c :: taken, remaining )

            else
                ( [], chars )

        [] ->
            ( [], [] )


isIdentStart : Char -> Bool
isIdentStart c =
    Char.isAlpha c || c == '_'


isIdentPart : Char -> Bool
isIdentPart c =
    Char.isAlphaNum c || c == '_'


classifyIdent : String -> Token
classifyIdent name =
    if List.member name keywords then
        TKw name

    else
        case String.uncons name of
            Just ( first, _ ) ->
                if Char.isUpper first then
                    TUpper name

                else
                    TLower name

            Nothing ->
                TLower name


matchOp : List Char -> Maybe ( String, List Char )
matchOp chars =
    case chars of
        a :: b :: rest ->
            let
                two =
                    String.fromList [ a, b ]
            in
            if List.member two twoCharOps then
                Just ( two, rest )

            else
                matchSingle (a :: b :: rest)

        _ ->
            matchSingle chars


singleCharOps : String
singleCharOps =
    "+-*/^=<>.,;()[]{}\\|"


matchSingle : List Char -> Maybe ( String, List Char )
matchSingle chars =
    case chars of
        c :: rest ->
            if String.contains (String.fromChar c) singleCharOps then
                Just ( String.fromChar c, rest )

            else
                Nothing

        [] ->
            Nothing


{-| Render a token for error messages. -}
show : Token -> String
show tok =
    case tok of
        TNum n ->
            String.fromFloat n

        TStr s ->
            "\"" ++ s ++ "\""

        TLower s ->
            s

        TUpper s ->
            s

        TKw s ->
            s

        TOp s ->
            s
