module Notebook.Parser exposing (parse, parseCell)

{-| A small precedence-climbing parser turning notebook source into an [`Ast.Expr`](Notebook-Ast#Expr).

The grammar is a friendly subset of Elm: literals, lists `[…]`, records `{ k = v }`,
field access `r.field`, lambdas `\x y -> …`, function application by juxtaposition,
the usual arithmetic/comparison/boolean operators plus `++`, the pipes `|>`/`<|`,
`if`/`then`/`else` and `let … in` (bindings separated by `;`).

@docs parse, parseCell

-}

import Notebook.Ast exposing (CellForm(..), Expr(..))
import Notebook.Lexer as Lexer exposing (Token(..))


{-| Parse a whole cell: a top-level `name = expr` binding, or a bare expression. -}
parseCell : String -> Result String CellForm
parseCell src =
    Lexer.tokenize src
        |> Result.andThen
            (\tokens ->
                case tokens of
                    (TLower name) :: (TOp "=") :: rest ->
                        parseExpr 0 rest
                            |> Result.andThen (expectEnd (CBind name))

                    [] ->
                        Err "empty cell"

                    _ ->
                        parseExpr 0 tokens
                            |> Result.andThen (expectEnd CBare)
            )


{-| Parse a source string as a single expression. -}
parse : String -> Result String Expr
parse src =
    Lexer.tokenize src
        |> Result.andThen (parseExpr 0)
        |> Result.andThen (expectEnd identity)


expectEnd : (a -> b) -> ( a, List Token ) -> Result String b
expectEnd wrap ( value, rest ) =
    case rest of
        [] ->
            Ok (wrap value)

        tok :: _ ->
            Err ("unexpected `" ++ Lexer.show tok ++ "` after expression")



-- EXPRESSIONS (precedence climbing) ------------------------------------------


parseExpr : Int -> List Token -> Result String ( Expr, List Token )
parseExpr minPrec tokens =
    parsePrefix tokens
        |> Result.andThen (\( left, rest ) -> binopLoop minPrec left rest)


binopLoop : Int -> Expr -> List Token -> Result String ( Expr, List Token )
binopLoop minPrec left tokens =
    case tokens of
        tok :: rest ->
            case binopInfo tok of
                Just ( op, prec, rightAssoc ) ->
                    if prec >= minPrec then
                        let
                            nextMin =
                                if rightAssoc then
                                    prec

                                else
                                    prec + 1
                        in
                        parseExpr nextMin rest
                            |> Result.andThen
                                (\( right, rest2 ) ->
                                    binopLoop minPrec (combine op left right) rest2
                                )

                    else
                        Ok ( left, tokens )

                Nothing ->
                    Ok ( left, tokens )

        [] ->
            Ok ( left, tokens )


combine : String -> Expr -> Expr -> Expr
combine op left right =
    case op of
        "|>" ->
            EApply right left

        "<|" ->
            EApply left right

        _ ->
            EBinop op left right


{-| (precedence, isRightAssociative) for each infix operator. Higher binds tighter. -}
binopInfo : Token -> Maybe ( String, Int, Bool )
binopInfo tok =
    case tok of
        TOp "<|" ->
            Just ( "<|", 1, True )

        TOp "|>" ->
            Just ( "|>", 2, False )

        TOp "||" ->
            Just ( "||", 3, True )

        TOp "&&" ->
            Just ( "&&", 4, True )

        TOp "==" ->
            Just ( "==", 5, False )

        TOp "/=" ->
            Just ( "/=", 5, False )

        TOp "<" ->
            Just ( "<", 5, False )

        TOp ">" ->
            Just ( ">", 5, False )

        TOp "<=" ->
            Just ( "<=", 5, False )

        TOp ">=" ->
            Just ( ">=", 5, False )

        TOp "++" ->
            Just ( "++", 6, True )

        TOp "+" ->
            Just ( "+", 7, False )

        TOp "-" ->
            Just ( "-", 7, False )

        TOp "*" ->
            Just ( "*", 8, False )

        TOp "/" ->
            Just ( "/", 8, False )

        TOp "^" ->
            Just ( "^", 9, True )

        _ ->
            Nothing



-- PREFIX FORMS: if / let / lambda / unary minus ------------------------------


parsePrefix : List Token -> Result String ( Expr, List Token )
parsePrefix tokens =
    case tokens of
        (TKw "if") :: rest ->
            parseIf rest

        (TKw "let") :: rest ->
            parseLet rest

        (TOp "\\") :: rest ->
            parseLambda rest

        (TOp "-") :: rest ->
            parseApp rest
                |> Result.map (\( e, r ) -> ( ENeg e, r ))

        _ ->
            parseApp tokens


parseIf : List Token -> Result String ( Expr, List Token )
parseIf tokens =
    parseExpr 0 tokens
        |> Result.andThen
            (\( cond, afterCond ) ->
                expect (TKw "then") afterCond
                    |> Result.andThen (parseExpr 0)
                    |> Result.andThen
                        (\( thenE, afterThen ) ->
                            expect (TKw "else") afterThen
                                |> Result.andThen (parseExpr 0)
                                |> Result.map
                                    (\( elseE, afterElse ) ->
                                        ( EIf cond thenE elseE, afterElse )
                                    )
                        )
            )


parseLambda : List Token -> Result String ( Expr, List Token )
parseLambda tokens =
    parseParams tokens []
        |> Result.andThen
            (\( params, afterArrow ) ->
                parseExpr 0 afterArrow
                    |> Result.map
                        (\( body, rest ) ->
                            ( List.foldr ELambda body params, rest )
                        )
            )


parseParams : List Token -> List String -> Result String ( List String, List Token )
parseParams tokens acc =
    case tokens of
        (TLower name) :: rest ->
            parseParams rest (name :: acc)

        (TOp "->") :: rest ->
            if List.isEmpty acc then
                Err "lambda needs at least one parameter before `->`"

            else
                Ok ( List.reverse acc, rest )

        tok :: _ ->
            Err ("expected a parameter or `->` in lambda, found `" ++ Lexer.show tok ++ "`")

        [] ->
            Err "unfinished lambda"


parseLet : List Token -> Result String ( Expr, List Token )
parseLet tokens =
    parseBindings tokens []
        |> Result.andThen
            (\( binds, afterIn ) ->
                parseExpr 0 afterIn
                    |> Result.map (\( body, rest ) -> ( ELet binds body, rest ))
            )


parseBindings : List Token -> List ( String, Expr ) -> Result String ( List ( String, Expr ), List Token )
parseBindings tokens acc =
    case tokens of
        (TLower name) :: (TOp "=") :: rest ->
            parseExpr 0 rest
                |> Result.andThen
                    (\( value, afterValue ) ->
                        case afterValue of
                            (TOp ";") :: more ->
                                parseBindings more (( name, value ) :: acc)

                            (TKw "in") :: more ->
                                Ok ( List.reverse (( name, value ) :: acc), more )

                            tok :: _ ->
                                Err ("expected `;` or `in` in let, found `" ++ Lexer.show tok ++ "`")

                            [] ->
                                Err "let is missing its `in`"
                    )

        tok :: _ ->
            Err ("expected a `name = …` binding in let, found `" ++ Lexer.show tok ++ "`")

        [] ->
            Err "empty let"



-- APPLICATION & ATOMS --------------------------------------------------------


parseApp : List Token -> Result String ( Expr, List Token )
parseApp tokens =
    parseAtom tokens
        |> Result.andThen (\( fn, rest ) -> appLoop fn rest)


appLoop : Expr -> List Token -> Result String ( Expr, List Token )
appLoop fn tokens =
    if startsAtom tokens then
        parseAtom tokens
            |> Result.andThen (\( arg, rest ) -> appLoop (EApply fn arg) rest)

    else
        Ok ( fn, tokens )


startsAtom : List Token -> Bool
startsAtom tokens =
    case tokens of
        tok :: _ ->
            case tok of
                TNum _ ->
                    True

                TStr _ ->
                    True

                TLower _ ->
                    True

                TKw "True" ->
                    True

                TKw "False" ->
                    True

                TOp "(" ->
                    True

                TOp "[" ->
                    True

                TOp "{" ->
                    True

                _ ->
                    False

        [] ->
            False


parseAtom : List Token -> Result String ( Expr, List Token )
parseAtom tokens =
    case tokens of
        (TNum n) :: rest ->
            fieldChain (ENum n) rest

        (TStr s) :: rest ->
            fieldChain (EStr s) rest

        (TKw "True") :: rest ->
            fieldChain (EBool True) rest

        (TKw "False") :: rest ->
            fieldChain (EBool False) rest

        (TLower name) :: rest ->
            fieldChain (EVar name) rest

        (TOp "(") :: rest ->
            parseExpr 0 rest
                |> Result.andThen
                    (\( inner, afterInner ) ->
                        expect (TOp ")") afterInner
                            |> Result.andThen (fieldChain inner)
                    )

        (TOp "[") :: rest ->
            parseList rest

        (TOp "{") :: rest ->
            parseRecord rest

        tok :: _ ->
            Err ("unexpected `" ++ Lexer.show tok ++ "`")

        [] ->
            Err "unexpected end of input"


fieldChain : Expr -> List Token -> Result String ( Expr, List Token )
fieldChain base tokens =
    case tokens of
        (TOp ".") :: (TLower field) :: rest ->
            fieldChain (EField base field) rest

        _ ->
            Ok ( base, tokens )


parseList : List Token -> Result String ( Expr, List Token )
parseList tokens =
    case tokens of
        (TOp "]") :: rest ->
            fieldChain (EList []) rest

        _ ->
            parseCommaSep tokens
                |> Result.andThen
                    (\( items, afterItems ) ->
                        expect (TOp "]") afterItems
                            |> Result.andThen (fieldChain (EList items))
                    )


parseCommaSep : List Token -> Result String ( List Expr, List Token )
parseCommaSep tokens =
    parseExpr 0 tokens
        |> Result.andThen
            (\( first, afterFirst ) ->
                case afterFirst of
                    (TOp ",") :: rest ->
                        parseCommaSep rest
                            |> Result.map (\( more, end ) -> ( first :: more, end ))

                    _ ->
                        Ok ( [ first ], afterFirst )
            )


parseRecord : List Token -> Result String ( Expr, List Token )
parseRecord tokens =
    case tokens of
        (TOp "}") :: rest ->
            fieldChain (ERecord []) rest

        _ ->
            parseFields tokens
                |> Result.andThen
                    (\( fields, afterFields ) ->
                        expect (TOp "}") afterFields
                            |> Result.andThen (fieldChain (ERecord fields))
                    )


parseFields : List Token -> Result String ( List ( String, Expr ), List Token )
parseFields tokens =
    case tokens of
        (TLower name) :: (TOp "=") :: rest ->
            parseExpr 0 rest
                |> Result.andThen
                    (\( value, afterValue ) ->
                        case afterValue of
                            (TOp ",") :: more ->
                                parseFields more
                                    |> Result.map (\( fields, end ) -> ( ( name, value ) :: fields, end ))

                            _ ->
                                Ok ( [ ( name, value ) ], afterValue )
                    )

        tok :: _ ->
            Err ("expected a record field `name = …`, found `" ++ Lexer.show tok ++ "`")

        [] ->
            Err "unfinished record"



-- HELPERS --------------------------------------------------------------------


expect : Token -> List Token -> Result String (List Token)
expect wanted tokens =
    case tokens of
        tok :: rest ->
            if tok == wanted then
                Ok rest

            else
                Err ("expected `" ++ Lexer.show wanted ++ "`, found `" ++ Lexer.show tok ++ "`")

        [] ->
            Err ("expected `" ++ Lexer.show wanted ++ "`, but input ended")
