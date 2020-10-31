module Vier.Syntax exposing (..)

import Parser exposing (consumeOne, do, fail, oneOf, oneOrMore, optional, succeed)
import Vier.Lexer.Indent as Indent
import Vier.Lexer.Token as Token exposing (IndentedToken, OpenOrClosed(..), Token, TokenKind(..))


type alias Parser a =
    Parser.Parser IndentedToken (List IndentedToken) a


type Expression
    = Literal String
    | Variable String
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error


parse : List IndentedToken -> Maybe Expression
parse =
    let
        uncons : List a -> Maybe ( a, List a )
        uncons ls =
            case ls of
                head :: tail ->
                    Just ( head, tail )

                [] ->
                    Nothing
    in
    -- TODO remove the drop 1, which is there to remove a spurious newline
    List.drop 1 >> expr uncons >> Maybe.map Tuple.first


tokenKind : Parser TokenKind
tokenKind =
    do consumeOne <| \indentedToken ->
    case indentedToken of
        Indent.Structure structure ->
            fail

        Indent.Content token ->
            succeed token.kind



----
--- Term
--


term : Parser Expression
term =
    do tokenKind <| \kind ->
    case kind of
        Token.NumberLiteral s ->
            succeed <| Literal s

        Token.StringLiteral s ->
            succeed <| Literal s

        Token.Symbol s ->
            succeed <| Variable s

        _ ->
            fail



{- Precedence rules:

   ()

   f a b ------------> function application

   not, risk --------> unary

   ^ ----------------> exp

   * / --------------> multiplicative

   + - ++ -----------> addittive

   >= <= == =/= -----> comparison

   and, or, xor -----> logical

   |> <| >> << ------> pipes

   := += -= /= *= ---> assignments

-}


expr : Parser Expression
expr =
    Parser.expression term
        [ parens
        , functionApplication
        , unops
        , binops [ "^" ]
        , binops [ "*", "/" ]
        , binops [ "+", "-", "++" ]
        , binops [ ">=", "<=", "==", "=/=" ]

        -- TODO pipes can't actually be mixed
        , binops [ "|>", "<|", "<<", ">>" ]
        , binops [ ":=", "+=", "-=", "/=", "*=" ]
        ]



----
--- Parens
--


parens : Parser Expression -> Parser Expression
parens higher =
    oneOf
        [ higher
        , surroundWith (RoundParen Open) (RoundParen Closed) (Parser.breakCircularDefinition <| \_ -> expr)
        ]


surroundWith : TokenKind -> TokenKind -> Parser a -> Parser a
surroundWith left right =
    Parser.surroundWith (exactTokenKind left) (exactTokenKind right)


exactTokenKind : TokenKind -> Parser ()
exactTokenKind targetKind =
    do tokenKind <| \kind ->
    if targetKind == kind then
        succeed ()

    else
        fail



----
--- Function application
--


functionApplication : Parser Expression -> Parser Expression
functionApplication higher =
    do higher <| \e ->
    do (oneOrMore higher) <| \es ->
    succeed <| FunctionCall e es



----
--- Unops
--


unops : Parser Expression -> Parser Expression
unops higher =
    do (optional unaryOperator) <| \maybeUnary ->
    do higher <| \right ->
    case maybeUnary of
        Just op ->
            succeed <| Unop op right

        Nothing ->
            succeed right


unaryOperator : Parser String
unaryOperator =
    do tokenKind <| \kind ->
    case kind of
        Token.Unop s ->
            succeed s

        _ ->
            fail



----
--- Binops
--


binops : List String -> Parser Expression -> Parser Expression
binops ops higher =
    let
        binopAndPrev : Parser ( String, Expression )
        binopAndPrev =
            Parser.tuple2 (binaryOperators ops) higher
    in
    do higher <| \left ->
    do (optional binopAndPrev) <| \maybeBinopAndPrev ->
    case maybeBinopAndPrev of
        Nothing ->
            succeed left

        Just ( op, right ) ->
            succeed <| Binop left op right


binaryOperators : List String -> Parser String
binaryOperators ops =
    do tokenKind <| \kind ->
    case kind of
        Token.Binop s ->
            -- TODO would a Set be faster? How do we ensure that he conversion to Set is not ran every time?
            -- It's probably better if the tokenizer sets the binop "precedence group" already
            if List.member s ops then
                succeed s

            else
                fail

        _ ->
            fail
