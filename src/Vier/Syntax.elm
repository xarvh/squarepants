module Vier.Syntax exposing (..)

import Parser exposing (consumeOne, do, fail, oneOf, optional, succeed)
import Vier.Token as Token exposing (OpenOrClosed(..), PrecedenceGroup(..), Token, TokenKind(..))


d name =
    --     Parser.doWithDebug (\{ path, first } -> Debug.log "d" ( path, first )) name
    do


w name parser =
    --     d name parser succeed
    parser


su : String -> a -> Parser a
su name a =
    d name (succeed a) succeed


type alias Parser a =
    Parser.Parser TokenKind (List TokenKind) a


type Expression
    = Literal String
    | Variable String
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error


parse : List Token -> Result String Expression
parse tokens =
    let
        uncons : List a -> Maybe ( a, List a )
        uncons ls =
            case ls of
                head :: tail ->
                    Just ( head, tail )

                [] ->
                    Nothing

        parser =
            d "root expr" expr <| \a ->
            d "root end" Parser.end <| \b ->
            succeed a
    in
    tokens
        |> List.map .kind
        |> Parser.parse parser uncons


tokenKind : Parser TokenKind
tokenKind =
    consumeOne



----
--- Term
--


term : Parser Expression
term =
    d "tokenKind" tokenKind <| \kind ->
    case kind of
        Token.NumberLiteral s ->
            su "nl" <| Literal s

        Token.StringLiteral s ->
            su "sl" <| Literal s

        Token.Symbol s ->
            su s <| Variable s

        _ ->
            fail



{- Precedence rules:

   ()

   f a b ------------> function application

   not, risk --------> unary

   ^ ----------------> exp

   * / --------------> multiplicative

   + - ++ -----------> addittive

   < > >= <= == =/= -> comparison

   and, or, xor -----> logical

   |> <| >> << ------> pipes

   = := += -= /= *= -> assignments

-}


expr : Parser Expression
expr =
    Parser.expression term
        [ parens
        , functionApplication
        , unops
        , binops Exponential
        , binops Multiplicative
        , binops AddittiveSpaced
        , binops Comparison

        -- TODO pipes can't actually be mixed
        , binops Pipe
        , binops Assignment
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
    do (Parser.zeroOrMore higher) <| \es ->
    case es of
        argsHead :: argsTail ->
            succeed <| FunctionCall e ( argsHead, argsTail )

        [] ->
            succeed e



----
--- Unops
--


unops : Parser Expression -> Parser Expression
unops higher =
    do (optional unaryOperator) <| \maybeUnary ->
    do higher <| \right ->
    case maybeUnary of
        Just op ->
            su "unop" <| Unop op right

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


binops : PrecedenceGroup -> Parser Expression -> Parser Expression
binops group higher =
    let
        binopAndPrev : Parser ( String, Expression )
        binopAndPrev =
            Parser.tuple2 (binaryOperators group) higher
    in
    do higher <| \left ->
    do (optional binopAndPrev) <| \maybeBinopAndPrev ->
    case maybeBinopAndPrev of
        Nothing ->
            succeed left

        Just ( op, right ) ->
            succeed <| Binop left op right


binaryOperators : PrecedenceGroup -> Parser String
binaryOperators group =
    do tokenKind <| \kind ->
    case kind of
        Token.Binop g s ->
            if g == group then
                succeed s

            else
                fail

        _ ->
            fail
