module Vier.Syntax exposing (..)

import Parser exposing (consumeOne, do, fail, oneOf, oneOrMore, optional, succeed)
import Vier.Error as Error exposing (Error)
import Vier.Token as Token exposing (OpenOrClosed(..), PrecedenceGroup(..), Token, TokenKind(..))


d name =
    let
        logDebug { path, first } =
            Debug.log "d"
                ( path
                , "<=="
                , Result.map (\( output, readState ) -> List.take 1 readState) first
                )
    in
    --     do
    Parser.doWithDebug logDebug name


w name parser =
    --     parser
    d name parser succeed


su : String -> a -> Parser a
su name a =
    d name (succeed a) succeed


type alias Parser a =
    Parser.Parser TokenKind (List TokenKind) a



----
--- AST
--


type alias Pattern =
    -- TODO
    String


type Expression
    = Literal String
    | Variable String
    | Lambda (List Pattern) (List Expression)
    | FunctionCall Expression ( Expression, List Expression )
    | Binop Expression String Expression
    | Unop String Expression
    | If { condition : Expression, true : Expression, false : Expression }
    | Error



----
--- Main
--


runParser : Parser a -> List TokenKind -> Result String a
runParser parser ts =
    Parser.parse parser uncons ts
        |> Debug.log "RESULT"


end : Parser a -> Parser a
end parser =
    do parser <| \v ->
    do Parser.end <| \_ ->
    succeed v


uncons : List a -> Maybe ( a, List a )
uncons ls =
    case ls of
        head :: tail ->
            Just ( head, tail )

        [] ->
            Nothing


parse : List Token -> Result Error Expression
parse tokens =
    let
        parser =
            do
                (oneOf
                    [ exactTokenKind BlockStart
                    , exactTokenKind NewSiblingLine
                    ]
                )
            <| \_ ->
           expr
    in
    tokens
        |> List.map .kind
        |> runParser (end parser)
        |> Result.mapError (\s -> { pos = 0, kind = Error.Whatever s })


tokenKind : Parser TokenKind
tokenKind =
    consumeOne



----
--- Term
--


term : Parser Expression
term =
    d "term" tokenKind <| \kind ->
    case kind of
        Token.NumberLiteral s ->
            su "nl" <| Literal s

        Token.StringLiteral s ->
            su "sl" <| Literal s

        Token.Symbol s ->
            su s <| Variable s

        _ ->
            fail



----
--- Expr (with precedence rules)
--


expr : Parser Expression
expr =
    Parser.expression term
        -- the `Or` stands for `Or higher priority parser`
        [ parensOr
        , lambdaOr
        , functionApplicationOr
        , unopsOr
        , binopsOr Exponential
        , binopsOr Multiplicative
        , binopsOr AddittiveSpaced
        , binopsOr Comparison

        -- TODO pipes can't actually be mixed
        , binopsOr Pipe
        , binopsOr Assignment
        ]



----
--- Parens
--


parensOr : Parser Expression -> Parser Expression
parensOr higher =
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
--- Lambda
--


lambdaOr : Parser Expression -> Parser Expression
lambdaOr higher =
    let
        def =
            do (exactTokenKind Fn) <| \_ ->
            do (oneOrMore pattern) <| \( argsHead, argsTail ) ->
            do (exactTokenKind (Token.Binop Assignment "=")) <| \_ ->
            succeed <| argsHead :: argsTail

        body =
            oneOf
                [ --
                  do (oneOrMore <| do (exactTokenKind NewSiblingLine) <| \_ -> expr) <| \( h, t ) ->
                  succeed (h :: t)
                , --
                  do (exactTokenKind BlockStart) <| \_ ->
                  do expr <| \e ->
                  do (exactTokenKind BlockEnd) <| \_ ->
                  succeed [ e ]
                , --
                  do (succeed ()) <| \_ ->
                  do expr <| \b ->
                  succeed [ b ]
                ]
    in
    oneOf
        [ higher
        , --
          do def <| \args ->
          do body <| \b ->
          succeed <| Lambda args b
        ]


{-| TODO
-}
pattern : Parser Pattern
pattern =
    do tokenKind <| \kind ->
    case kind of
        Symbol s ->
            succeed s

        _ ->
            fail



----
--- Function application
--


functionApplicationOr : Parser Expression -> Parser Expression
functionApplicationOr higher =
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


unopsOr : Parser Expression -> Parser Expression
unopsOr higher =
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


binopsOr : PrecedenceGroup -> Parser Expression -> Parser Expression
binopsOr group higher =
    let
        binopAndPrev : Parser ( String, Expression )
        binopAndPrev =
            Parser.tuple2 (binaryOperators group) higher
    in
    do higher <| \left ->
    do (Parser.zeroOrMore binopAndPrev) <| \binopAndPrevs ->
    binopAndPrevs
        |> List.foldl (\( op, right ) leftAccum -> Binop leftAccum op right) left
        |> succeed


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
