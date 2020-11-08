module Vier.Syntax exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (consumeOne, do, fail, oneOf, oneOrMore, optional, succeed, zeroOrMore)
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
    | Lambda { parameters : OneOrMore Pattern, body : OneOrMore Statement }
    | FunctionCall { reference : Expression, arguments : OneOrMore Expression }
    | Binop Expression String Expression
    | Unop String Expression
    | If_Functional { condition : Expression, true : Expression, false : Expression }
    | Match_Functional { value : Expression, patterns : List ( Pattern, Expression ), maybeElse : Maybe Expression }
    | Error


type Statement
    = Pass
    | Evaluate Expression
    | Definition { name : Pattern, parameters : List Pattern, body : OneOrMore Statement }
    | Return Expression
    | If_Imperative { condition : Expression, true : List Statement, false : List Statement }
    | Match_Imperative { value : Expression, patterns : List ( Pattern, List Statement ), maybeElse : Maybe (List Statement) }



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


parse : List Token -> Result Error (List Statement)
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
           oomSeparatedBy (exactTokenKind NewSiblingLine) statement
    in
    tokens
        |> List.map .kind
        |> runParser (end parser)
        |> Result.mapError (\s -> { pos = 0, kind = Error.Whatever s })
        |> Result.map OneOrMore.toList


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
        , binopsOr Addittive
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
--- Statements
--


statement : Parser Statement
statement =
    Parser.breakCircularDefinition <| \_ ->
    Parser.oneOf
        [ -- return
          do (discardFirst (exactTokenKind Token.Return) expr) <| \s ->
          succeed (Return s)
        , -- definition
          do (oneOrMore pattern) <| \( name, params ) ->
          do (exactTokenKind Token.Defop) <| \_ ->
          do
              (oneOf
                  [ statementBlock
                  , do expr <| \e -> succeed ( Evaluate e, [] )
                  ]
              )
          <| \sb ->
         succeed <| Definition { name = name, parameters = params, body = sb }

        -- TODO if
        -- TODO match
        , do expr <| (Evaluate >> succeed)
        ]


statementBlock : Parser (OneOrMore Statement)
statementBlock =
    statement
        |> oomSeparatedBy (exactTokenKind NewSiblingLine)
        |> Parser.surroundWith (exactTokenKind BlockStart) (exactTokenKind BlockEnd)


oomSeparatedBy : Parser a -> Parser b -> Parser (OneOrMore b)
oomSeparatedBy sep pa =
    Parser.tuple2 pa (zeroOrMore (discardFirst sep pa))


discardFirst : Parser a -> Parser b -> Parser b
discardFirst a b =
    do a <| \_ -> b



----
--- Lambda
--


lambdaOr : Parser Expression -> Parser Expression
lambdaOr higher =
    let
        def =
            do (exactTokenKind Fn) <| \_ ->
            do (oneOrMore pattern) <| \params ->
            do (exactTokenKind Token.Defop) <| \_ ->
            succeed params

        body : Parser (OneOrMore Statement)
        body =
            oneOf
                [ {-
                     fn x =
                     a
                     b
                     c
                  -}
                  oneOrMore (discardFirst (exactTokenKind NewSiblingLine) statement)
                , {-
                     fn x =
                       a
                  -}
                  statementBlock
                , {-
                     fn x = a
                  -}
                  do (succeed ()) <| \_ ->
                  do expr <| \e ->
                  succeed ( Evaluate e, [] )
                ]
    in
    oneOf
        [ higher
        , --
          do def <| \params ->
          do body <| \b ->
          succeed <| Lambda { parameters = params, body = b }
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
    do (zeroOrMore higher) <| \es ->
    case es of
        argsHead :: argsTail ->
            succeed <| FunctionCall { reference = e, arguments = ( argsHead, argsTail ) }

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
