module Compiler.TokensToFormattableAst exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (consumeOne, do, fail, oneOf, oneOrMore, optional, succeed, zeroOrMore)
import Types.Error as Error exposing (Error)
import Types.Token as Token exposing (Token)
import Types.FormattableAst as FA


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
    Parser.Parser Token.Kind (List Token.Kind) a



----
--- Main
--


runParser : Parser a -> List Token.Kind -> Result String a
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


parse : List Token -> Result Error (List FA.Statement)
parse tokens =
    let
        parser =
            do
                (oneOf
                    [ exactTokenKind Token.BlockStart
                    , exactTokenKind Token.NewSiblingLine
                    ]
                )
            <| \_ ->
           oomSeparatedBy (exactTokenKind Token.NewSiblingLine) statement
    in
    tokens
        |> List.map .kind
        |> runParser (end parser)
        |> Result.mapError (\s -> { pos = 0, kind = Error.Whatever s })
        |> Result.map OneOrMore.toList


tokenKind : Parser Token.Kind
tokenKind =
    consumeOne



----
--- Term
--


term : Parser FA.Expression
term =
    d "term" tokenKind <| \kind ->
    case kind of
        Token.NumberLiteral s ->
            su "nl" <| FA.NumberLiteral s

        Token.StringLiteral s ->
            su "sl" <| FA.StringLiteral s

        Token.Symbol s ->
            su s <| FA.Variable s

        _ ->
            fail



----
--- Expr (with precedence rules)
--


expr : Parser FA.Expression
expr =
    Parser.expression term
        -- the `Or` stands for `Or higher priority parser`
        [ parensOr
        , lambdaOr
        , functionApplicationOr
        , unopsOr
        , binopsOr Token.Exponential
        , binopsOr Token.Multiplicative
        , binopsOr Token.Addittive
        , binopsOr Token.Comparison

        -- TODO pipes can't actually be mixed
        , binopsOr Token.Pipe
        , binopsOr Token.Assignment
        ]



----
--- Parens
--


parensOr : Parser FA.Expression -> Parser FA.Expression
parensOr higher =
    oneOf
        [ higher
        , surroundWith (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) (Parser.breakCircularDefinition <| \_ -> expr)
        ]


surroundWith : Token.Kind -> Token.Kind -> Parser a -> Parser a
surroundWith left right =
    Parser.surroundWith (exactTokenKind left) (exactTokenKind right)


exactTokenKind : Token.Kind -> Parser ()
exactTokenKind targetKind =
    do tokenKind <| \kind ->
    if targetKind == kind then
        succeed ()

    else
        fail



----
--- Statements
--


statement : Parser FA.Statement
statement =
    Parser.breakCircularDefinition <| \_ ->
    Parser.oneOf
        [ -- return
          do (discardFirst (exactTokenKind Token.Return) expr) <| \s ->
          succeed (FA.Return s)
        , -- definition
          do (oneOrMore pattern) <| \( name, params ) ->
          do (exactTokenKind Token.Defop) <| \_ ->
          do
              (oneOf
                  [ statementBlock
                  , do expr <| \e -> succeed ( FA.Evaluate e, [] )
                  ]
              )
          <| \sb ->
         succeed <| FA.Definition { name = name, parameters = params, body = sb }

        -- TODO if
        -- TODO match
        , do expr <| (FA.Evaluate >> succeed)
        ]


statementBlock : Parser (OneOrMore FA.Statement)
statementBlock =
    statement
        |> oomSeparatedBy (exactTokenKind Token.NewSiblingLine)
        |> Parser.surroundWith (exactTokenKind Token.BlockStart) (exactTokenKind Token.BlockEnd)


oomSeparatedBy : Parser a -> Parser b -> Parser (OneOrMore b)
oomSeparatedBy sep pa =
    Parser.tuple2 pa (zeroOrMore (discardFirst sep pa))


discardFirst : Parser a -> Parser b -> Parser b
discardFirst a b =
    do a <| \_ -> b



----
--- Lambda
--


lambdaOr : Parser FA.Expression -> Parser FA.Expression
lambdaOr higher =
    let
        def =
            do (exactTokenKind Token.Fn) <| \_ ->
            do (oneOrMore pattern) <| \params ->
            do (exactTokenKind Token.Defop) <| \_ ->
            succeed params

        body : Parser (OneOrMore FA.Statement)
        body =
            oneOf
                [ {-
                     fn x =
                     a
                     b
                     c
                  -}
                  oneOrMore (discardFirst (exactTokenKind Token.NewSiblingLine) statement)
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
                  succeed ( FA.Evaluate e, [] )
                ]
    in
    oneOf
        [ higher
        , --
          do def <| \params ->
          do body <| \b ->
          succeed <| FA.Lambda { parameters = params, body = b }
        ]


{-| TODO
-}
pattern : Parser FA.Pattern
pattern =
    do tokenKind <| \kind ->
    case kind of
        Token.Symbol s ->
            succeed s

        _ ->
            fail



----
--- Function application
--


functionApplicationOr : Parser FA.Expression -> Parser FA.Expression
functionApplicationOr higher =
    do higher <| \e ->
    do (zeroOrMore higher) <| \es ->
    case es of
        argsHead :: argsTail ->
            succeed <| FA.FunctionCall { reference = e, arguments = ( argsHead, argsTail ) }

        [] ->
            succeed e



----
--- Unops
--


unopsOr : Parser FA.Expression -> Parser FA.Expression
unopsOr higher =
    do (optional unaryOperator) <| \maybeUnary ->
    do higher <| \right ->
    case maybeUnary of
        Just op ->
            su "unop" <| FA.Unop op right

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


binopsOr : Token.PrecedenceGroup -> Parser FA.Expression -> Parser FA.Expression
binopsOr group higher =
    let
        binopAndPrev : Parser ( String, FA.Expression )
        binopAndPrev =
            Parser.tuple2 (binaryOperators group) higher
    in
    do higher <| \left ->
    do (Parser.zeroOrMore binopAndPrev) <| \binopAndPrevs ->
    binopAndPrevs
        |> List.foldl (\( op, right ) leftAccum -> FA.Binop leftAccum op right) left
        |> succeed


binaryOperators : Token.PrecedenceGroup -> Parser String
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
