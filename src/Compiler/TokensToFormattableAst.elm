module Compiler.TokensToFormattableAst exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (do, fail, maybe, oneOf, oneOrMore, succeed, zeroOrMore)
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA
import Types.Token as Token exposing (Token)


d name =
    let
        logDebug { path, first } =
            Debug.log "d"
                ( path
                , "<=="
                , Result.map (\( output, readState ) -> List.take 1 readState) first
                )
    in
    --Parser.doWithDebug logDebug name
    do


w name parser =
    --     parser
    d name parser succeed


su : String -> a -> Parser a
su name a =
    d name (succeed a) succeed


type alias Parser a =
    Parser.Parser Token (List Token) a



----
--- Helpers
--


oomSeparatedBy : Parser a -> Parser b -> Parser (OneOrMore b)
oomSeparatedBy sep pa =
    Parser.tuple2 pa (zeroOrMore (discardFirst sep pa))


discardFirst : Parser a -> Parser b -> Parser b
discardFirst a b =
    do a <| \_ -> b


isUppercaseSymbol : String -> Bool
isUppercaseSymbol s =
    -- TODO
    True


oneToken : Parser Token
oneToken =
    Parser.consumeOne


inlineOrIndented : Parser a -> Parser a
inlineOrIndented p =
    oneOf
        [ surroundWith Token.BlockStart Token.BlockEnd p
        , p
        ]



----
--- Main
--


runParser : Parser a -> List Token -> Result String a
runParser parser ts =
    Parser.parse parser uncons ts


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


parse : List Token -> Result Error (List FA.RootStatement)
parse tokens =
    tokens
        |> runParser (end module_)
        |> Result.mapError (\s -> { pos = 0, kind = Error.Whatever s })
        |> Result.map OneOrMore.toList


module_ : Parser (OneOrMore FA.RootStatement)
module_ =
    do
        (oneOf
            [ exactTokenKind Token.BlockStart
            , exactTokenKind Token.NewSiblingLine
            ]
        )
    <| \_ ->
   oomSeparatedBy (exactTokenKind Token.NewSiblingLine) rootStatement



----
--- Root Statements
--
-- These can contain type declarations
--


rootStatement : Parser FA.RootStatement
rootStatement =
    oneOf
        [ typeAlias
        , typeDefinition
        , do statement <| \s -> succeed (FA.Statement s)
        ]


typeAlias : Parser FA.RootStatement
typeAlias =
    do (exactTokenKind <| Token.Symbol "alias") <| \_ ->
    do (oneOrMore termName) <| \( name, args ) ->
    do (exactTokenKind Token.Defop) <| \_ ->
    do (inlineOrIndented typeExpr) <| \type_ ->
    { name = name
    , args = args
    , type_ = type_
    }
        |> FA.TypeAlias
        |> succeed


typeDefinition : Parser FA.RootStatement
typeDefinition =
    do (exactTokenKind <| Token.Symbol "type") <| \_ ->
    do (oneOrMore termName) <| \( name, args ) ->
    do (exactTokenKind Token.Defop) <| \_ ->
    do (inlineOrIndented constructors) <| \cons ->
    { name = name
    , args = args
    , constructors = cons
    }
        |> FA.TypeDefinition
        |> succeed


constructors : Parser (List { name : String, args : List FA.Type })
constructors =
    do (maybe <| exactTokenKind Token.ActualPipe) <| \_ ->
    zeroOrMore constructor


constructor : Parser { name : String, args : List FA.Type }
constructor =
    do (maybe <| exactTokenKind Token.NewSiblingLine) <| \_ ->
    do (exactTokenKind Token.ActualPipe) <| \_ ->
    do termName <| \name ->
    do (zeroOrMore typeExpr) <| \args ->
    succeed { name = name, args = args }



----
--- Term
--


term : Parser FA.Expression
term =
    do oneToken <| \token ->
    case token.kind of
        Token.NumberLiteral s ->
            su "nl" <| FA.NumberLiteral { start = token.start, end = token.end, number = s }

        Token.StringLiteral s ->
            su "sl" <| FA.StringLiteral { start = token.start, end = token.end, string = s }

        Token.Symbol s ->
            { start = token.start
            , end = token.end
            , variable = s
            , willBeMutated = False
            }
                |> FA.Variable
                |> su s

        Token.Mutop "@" ->
            do oneToken <| \token2 ->
            case token2.kind of
                Token.Symbol s ->
                    { start = token.start
                    , end = token2.end
                    , variable = s
                    , willBeMutated = True
                    }
                        |> FA.Variable
                        |> succeed

                _ ->
                    Parser.abort "something weird is following the mutop!"

        _ ->
            fail


termName : Parser String
termName =
    do oneToken <| \token ->
    case token.kind of
        Token.Symbol s ->
            succeed s

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
        ]



----
--- Parens
--


parensOr : Parser FA.Expression -> Parser FA.Expression
parensOr higher =
    oneOf
        [ higher
        , do (surroundWith (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) (Parser.breakCircularDefinition <| \_ -> commaSeparated expr)) <| \es ->
        case es of
            ( head, [] ) ->
                succeed head

            ( head, tail ) ->
                succeed <| FA.Tuple (head :: tail)
        ]


commaSeparated : Parser expr -> Parser (OneOrMore expr)
commaSeparated v =
    let
        comma =
            exactTokenKind Token.Comma

        commaAndV =
            do comma <| \_ -> v
    in
    do v <| \head ->
    do (zeroOrMore commaAndV) <| \tail ->
    do (maybe comma) <| \_ ->
    succeed ( head, tail )


surroundWith : Token.Kind -> Token.Kind -> Parser a -> Parser a
surroundWith left right =
    Parser.surroundWith (exactTokenKind left) (exactTokenKind right)


exactTokenKind : Token.Kind -> Parser Token
exactTokenKind targetKind =
    do oneToken <| \token ->
    if targetKind == token.kind then
        succeed token

    else
        fail



----
--- Statements
--


statement : Parser FA.Statement
statement =
    Parser.breakCircularDefinition <| \_ ->
    Parser.oneOf
        [ definition
        , do expr <| (FA.Evaluate >> succeed)
        ]


definition : Parser FA.Statement
definition =
    do (maybe typeAnnotation) <| \maybeAnnotation ->
    do (oneOrMore pattern) <| \( namePattern, params ) ->
    do (exactTokenKind Token.Defop) <| \_ ->
    do (oneOf [ inlineStatement, statementBlock ]) <| \sb ->
    case namePattern of
        -- TODO if namePattern is any other pattern, then params must be empty
        FA.PatternAny name ->
            if maybeAnnotation /= Nothing && Maybe.map .name maybeAnnotation /= Just name then
                Parser.abort "annotation name doesn't match definition name"

            else
                { name = namePattern
                , parameters = params
                , body = sb
                , maybeAnnotation = Maybe.map .type_ maybeAnnotation
                }
                    |> FA.Definition
                    |> succeed


inlineStatement : Parser (OneOrMore FA.Statement)
inlineStatement =
    do statement <| \s ->
    succeed ( s, [] )


statementBlock : Parser (OneOrMore FA.Statement)
statementBlock =
    statement
        |> oomSeparatedBy (exactTokenKind Token.NewSiblingLine)
        |> Parser.surroundWith (exactTokenKind Token.BlockStart) (exactTokenKind Token.BlockEnd)



----
--- Types
--


typeAnnotation : Parser { name : String, type_ : FA.Type }
typeAnnotation =
    do symbolName <| \name ->
    do (exactTokenKind Token.HasType) <| \_ ->
    do typeExpr <| \t ->
    do (exactTokenKind Token.NewSiblingLine) <| \_ ->
    succeed { name = name, type_ = t }


typeExpr : Parser FA.Type
typeExpr =
    Parser.expression typeTerm
        -- the `Or` stands for `Or higher priority parser`
        [ typeParensOr
        , typeMutableOr
        , typeFunctionOr
        , typeApplicationOr

        -- TODO record
        ]


typeTerm : Parser FA.Type
typeTerm =
    do oneToken <| \token ->
    case token.kind of
        Token.Symbol s ->
            { name = s
            }
                |> FA.TypeConstantOrVariable
                |> succeed

        _ ->
            fail


typeParensOr : Parser FA.Type -> Parser FA.Type
typeParensOr higher =
    oneOf
        [ higher
        , let
            parens =
                surroundWith
                    (Token.RoundParen Token.Open)
                    (Token.RoundParen Token.Closed)
                    (Parser.breakCircularDefinition <| \_ -> commaSeparated typeExpr)
          in
          do parens <| \t ->
          case t of
              ( head, [] ) ->
                  succeed <| head

              ( head, tail ) ->
                  succeed <| FA.TypeTuple (head :: tail)
        ]


typeMutableOr : Parser FA.Type -> Parser FA.Type
typeMutableOr higher =
    oneOf
        [ higher
        , do (discardFirst (exactTokenKind (Token.Mutop "@")) higher) <| \t ->
        succeed <| FA.TypeMutable t
        ]


typeFunctionOr : Parser FA.Type -> Parser FA.Type
typeFunctionOr higher =
    let
        arrowAndHigher =
            do (exactTokenKind Token.Arrow) <| \_ -> higher
    in
    do higher <| \e ->
    do (zeroOrMore arrowAndHigher) <| \es ->
    if es == [] then
        succeed e

    else
        succeed <| FA.TypeFunction (e :: es)


typeApplicationOr : Parser FA.Type -> Parser FA.Type
typeApplicationOr higher =
    oneOf
        [ higher
        , do typeTerm <| \base ->
        do (oneOrMore higher) <| \args ->
        { name = base
        , args = args
        }
            |> FA.TypePolymorphic
            |> succeed
        ]



----
--- Lambda
--


lambdaOr : Parser FA.Expression -> Parser FA.Expression
lambdaOr higher =
    let
        def =
            do (exactTokenKind Token.Fn) <| \fn ->
            do (oneOrMore pattern) <| \params ->
            do (exactTokenKind Token.Defop) <| \_ ->
            succeed ( fn, params )

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
          do def <| \( fn, params ) ->
          do body <| \b ->
          succeed <| FA.Lambda { start = fn.start, parameters = params, body = b }
        ]


{-| TODO
-}
pattern : Parser FA.Pattern
pattern =
    do oneToken <| \token ->
    case token.kind of
        Token.Symbol s ->
            succeed (FA.PatternAny s)

        _ ->
            fail


symbolName : Parser String
symbolName =
    do oneToken <| \token ->
    case token.kind of
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
    do (maybe unaryOperator) <| \maybeUnary ->
    do higher <| \right ->
    case maybeUnary of
        Just ( opAsString, opToken ) ->
            su "unop" <| FA.Unop { start = opToken.start, op = opAsString, right = right }

        Nothing ->
            succeed right


unaryOperator : Parser ( String, Token )
unaryOperator =
    do oneToken <| \token ->
    case token.kind of
        Token.Unop s ->
            succeed ( s, token )

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
        |> List.foldl (\( op, right ) leftAccum -> FA.Binop { left = leftAccum, op = op, right = right }) left
        |> succeed


binaryOperators : Token.PrecedenceGroup -> Parser String
binaryOperators group =
    do oneToken <| \token ->
    case token.kind of
        Token.Binop g s ->
            if g == group then
                succeed s

            else
                fail

        _ ->
            fail
