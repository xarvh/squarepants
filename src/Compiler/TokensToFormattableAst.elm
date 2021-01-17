module Compiler.TokensToFormattableAst exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (do, fail, maybe, oneOf, oneOrMore, succeed, zeroOrMore)
import SepList exposing (SepList)
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA
import Types.Token as Token exposing (Token)



{- TODO: if possible, replace all OneOrMore with List, BUT -}


d name =
    let
        logDebug { path, first } =
            Debug.log "d"
                ( path
                , "<=="
                , Result.map (\( output, readState ) -> List.take 1 readState |> List.map .kind) first
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


sepList : Parser sep -> Parser item -> Parser (SepList sep item)
sepList sep item =
    Parser.tuple2 item (zeroOrMore (Parser.tuple2 sep item))


discardFirst : Parser a -> Parser b -> Parser b
discardFirst a b =
    do a <| \_ -> b


oomSeparatedBy : Parser a -> Parser b -> Parser (OneOrMore b)
oomSeparatedBy sep pa =
    Parser.tuple2 pa (zeroOrMore (discardFirst sep pa))


oneToken : Parser Token
oneToken =
    Parser.consumeOne


inlineOrIndented : Parser a -> Parser a
inlineOrIndented p =
    oneOf
        [ surroundWith Token.BlockStart Token.BlockEnd p
        , p
        ]


nonMutName : Parser String
nonMutName =
    do oneToken <| \token ->
    case token.kind of
        Token.Name { mutable } s ->
            if mutable then
                fail

            else
                succeed s

        _ ->
            fail


defop : Parser { mutable : Bool }
defop =
    do oneToken <| \token ->
    case token.kind of
        Token.Defop arg ->
            succeed arg

        _ ->
            fail


standardList : { separator : Token.Kind, item : Parser a } -> Parser (OneOrMore a)
standardList { separator, item } =
    let
        sibsep =
            do (maybe <| kind Token.NewSiblingLine) <| \_ ->
            kind separator
    in
    discardFirst (maybe sibsep) (oomSeparatedBy sibsep item)



----
--- Main
--


runParser : Parser a -> List Token -> Result String a
runParser parser ts =
    Parser.parse parser unconsIgnoreComments ts


end : Parser a -> Parser a
end parser =
    do parser <| \v ->
    do Parser.end <| \_ ->
    succeed v


unconsIgnoreComments : List Token -> Maybe ( Token, List Token )
unconsIgnoreComments ls =
    case ls of
        head :: tail ->
            if head.kind == Token.Comment then
                unconsIgnoreComments tail

            else
                Just ( head, tail )

        [] ->
            Nothing


parse : List Token -> Result Error (List FA.Statement)
parse tokens =
    tokens
        |> runParser (end module_)
        |> Result.mapError (\s -> { pos = 0, kind = Error.Whatever s })
        |> Result.map OneOrMore.toList


module_ : Parser (OneOrMore FA.Statement)
module_ =
    do
        (oneOf
            [ kind Token.BlockStart
            , kind Token.NewSiblingLine
            ]
        )
    <| \_ ->
    oomSeparatedBy (kind Token.NewSiblingLine) statement



----
--- Statements
--


typeAlias : Parser FA.Statement
typeAlias =
    do (kind <| Token.Name { mutable = False } "alias") <| \_ ->
    do (oneOrMore nonMutName) <| \( name, args ) ->
    do defop <| \{ mutable } ->
    do (inlineOrIndented typeExpr) <| \type_ ->
    if mutable then
        Parser.abort "aliases can't be mutable"

    else
        { name = name
        , args = args
        , type_ = type_
        }
            |> FA.TypeAlias
            |> succeed


typeDefinition : Parser FA.Statement
typeDefinition =
    do (kind <| Token.Name { mutable = False } "type") <| \_ ->
    do (oneOrMore nonMutName) <| \( name, args ) ->
    do defop <| \{ mutable } ->
    do (inlineOrIndented <| standardList { separator = Token.Comma, item = constructor }) <| \cons ->
    if mutable then
        Parser.abort "can't use @= to define a type"

    else
        { name = name
        , args = args
        , constructors = OneOrMore.toList cons
        }
            |> FA.TypeDefinition
            |> succeed


constructor : Parser { name : String, args : List FA.Type }
constructor =
    let
        ctorArgs =
            oneOf
                [ typeTerm
                , surroundWith (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) typeExpr
                ]
    in
    do nonMutName <| \name ->
    do (zeroOrMore ctorArgs) <| \args ->
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

        Token.Name { mutable } s ->
            { start = token.start
            , end = token.end
            , name = s
            }
                |> (if mutable then
                        FA.Lvalue

                    else
                        FA.Variable
                   )
                |> su s

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
        , recordOr
        , lambdaOr
        , functionApplicationOr
        , unopsOr
        , binopsOr Token.Exponential
        , binopsOr Token.Multiplicative
        , binopsOr Token.Addittive

        -- TODO compops can collapse (ie, `1 < x < 10` => `1 < x && x < 10`)
        , binopsOr Token.Comparison

        -- TODO chain tuples
        , binopsOr Token.Tuple

        -- TODO pipes can't actually be mixed
        , binopsOr Token.Pipe
        , binopsOr Token.Mutop
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


commaSeparated : Parser expr -> Parser (OneOrMore expr)
commaSeparated v =
    let
        comma =
            kind Token.Comma

        commaAndV =
            do comma <| \_ -> v
    in
    do v <| \head ->
    do (zeroOrMore commaAndV) <| \tail ->
    do (maybe comma) <| \_ ->
    succeed ( head, tail )


surroundWith : Token.Kind -> Token.Kind -> Parser a -> Parser a
surroundWith left right =
    Parser.surroundWith (kind left) (kind right)


kind : Token.Kind -> Parser Token
kind targetKind =
    do oneToken <| \token ->
    if targetKind == token.kind then
        succeed token

    else
        fail



----
--- Record
--


recordOr : Parser FA.Expression -> Parser FA.Expression
recordOr higher =
    let
        attrAssignment =
            discardFirst
                (kind <| Token.Defop { mutable = False })
                (Parser.breakCircularDefinition <| \_ -> expr)

        attr =
            do nonMutName <| \name ->
            do (maybe attrAssignment) <| \maybeAssignment ->
            succeed ( name, maybeAssignment )

        updateTarget =
            do (Parser.breakCircularDefinition <| \_ -> expr) <| \h ->
            do (kind Token.With) <| \_ ->
            succeed h

        content =
            do (maybe updateTarget) <| \maybeUpdateTarget ->
            do (commaSeparated attr) <| \attrs ->
            { maybeUpdateTarget = maybeUpdateTarget
            , attrs = OneOrMore.toList attrs
            }
                |> FA.Record
                |> succeed
    in
    oneOf
        [ higher
        , surroundWith (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) content
        ]



----
--- Statements
--


statement : Parser FA.Statement
statement =
    Parser.breakCircularDefinition <| \_ ->
    Parser.oneOf
        [ typeAlias
        , typeDefinition
        , definition
        , do expr <| (FA.Evaluation >> succeed)
        ]


definition : Parser FA.Statement
definition =
    do (maybe typeAnnotation) <| \maybeAnnotation ->
    do (oneOrMore pattern) <| \( namePattern, params ) ->
    do defop <| \{ mutable } ->
    do (oneOf [ inlineStatement, statementBlock ]) <| \sb ->
    case namePattern of
        -- TODO if namePattern is any other pattern, then params must be empty
        FA.PatternAny name ->
            case annotationError maybeAnnotation name mutable of
                Just err ->
                    Parser.abort err

                Nothing ->
                    { name = namePattern
                    , mutable = mutable
                    , parameters = params
                    , body = sb
                    , maybeAnnotation = maybeAnnotation
                    }
                        |> FA.Definition
                        |> succeed


annotationError : Maybe FA.Annotation -> String -> Bool -> Maybe String
annotationError maybeAnnotation name mutable =
    case maybeAnnotation of
        Nothing ->
            Nothing

        Just annotation ->
            if annotation.name /= name then
                Just "annotation name doesn't match definition name"

            else if annotation.mutable /= mutable then
                Just "annotation mutability doesn't match definition"

            else
                Nothing


inlineStatement : Parser (OneOrMore FA.Statement)
inlineStatement =
    do statement <| \s ->
    succeed ( s, [] )


statementBlock : Parser (OneOrMore FA.Statement)
statementBlock =
    statement
        |> oomSeparatedBy (kind Token.NewSiblingLine)
        |> Parser.surroundWith (kind Token.BlockStart) (kind Token.BlockEnd)



----
--- Types
--


typeAnnotation : Parser FA.Annotation
typeAnnotation =
    do nonMutName <| \name ->
    do hasType <| \{ mutable } ->
    do typeExpr <| \t ->
    do (kind Token.NewSiblingLine) <| \_ ->
    succeed
        { name = name
        , mutable = mutable
        , type_ = t
        }


hasType : Parser { mutable : Bool }
hasType =
    do oneToken <| \token ->
    case token.kind of
        Token.HasType mutability ->
            succeed mutability

        _ ->
            fail


typeExpr : Parser FA.Type
typeExpr =
    Parser.expression typeTerm
        -- the `Or` stands for `Or higher priority parser`
        [ typeParensOr
        , typeApplicationOr
        , typeTupleOr
        , typeFunctionOr

        -- TODO record
        ]


typeTupleOr : Parser FA.Type -> Parser FA.Type
typeTupleOr higher =
    let
        group =
            Token.Tuple

        binopAndPrev : Parser FA.Type
        binopAndPrev =
            discardFirst (binaryOperators group) higher
    in
    do higher <| \head ->
    do (Parser.zeroOrMore binopAndPrev) <| \tail ->
    if tail == [] then
        succeed head

    else
        (head :: tail)
            |> FA.TypeTuple
            |> succeed


typeTerm : Parser FA.Type
typeTerm =
    Parser.map (\n -> FA.TypeConstantOrVariable { name = n, args = [] }) nonMutName


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


typeFunctionOr : Parser FA.Type -> Parser FA.Type
typeFunctionOr higher =
    let
        arrowAndHigher : Parser ( Bool, FA.Type )
        arrowAndHigher =
            do arrow <| \{ mutable } ->
            do higher <| \h ->
            succeed ( mutable, h )

        fold : ( Bool, FA.Type ) -> ( Bool, FA.Type ) -> ( Bool, FA.Type )
        fold ( nextIsMutable, ty ) ( thisIsMutable, accum ) =
            ( nextIsMutable
            , FA.TypeFunction
                { from = ty
                , fromIsMutable = thisIsMutable
                , to = accum
                }
            )
    in
    do higher <| \e ->
    do (zeroOrMore arrowAndHigher) <| \es ->
    let
        ( return, reversedArgs ) =
            OneOrMore.reverse ( ( False, e ), es )
    in
    reversedArgs
        |> List.foldl fold return
        |> Tuple.second
        |> succeed


arrow : Parser { mutable : Bool }
arrow =
    do oneToken <| \token ->
    case token.kind of
        Token.Arrow arg ->
            succeed arg

        _ ->
            fail


typeApplicationOr : Parser FA.Type -> Parser FA.Type
typeApplicationOr higher =
    do higher <| \ty ->
    case ty of
        FA.TypeConstantOrVariable fa ->
            -- TODO what about fa.args?
            do (zeroOrMore higher) <| \args ->
            { name = fa.name
            , args = args
            }
                |> FA.TypeConstantOrVariable
                |> succeed

        _ ->
            succeed ty



----
--- Lambda
--


lambdaOr : Parser FA.Expression -> Parser FA.Expression
lambdaOr higher =
    let
        def =
            do (kind Token.Fn) <| \fn ->
            do (oneOrMore pattern) <| \params ->
            do defop <| \{ mutable } ->
            if mutable then
                Parser.abort "lambdas can't be mutable"

            else
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
                  oneOrMore (discardFirst (kind Token.NewSiblingLine) statement)
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
                  succeed ( FA.Evaluation e, [] )
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
    do nonMutName <| \name ->
    succeed (FA.PatternAny name)



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
    do (sepList (binaryOperators group) higher) <| \( head, sepTail ) ->
    if sepTail == [] then
        succeed head

    else
        { group = group
        , sepList = ( head, sepTail )
        }
            |> FA.Binop
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
