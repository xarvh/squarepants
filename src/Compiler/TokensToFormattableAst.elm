module Compiler.TokensToFormattableAst exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (do, fail, maybe, oneOf, oneOrMore, succeed, zeroOrMore)
import SepList exposing (SepList)
import Types.Error as Error exposing (Error, Res)
import Types.FormattableAst as FA
import Types.Literal
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
--- Terms
--


oneToken : Parser Token
oneToken =
    Parser.consumeOne


kind : Token.Kind -> Parser Token
kind targetKind =
    do oneToken <| \token ->
    if targetKind == token.kind then
        succeed token

    else
        fail


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



----
--- Combinators
--


discardFirst : Parser a -> Parser b -> Parser b
discardFirst a b =
    do a <| \_ -> b


discardSecond : Parser a -> Parser b -> Parser a
discardSecond a b =
    do a <| \aa ->
    do b <| \_ ->
    succeed aa


inlineOrIndented : Parser a -> Parser a
inlineOrIndented p =
    oneOf
        [ surroundStrict Token.BlockStart Token.BlockEnd p
        , discardFirst (maybe <| kind Token.NewSiblingLine) p
        ]


surroundStrict : Token.Kind -> Token.Kind -> Parser a -> Parser a
surroundStrict left right =
    Parser.surroundWith (kind left) (kind right)


surroundMultiline : Token.Kind -> Token.Kind -> Parser a -> Parser a
surroundMultiline left right content =
    discardFirst
        (kind left)
        (inlineOrIndented
            (discardSecond
                content
                (inlineOrIndented (kind right))
            )
        )


oomSeparatedBy : Parser a -> Parser b -> Parser (OneOrMore b)
oomSeparatedBy sep pa =
    Parser.tuple2 pa (zeroOrMore (discardFirst sep pa))


{-| TODO we support

    ```
    a = x
       + 2
    ```

but not

    ```
    a = x
       + 2
         + 3
    ```

also, note whether it is multiline or not, so that formatting can preserve it

-}
sepList : Parser sep -> Parser item -> Parser (SepList sep item)
sepList sep item =
    Parser.tuple2 item (zeroOrMore <| inlineOrIndented (Parser.tuple2 sep item))


{-| TODO make it more flexible

also, note whether it is multiline or not, so that formatting can preserve it

    x = [ a ]
    x = [, a ]
    x = [
      , a
      , b
      ]
    x = [
      , a, b
      , c, d
      ]

-}
rawList : Parser a -> Parser (OneOrMore a)
rawList item =
    let
        sibsep =
            do (maybe <| kind Token.NewSiblingLine) <| \_ ->
            kind Token.Comma
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


parse : List Token -> Res (List FA.Statement)
parse tokens =
    tokens
        |> runParser (end module_)
        |> Result.mapError (\e -> Error.Simple { pos = 0, kind = Error.Whatever e })
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


unionDef : Parser FA.Statement
unionDef =
    do (kind <| Token.Name { mutable = False } "type") <| \_ ->
    do (oneOrMore nonMutName) <| \( name, args ) ->
    do defop <| \{ mutable } ->
    do (inlineOrIndented <| rawList typeExpr) <| \cons ->
    if mutable then
        Parser.abort "can't use @= to define a type"

    else
        { name = name
        , args = args
        , constructors = OneOrMore.toList cons
        }
            |> FA.UnionDef
            |> succeed



{-
   constructor : Parser { name : String, args : List FA.Type }
   constructor =
       let
           ctorArgs =
               oneOf
                   [ typeTerm
                   , surroundStrict (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) typeExpr
                   ]
       in
       do nonMutName <| \name ->
       do (zeroOrMore ctorArgs) <| \args ->
       succeed { name = name, args = args }
-}
----
--- Term
--


term : Parser FA.Expression
term =
    do oneToken <| \token ->
    case token.kind of
        Token.NumberLiteral s ->
            { start = token.start, end = token.end, value = Types.Literal.Number s }
                |> FA.Literal
                |> succeed

        Token.TextLiteral s ->
            { start = token.start, end = token.end, value = Types.Literal.Text s }
                |> FA.Literal
                |> succeed

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
    let
        nest =
            Parser.breakCircularDefinition <| \_ -> expr

        recordConstructor maybeUpdateTarget attrs =
            { maybeUpdateTarget = maybeUpdateTarget
            , attrs = attrs
            }
                |> FA.Record
                |> succeed
    in
    Parser.expression term
        -- the `Or` stands for `Or higher priority parser`
        [ parensOr nest
        , listOr FA.List nest
        , recordOr Token.Defop recordConstructor nest
        , lambdaOr
        , functionApplicationOr
        , unopsOr
        , binopsOr Token.Exponential
        , binopsOr Token.Multiplicative
        , binopsOr Token.Addittive

        -- Compops can collapse (ie, `1 < x < 10` => `1 < x && x < 10`)
        , binopsOr Token.Comparison

        -- Tuples are chained (ie, `a & b & c` makes a tuple3)
        , binopsOr Token.Tuple

        -- TODO pipes can't actually be mixed
        , binopsOr Token.Pipe
        , binopsOr Token.Mutop
        , ifOr
        , tryOr
        ]



----
--- Parens
--


parensOr : Parser a -> Parser a -> Parser a
parensOr main higher =
    oneOf
        [ higher
        , surroundStrict (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) main
        ]



----
--- List
--


listOr : (List a -> a) -> Parser a -> Parser a -> Parser a
listOr constructor main higher =
    oneOf
        [ higher
        , do (surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (maybe (rawList main))) <| \maybeLs ->
        (case maybeLs of
            Just ( h, t ) ->
                h :: t

            Nothing ->
                []
        )
            |> constructor
            |> succeed
        ]



----
--- Record
--


recordOr : ({ mutable : Bool } -> Token.Kind) -> (Maybe a -> List ( String, Maybe a ) -> Parser a) -> Parser a -> Parser a -> Parser a
recordOr assign constructor main higher =
    let
        attrAssignment =
            discardFirst
                (kind <| assign { mutable = False })
                main

        attr =
            do nonMutName <| \name ->
            do (maybe attrAssignment) <| \maybeAssignment ->
            succeed ( name, maybeAssignment )

        updateTarget =
            do main <| \h ->
            do (kind Token.With) <| \_ ->
            succeed h

        content =
            do (maybe updateTarget) <| \maybeUpdateTarget ->
            do (rawList attr) <| \attrs ->
            constructor maybeUpdateTarget (OneOrMore.toList attrs)
    in
    oneOf
        [ higher
        , do (surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (maybe content)) <| \maybeRecord ->
        case maybeRecord of
            Just re ->
                succeed re

            Nothing ->
                constructor Nothing []
        ]



----
--- if..then
--


ifOr : Parser FA.Expression -> Parser FA.Expression
ifOr higher =
    let
        maybeNewLine k =
            discardFirst
                (maybe (kind Token.NewSiblingLine))
                (kind k)
    in
    oneOf
        [ higher
        , do (kind Token.If) <| \ifToken ->
        do expr <| \condition ->
        do (maybeNewLine Token.Then) <| \_ ->
        do inlineStatementOrBlock <| \true ->
        do (maybeNewLine Token.Else) <| \_ ->
        do inlineStatementOrBlock <| \false ->
        { start = ifToken.start
        , isOneLine = False
        , condition = condition
        , true = true
        , false = false
        }
            |> FA.If
            |> succeed
        ]



----
--- try..as
--


tryOr : Parser FA.Expression -> Parser FA.Expression
tryOr higher =
    let
        maybeNewLine : Parser a -> Parser a
        maybeNewLine =
            discardFirst (maybe (kind Token.NewSiblingLine))

        maybeNewLineKind : Token.Kind -> Parser Token
        maybeNewLineKind k =
            maybeNewLine (kind k)

        patternAndAccept =
            do pattern <| \p ->
            do (maybeNewLineKind Token.Then) <| \_ ->
            do inlineStatementOrBlock <| \accept ->
            succeed ( p, accept )

        default =
            discardFirst
                (maybeNewLineKind Token.Else)
                inlineStatementOrBlock

        single =
            do patternAndAccept <| \pna ->
            do default <| \def ->
            succeed ( [ pna ], Just def )

        multi =
            Parser.surroundWith (kind Token.BlockStart) (kind Token.BlockEnd) <|
                do (zeroOrMore (maybeNewLine patternAndAccept)) <| \pnas ->
                do (maybe default) <| \mdef ->
                succeed ( pnas, mdef )
    in
    oneOf
        [ higher
        , do (kind Token.Try) <| \tryToken ->
        do expr <| \value ->
        do (maybeNewLineKind Token.As) <| \_ ->
        do (oneOf [ single, multi ]) <| \( patterns, maybeElse ) ->
        { start = tryToken.start
        , isOneLine = False
        , value = value
        , patterns = patterns
        , maybeElse = maybeElse
        }
            |> FA.Try
            |> succeed
        ]



----
--- Statements
--


statement : Parser FA.Statement
statement =
    Parser.breakCircularDefinition <| \_ ->
    oneOf
        [ typeAlias
        , unionDef
        , definition
        , do expr <| (FA.Evaluation >> succeed)
        ]


{-| TODO separate annotation and definition to different statements to make the parser more flexible?
-}
definition : Parser FA.Statement
definition =
    do (maybe typeAnnotation) <| \maybeAnnotation ->
    do pattern <| \p ->
    do defop <| \{ mutable } ->
    do inlineStatementOrBlock <| \sb ->
    { pattern = p
    , mutable = mutable
    , body = sb
    , maybeAnnotation = maybeAnnotation
    }
        |> FA.Definition
        |> succeed


inlineStatementOrBlock : Parser (OneOrMore FA.Statement)
inlineStatementOrBlock =
    oneOf
        [ do (Parser.breakCircularDefinition <| \_ -> expr) <| \e -> succeed ( FA.Evaluation e, [] )
        , statement
            |> oomSeparatedBy (kind Token.NewSiblingLine)
            |> Parser.surroundWith (kind Token.BlockStart) (kind Token.BlockEnd)
        ]



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


typeTerm : Parser FA.Type
typeTerm =
    Parser.map (\n -> FA.TypeName { name = n }) nonMutName


typeExpr : Parser FA.Type
typeExpr =
    let
        nest =
            Parser.breakCircularDefinition <| \_ -> typeExpr

        recordConstructor : Maybe FA.Type -> List ( String, Maybe FA.Type ) -> Parser FA.Type
        recordConstructor extensible attrs =
            if extensible /= Nothing then
                Parser.abort "Extensible types are not supported, I want to see if it's good to do without them"

            else
                attrs
                    |> List.map (\( name, maybeAttr ) -> ( name, Maybe.withDefault (FA.TypeName { name = name }) maybeAttr ))
                    |> FA.TypeRecord
                    |> succeed
    in
    Parser.expression typeTerm
        -- the `Or` stands for `Or higher priority parser`
        [ typeParensOr nest
        , typeListOr nest
        , recordOr Token.HasType recordConstructor nest
        , typeApplicationOr
        , typeTupleOr
        , typeFunctionOr
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


typeParensOr : Parser FA.Type -> Parser FA.Type -> Parser FA.Type
typeParensOr main higher =
    oneOf
        [ higher
        , surroundStrict
            (Token.RoundParen Token.Open)
            (Token.RoundParen Token.Closed)
            main
        ]


typeListOr : Parser FA.Type -> Parser FA.Type -> Parser FA.Type
typeListOr main higher =
    oneOf
        [ higher
        , do
            (surroundStrict
                (Token.SquareBracket Token.Open)
                (Token.SquareBracket Token.Closed)
                main
            )
          <| \t ->
          { name = "List"
          , args = [ t ]
          }
              |> FA.TypePolymorphic
              |> succeed
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
        FA.TypeName { name } ->
            do (zeroOrMore higher) <| \args ->
            if args == [] then
                succeed ty

            else
                { name = name
                , args = args
                }
                    |> FA.TypePolymorphic
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
                     fn x = a

                     fn x =
                       a

                  -}
                  inlineStatementOrBlock
                ]
    in
    oneOf
        [ higher
        , --
          do def <| \( fn, params ) ->
          do body <| \b ->
          succeed <| FA.Lambda { start = fn.start, parameters = params, body = b }
        ]



----
--- Pattern
--


pattern : Parser FA.Pattern
pattern =
    let
        nest =
            Parser.breakCircularDefinition <| \_ -> pattern

        recordConstructor maybeUpdateTarget attrs =
            if maybeUpdateTarget /= Nothing then
                Parser.abort "can't use `with` for pattern matching!"

            else
                attrs
                    |> FA.PatternRecord
                    |> succeed
    in
    Parser.expression patternTerm
        -- the `Or` stands for `Or higher priority parser`
        [ parensOr nest
        , listOr FA.PatternList nest
        , recordOr Token.Defop recordConstructor nest
        , patternApplicationOr

        --         , patternListConsOr
        --         , patternTupleOr
        ]


patternTerm : Parser FA.Pattern
patternTerm =
    do oneToken <| \token ->
    case token.kind of
        Token.NumberLiteral s ->
            s
                |> Types.Literal.Number
                |> FA.PatternLiteral
                |> succeed

        Token.TextLiteral s ->
            s
                |> Types.Literal.Text
                |> FA.PatternLiteral
                |> succeed

        Token.Name { mutable } s ->
            if mutable then
                fail

            else
                s
                    |> FA.PatternAny
                    |> succeed

        _ ->
            fail


patternApplicationOr : Parser FA.Pattern -> Parser FA.Pattern
patternApplicationOr higher =
    do higher <| \p ->
    case p of
        FA.PatternAny name ->
            do (zeroOrMore higher) <| \args ->
            if args == [] then
                succeed p

            else
                FA.PatternApplication name args
                    |> succeed

        _ ->
            succeed p



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
