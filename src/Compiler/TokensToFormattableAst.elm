module Compiler.TokensToFormattableAst exposing (..)

import OneOrMore exposing (OneOrMore)
import Parser exposing (do, fail, higherOr, maybe, oneOf, oneOrMore, succeed, zeroOrMore)
import SepList exposing (SepList)
import Types.Error as Error exposing (Res)
import Types.FormattableAst as FA
import Types.Literal
import Types.Token as Token exposing (Token)


type alias MakeError =
    String -> String -> List Token -> Error.Error



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
    Parser.Parser Token (List Token) MakeError a


here : Parser Int
here =
    do Parser.here <| \tokens ->
    succeed
        (case tokens of
            next :: rest ->
                next.start

            [] ->
                0
        )



----
--- Main
--


parse : String -> String -> List Token -> Res (List FA.Statement)
parse moduleName code tokens =
    tokens
        |> Parser.parse module_ unconsIgnoreComments
        |> outcomeToResult moduleName code tokens
        |> Result.map OneOrMore.toList


outcomeToResult : String -> String -> List Token -> Parser.Outcome (List Token) MakeError a -> Res a
outcomeToResult moduleName code tokens outcome =
    case outcome of
        Parser.Success readState output ->
            Ok output

        Parser.Abort readState makeError ->
            Err <| makeError moduleName code readState

        Parser.Failure readState ->
            Err <| errorOptionsExhausted moduleName code readState


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


module_ : Parser (OneOrMore FA.Statement)
module_ =
    let
        start =
            oneOf
                [ kind Token.BlockStart
                , kind Token.NewSiblingLine
                ]

        statements =
            oomSeparatedBy (kind Token.NewSiblingLine) statement

        end =
            Parser.end
    in
    Parser.surroundWith start end statements



----
--- Errors
--


errorOptionsExhausted : String -> String -> List Token -> Error.Error
errorOptionsExhausted moduleName code nonConsumedTokens =
    case nonConsumedTokens of
        [] ->
            Error.makeError moduleName
                [ Error.text "I got to the end of file and I can't make sense of it. =(" ]

        token :: _ ->
            Error.makeError moduleName
                [ Error.text "I got stuck parsing at this point:"
                , Error.showLines code 2 token.start
                ]


errorCantUseMutableAssignmentHere : String -> String -> List Token -> Error.Error
errorCantUseMutableAssignmentHere moduleName code state =
    Error.makeError moduleName
        [ Error.text "errorCantUseMutableAssignmentHere" ]



{-
   errorCantUseWithInsidePatternMatching : String -> String -> List Token -> Error.Error
   errorCantUseWithInsidePatternMatching moduleName code nonConsumedTokens =
       case nonConsumedTokens of
           [] ->
               Debug.todo "wat"

           token :: _ ->
               Error.makeError moduleName
                   [ Error.text "`with` cannot be used when pattern-matching"
                   , Error.showLines code 2 token.start
                   ]
-}
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


{-|

    a >> b >> c

    a
        >> b
        >> c

    a
        >> b
        >> c

    a
        >> b
        >> c

-}
block : Parser a -> Parser a
block =
    surroundStrict Token.BlockStart Token.BlockEnd


sib : Parser a -> Parser a
sib =
    discardFirst (kind Token.NewSiblingLine)


sepListAtSep : Parser sep -> Parser item -> Parser (List ( sep, item ))
sepListAtSep sep item =
    do sep <| \sep0 ->
    do
        (oneOf
            [ block (sepListAtItem sep item)
            , sib (sepListAtItem sep item)
            , sepListAtItem sep item
            ]
        )
    <| \( item0, tail ) ->
    succeed (( sep0, item0 ) :: tail)


sepListAtItem : Parser sep -> Parser item -> Parser (SepList sep item)
sepListAtItem sep item =
    do item <| \item0 ->
    do
        (oneOf
            [ block (sepListAtSep sep item)
            , sib (sepListAtSep sep item)
            , sepListAtSep sep item
            , succeed []
            ]
        )
    <| \sepsAndItems ->
    succeed ( item0, sepsAndItems )


sepList : Parser sep -> Parser item -> Parser (SepList sep item)
sepList =
    sepListAtItem


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
--- Statements
--


typeAlias : Parser FA.Statement
typeAlias =
    do (kind <| Token.Name { mutable = False } "alias") <| \_ ->
    do (oneOrMore nonMutName) <| \( name, args ) ->
    do defop <| \{ mutable } ->
    do (inlineOrIndented typeExpr) <| \type_ ->
    if mutable then
        Parser.abort errorCantUseMutableAssignmentHere

    else
        { name = name
        , args = args
        , type_ = type_
        }
            |> FA.TypeAlias
            |> succeed


unionDef : Parser FA.Statement
unionDef =
    do (kind <| Token.Name { mutable = False } "union") <| \_ ->
    do (oneOrMore nonMutName) <| \( name, args ) ->
    do defop <| \{ mutable } ->
    do (inlineOrIndented <| rawList typeExpr) <| \cons ->
    if mutable then
        Parser.abort errorCantUseMutableAssignmentHere

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
            (if mutable then
                FA.Lvalue
                    { start = token.start
                    , end = token.end
                    , name = s
                    }

             else
                FA.Variable
                    { start = token.start
                    , end = token.end
                    , name = s
                    , binop = False
                    }
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
    in
    Parser.expression
        term
        -- the `Or` stands for `Or higher priority parser`
        [ higherOr <| parens (oneOf [ binopInsideParens, nest ])
        , higherOr <| list (\pos_TODO -> FA.List) nest
        , higherOr <| record Token.Defop FA.Record nest
        , higherOr lambda
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


parens : Parser a -> Parser a
parens main =
    surroundStrict (Token.RoundParen Token.Open) (Token.RoundParen Token.Closed) main



----
--- List
--


list : (FA.Pos -> List a -> a) -> Parser a -> Parser a
list constructor main =
    do here <| \start ->
    do (surroundMultiline (Token.SquareBracket Token.Open) (Token.SquareBracket Token.Closed) (maybe (rawList main))) <| \maybeLs ->
    do here <| \end ->
    (case maybeLs of
        Just ( h, t ) ->
            h :: t

        Nothing ->
            []
    )
        |> constructor ( start, end )
        |> succeed



----
--- Record
--


record : ({ mutable : Bool } -> Token.Kind) -> (FA.Pos -> FA.RecordArgs a -> a) -> Parser a -> Parser a
record assign constructor main =
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

        content start =
            do (maybe updateTarget) <| \maybeUpdateTarget ->
            do (rawList attr) <| \attrs ->
            do here <| \end ->
            { extends = maybeUpdateTarget
            , attrs = OneOrMore.toList attrs
            }
                |> constructor ( start, end )
                |> succeed
    in
    do here <| \s ->
    do (surroundMultiline (Token.CurlyBrace Token.Open) (Token.CurlyBrace Token.Closed) (maybe <| content s)) <| \maybeRecord ->
    do here <| \e ->
    case maybeRecord of
        Just re ->
            succeed re

        Nothing ->
            { extends = Nothing
            , attrs = []
            }
                |> constructor ( s, e )
                |> succeed



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
    in
    Parser.expression
        typeTerm
        -- the `Or` stands for `Or higher priority parser`
        [ typeParensOr nest
        , typeListOr nest
        , higherOr <| record Token.HasType FA.TypeRecord nest
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


lambda : Parser FA.Expression
lambda =
    let
        def =
            do (kind Token.Fn) <| \fn ->
            do (oneOrMore <| functionParameter pattern) <| \params ->
            do defop <| \{ mutable } ->
            if mutable then
                Parser.abort errorCantUseMutableAssignmentHere

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
    do def <| \( fn, params ) ->
    do body <| \b ->
    succeed <| FA.Lambda { start = fn.start, parameters = params, body = b }


functionParameter : Parser FA.Pattern -> Parser FA.Pattern
functionParameter nest =
    oneOf
        [ patternApplication fail
        , parens nest
        , list FA.PatternList nest
        , record Token.Defop FA.PatternRecord nest
        ]



----
--- Pattern
--


pattern : Parser FA.Pattern
pattern =
    let
        nest =
            Parser.breakCircularDefinition <| \_ -> pattern
    in
    Parser.expression
        (patternApplication <| functionParameter nest)
        -- the `Or` stands for `Or higher priority parser`
        [ higherOr <| parens nest
        , higherOr <| list FA.PatternList nest
        , higherOr <| record Token.Defop FA.PatternRecord nest

        --         , patternListConsOr
        --         , patternTupleOr
        ]


patternApplication : Parser FA.Pattern -> Parser FA.Pattern
patternApplication param =
    do oneToken <| \token ->
    case token.kind of
        Token.NumberLiteral s ->
            s
                |> Types.Literal.Number
                |> FA.PatternLiteral ( token.start, token.end )
                |> succeed

        Token.TextLiteral s ->
            s
                |> Types.Literal.Text
                |> FA.PatternLiteral ( token.start, token.end )
                |> succeed

        Token.Name { mutable } name ->
            if mutable then
                fail

            else
                do (zeroOrMore param) <| \params ->
                do here <| \end ->
                if params == [] then
                    FA.PatternAny ( token.start, token.end ) name
                        |> succeed

                else
                    FA.PatternApplication ( token.start, end ) name params
                        |> succeed

        _ ->
            fail



----
--- Function application
--


functionApplicationOr : Parser FA.Expression -> Parser FA.Expression
functionApplicationOr higher =
    do here <| \start ->
    do higher <| \e ->
    do (zeroOrMore higher) <| \es ->
    do here <| \end ->
    case es of
        argsHead :: argsTail ->
            succeed <| FA.FunctionCall start end { reference = e, arguments = ( argsHead, argsTail ) }

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


binopInsideParens : Parser FA.Expression
binopInsideParens =
    do oneToken <| \token ->
    case token.kind of
        Token.Binop g s ->
            { start = token.start
            , end = token.end
            , name = s
            , binop = True
            }
                |> FA.Variable
                |> succeed

        _ ->
            fail


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
