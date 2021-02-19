module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Types.Literal


type alias Name =
    String


type alias Path =
    Name



----
--- Module
--


type alias Module e =
    { aliases : Dict Name AliasDef
    , unions : Dict Name UnionDef
    , values : List (ValueDef e)
    }


type alias AliasDef =
    { name : Name
    , args : List Name
    , ty : Type
    }


type alias UnionDef =
    { name : Name
    , args : List Name
    , constructors : List UnionConstructor
    }


type alias UnionConstructor =
    { name : Name
    , args : List Type
    }


type alias ValueDef e =
    { pattern : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Type
    , body : List (Statement e)
    }



----
--- Type
--


type Type
    = TypeConstant
        { path : Path
        , args : List Type
        }
    | TypeVariable
        { name : Name
        }
    | TypeFunction
        { from : Type
        , fromIsMutable : Maybe Bool
        , to : Type
        }
    | TypeRecord TypeRecordArgs
    | TypeAlias Path Type


type alias TypeRecordArgs =
    { extensible : Maybe Name
    , attrs : Dict Name Type
    }



----
--- Expressions
--
{-

   FormattableToCanonical should populate the `e` parameter with:

     * start and end position

     * unique id number, usable as type inference variable type

   But I don't think this will be implemented there before mutability is available.

   The important part is that this data can be replaced or removed (for example, for testing) via extensionFold_module.


-}


type Statement e
    = Definition (ValueDef e)
      -- Evaluations are needed for return, mutation and debug
    | Evaluation (Expression e)


type Expression e
    = Literal
        e
        { start : Int
        , end : Int
        , value : Types.Literal.Value
        }
    | Variable e VariableArgs
    | Lambda
        e
        { start : Int
        , parameter : Pattern
        , body : List (Statement e)
        }
    | Record
        e
        -- TODO rename to `extends`?
        { maybeUpdateTarget : Maybe VariableArgs
        , attrs : Dict String (Expression e)
        }
    | Call
        e
        { reference : Expression e
        , argument : Argument e
        }
    | If
        e
        { start : Int

        -- we use the if also to get lazy ops and compacted compops, so even if the syntax does
        -- not support statement blocks inside if condition, it's useful that the AST can model it.
        , condition : List (Statement e)
        , true : List (Statement e)
        , false : List (Statement e)
        }
    | Try
        e
        { start : Int
        , value : Expression e

        -- TODO rename to patternBlocks?
        , patterns : List ( Pattern, List (Statement e) )
        }


type Argument e
    = ArgumentExpression (Expression e)
    | ArgumentMutable VariableArgs


type alias VariableArgs =
    { start : Int
    , end : Int
    , path : Path
    , attrPath : List Name
    }



----
--- Pattern
--


type Pattern
    = PatternDiscard
    | PatternAny Name
    | PatternLiteral Types.Literal.Value
    | PatternConstructor Path (List Pattern)
    | PatternRecord (Dict Name Pattern)


patternNames : Pattern -> Set Name
patternNames p =
    case p of
        PatternDiscard ->
            Set.empty

        PatternAny n ->
            Set.singleton n

        PatternLiteral _ ->
            Set.empty

        PatternConstructor path ps ->
            List.foldl (patternNames >> Set.union) Set.empty ps

        PatternRecord ps ->
            Dict.foldl (\k -> patternNames >> Set.union) Set.empty ps



----
--- Helpers
--


findValue : Name -> Module e -> Maybe (ValueDef e)
findValue name mod =
    let
        rec vs =
            case vs of
                [] ->
                    Nothing

                h :: t ->
                    if Set.member name (patternNames h.pattern) then
                        Just h

                    else
                        rec t
    in
    rec mod.values


extensionFold_module : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( Module a, acc ) -> ( Module b, acc )
extensionFold_module f ( mod, acc ) =
    let
        fold a_valueDef ( vals, aX ) =
            Tuple.mapFirst
                (\b_expr -> b_expr :: vals)
                (extensionFold_valueDef f ( a_valueDef, aX ))
    in
    mod.values
        |> List.foldr fold ( [], acc )
        |> Tuple.mapFirst
            (\b_values ->
                { aliases = mod.aliases
                , unions = mod.unions
                , values = b_values
                }
            )


extensionFold_expression : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( Expression a, acc ) -> ( Expression b, acc )
extensionFold_expression f ( expr, acc ) =
    case expr of
        Literal a ar ->
            Tuple.mapFirst (\b -> Literal b ar) (f expr ( a, acc ))

        Variable a args ->
            Tuple.mapFirst (\b -> Variable b args) (f expr ( a, acc ))

        Lambda a ar ->
            let
                ( b, acc1 ) =
                    f expr ( a, acc )

                ( bodyB, acc2 ) =
                    extensionFold_block f ( ar.body, acc1 )
            in
            ( Lambda b { start = ar.start, parameter = ar.parameter, body = bodyB }
            , acc2
            )

        Record a ar ->
            let
                ( b, acc1 ) =
                    f expr ( a, acc )

                fold name a_expr ( attrs, aX ) =
                    Tuple.mapFirst
                        (\b_expr -> Dict.insert name b_expr attrs)
                        (extensionFold_expression f ( a_expr, aX ))

                ( b_attrs, acc2 ) =
                    Dict.foldl fold ( Dict.empty, acc1 ) ar.attrs
            in
            ( Record b { maybeUpdateTarget = ar.maybeUpdateTarget, attrs = b_attrs }
            , acc2
            )

        Call a ar ->
            let
                ( b, acc1 ) =
                    f expr ( a, acc )

                ( b_ref, acc2 ) =
                    extensionFold_expression f ( ar.reference, acc1 )

                ( b_arg, acc3 ) =
                    extensionFold_argument f ( ar.argument, acc2 )
            in
            ( Call b { reference = b_ref, argument = b_arg }
            , acc3
            )

        If a ar ->
            let
                -- TODO use a `do blah <| \b_meh ->` to avoid using accX?
                ( b, acc1 ) =
                    f expr ( a, acc )

                ( b_cond, acc2 ) =
                    extensionFold_block f ( ar.condition, acc1 )

                ( b_true, acc3 ) =
                    extensionFold_block f ( ar.true, acc2 )

                ( b_false, acc4 ) =
                    extensionFold_block f ( ar.false, acc3 )
            in
            ( If b { start = ar.start, condition = b_cond, true = b_true, false = b_false }
            , acc4
            )

        Try a ar ->
            let
                ( b, acc1 ) =
                    f expr ( a, acc )

                ( b_value, acc2 ) =
                    extensionFold_expression f ( ar.value, acc1 )

                fold ( pa, a_block ) ( pasAndBlocks, accX ) =
                    Tuple.mapFirst (\b_block -> ( pa, b_block ) :: pasAndBlocks) (extensionFold_block f ( a_block, accX ))

                ( b_patterns, acc3 ) =
                    List.foldr fold ( [], acc2 ) ar.patterns
            in
            ( Try b { start = ar.start, value = b_value, patterns = b_patterns }
            , acc3
            )


extensionFold_block : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( List (Statement a), acc ) -> ( List (Statement b), acc )
extensionFold_block f ( block, acc ) =
    let
        fold a_stat ( b_stats, accX ) =
            Tuple.mapFirst
                (\b_stat -> b_stat :: b_stats)
                (extensionFold_statement f ( a_stat, accX ))
    in
    List.foldr fold ( [], acc ) block


extensionFold_argument : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( Argument a, acc ) -> ( Argument b, acc )
extensionFold_argument f ( arg, acc ) =
    case arg of
        ArgumentExpression expr ->
            ( expr, acc )
                |> extensionFold_expression f
                |> Tuple.mapFirst ArgumentExpression

        ArgumentMutable ar ->
            ( ArgumentMutable ar, acc )


extensionFold_valueDef : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( ValueDef a, acc ) -> ( ValueDef b, acc )
extensionFold_valueDef f ( def, acc ) =
    ( def.body, acc )
        |> extensionFold_block f
        |> Tuple.mapFirst
            (\b_body ->
                { pattern = def.pattern
                , mutable = def.mutable
                , maybeAnnotation = def.maybeAnnotation
                , body = b_body
                }
            )


extensionFold_statement : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( Statement a, acc ) -> ( Statement b, acc )
extensionFold_statement f ( stat, acc ) =
    case stat of
        Definition ar ->
            Tuple.mapFirst Definition (extensionFold_valueDef f ( ar, acc ))

        Evaluation expr ->
            ( expr, acc )
                |> extensionFold_expression f
                |> Tuple.mapFirst Evaluation
