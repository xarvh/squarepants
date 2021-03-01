module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Types.Literal


{-| The free variable type parameter `e` is used to store (and remove) position and type annotations

The Pos type is only one possibility, but it lives here because it's used in a few different modules.

-}
type alias Pos =
    ( Int, Int )


{-| TODO rename to Program? It's not even a program, it's just to sum of all declarations, not all of them will be used.
-}
type alias Module e =
    Dict String (RootDef e)



----
--- Root
--


type RootDef e
    = Union UnionDef
    | Alias AliasDef
    | Value (ValueDef e)


type alias AliasDef =
    { name : String
    , args : List String
    , ty : Type
    }


type alias UnionDef =
    { name : String
    , args : List String
    , constructors : Dict String (List Type)
    }


{-| TODO

      split RootValueDef
      (Right now I'm using `body == []` for native defs)

      -- also use this for every other name?
      type alias ref = { mod : String, def : String }

      type alias RootValueDef e =
          { ref : Ref
          , maybeAnnotation : Maybe Type
          , isNative : Bool
          , body : List (Statement e)
          }

-}
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
        { ref : String
        , args : List Type
        }
    | TypeVariable
        { name : String
        }
    | TypeFunction
        { from : Type
        , fromIsMutable : Maybe Bool
        , to : Type
        }
    | TypeRecord TypeRecordArgs
    | TypeAlias String Type


type alias TypeRecordArgs =
    { extensible : Maybe String
    , attrs : Dict String Type
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

   Maybe in the future, I can remove `e` from the constructors that are not needed to produce an error?

-}


type Statement e
    = Definition (ValueDef e)
      -- Evaluations are needed for return, mutation and debug
    | Evaluation (Expression e)


type Expression e
    = Literal e Types.Literal.Value
    | Variable e VariableArgs
    | Lambda
        e
        { parameter : Pattern
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
        -- we use the if also to get lazy ops and compacted compops, so even if the syntax does
        -- not support statement blocks inside if condition, it's useful that the AST can model it.
        { condition : List (Statement e)
        , true : List (Statement e)
        , false : List (Statement e)
        }
    | Try
        e
        { value : Expression e

        -- TODO rename to patternBlocks?
        , patterns : List ( Pattern, List (Statement e) )
        }


type Argument e
    = ArgumentExpression (Expression e)
    | ArgumentMutable VariableArgs


type alias VariableArgs =
    { name : String
    , attrPath : List String

    -- True => declared in the root scope
    -- False => declared inside a function's scope
    , isRoot : Bool
    }



----
--- Pattern
--


type Pattern
    = PatternDiscard
    | PatternAny String
    | PatternLiteral Types.Literal.Value
    | PatternConstructor String (List Pattern)
    | PatternRecord (Dict String Pattern)


patternNames : Pattern -> Set String
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
--- RootDef splitters
--


asValue : RootDef e -> Maybe (ValueDef e)
asValue r =
    case r of
        Value v ->
            Just v

        _ ->
            Nothing


asAlias : RootDef e -> Maybe AliasDef
asAlias r =
    case r of
        Alias a ->
            Just a

        _ ->
            Nothing


asUnion : RootDef e -> Maybe UnionDef
asUnion r =
    case r of
        Union u ->
            Just u

        _ ->
            Nothing


split : Module e -> ( Dict String AliasDef, Dict String UnionDef, Dict String (ValueDef e) )
split =
    let
        part3 n rootDef ( als, uns, vals ) =
            case rootDef of
                Alias a ->
                    ( Dict.insert n a als, uns, vals )

                Union u ->
                    ( als, Dict.insert n u uns, vals )

                Value v ->
                    ( als, uns, Dict.insert n v vals )
    in
    Dict.foldl part3 ( Dict.empty, Dict.empty, Dict.empty )


findValue : String -> Module e -> Maybe (ValueDef e)
findValue name mod =
    case Dict.get name mod of
        Just (Value valueDef) ->
            Just valueDef

        _ ->
            Nothing



----
--- Crawler
--


extensionFold_module : (Expression a -> ( a, acc ) -> ( b, acc )) -> ( Module a, acc ) -> ( Module b, acc )
extensionFold_module f ( a_mod, acc ) =
    let
        fold name a_rootDef ( b_mod, accX ) =
            case a_rootDef of
                Alias a ->
                    ( Dict.insert name (Alias a) b_mod, accX )

                Union u ->
                    ( Dict.insert name (Union u) b_mod, accX )

                Value a_valueDef ->
                    Tuple.mapFirst
                        (\b_rootDef -> Dict.insert name (Value b_rootDef) b_mod)
                        (extensionFold_valueDef f ( a_valueDef, accX ))
    in
    Dict.foldl fold ( Dict.empty, acc ) a_mod


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
            ( Lambda b { parameter = ar.parameter, body = bodyB }
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
            ( If b { condition = b_cond, true = b_true, false = b_false }
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
            ( Try b { value = b_value, patterns = b_patterns }
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
