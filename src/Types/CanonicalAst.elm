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
    , values : Dict Name (ValueDef e)
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


{-| TODO statements are always used in blocks.

type alias StatementBlock =
{ List { maybeAssignment, pattern, mutable, annotation

    Pattern
    mutable
    annotation

    { pattern : Pattern
    , mutable : Bool
    , maybeAnnotation : Maybe Type
    , body : List (Statement e)

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
{-
   expression_fold : (a -> accum -> ( b, accum )) -> accum -> Expression a -> ( Expression b, accum )
   expression_fold f accum expr =
       case expr of
           NumberLiteral a args ->
               f a accum
                   |> Tuple.mapFirst (\b -> NumberLiteral b args)

           Variable a args ->
               f a accum
                   |> Tuple.mapFirst (\b -> Variable b args)

           Lambda a { start, parameter, body } ->
               let
                   ( b, accum1 ) =
                       f a accum
               in
               expression_fold f accum1 body
                   |> Tuple.mapFirst (\bodyB -> Lambda b { start = start, parameter = parameter, body = bodyB })

           Record a attrsA ->
               let
                   ( b, accum1 ) =
                       f a accum

                   fold attrA ( attrs, acc0 ) =
                       let
                           ( exprB, acc1 ) =
                               expression_fold f acc0 attrA.value
                       in
                       ( { name = attrA.name, value = exprB } :: attrs
                       , acc1
                       )
               in
               List.foldl fold ( [], accum1 ) attrsA
                   |> Tuple.mapFirst (\attrsB -> Record b attrsB)

           Call a { reference, argument, argumentIsMutable } ->
               let
                   ( b, accum1 ) =
                       f a accum

                   ( refB, accum2 ) =
                       expression_fold f accum1 reference

                   ( argB, accum3 ) =
                       expression_fold f accum2 argument
               in
               ( Call b { reference = refB, argument = argB, argumentIsMutable = argumentIsMutable }
               , accum3
               )

           If a { start, condition, true, false } ->
               let
                   ( b, accum1 ) =
                       f a accum

                   ( condB, accum2 ) =
                       expression_fold f accum1 condition

                   ( trueB, accum3 ) =
                       expression_fold f accum2 true

                   ( falseB, accum4 ) =
                       expression_fold f accum3 false
               in
               ( If b { start = start, condition = condB, true = trueB, false = falseB }
               , accum4
               )


   expression_map : (a -> b) -> Expression a -> Expression b
   expression_map f ea =
       expression_fold (\a {} -> ( f a, {} )) {} ea |> Tuple.first
-}
