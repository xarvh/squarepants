module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}

import Dict exposing (Dict)


type alias Module e =
    { typeDefinitions : Dict String TypeDefinition
    , valueDefinitions : Dict String (ValueDefinition e)
    }


type alias TypeDefinition =
    { name : String
    , args : List String
    , constructors : List TypeConstructor
    }


type alias ValueDefinition e =
    { name : String
    , maybeAnnotation : Maybe Type
    , body : List (Statement e)
    }


type alias TypeConstructor =
    { name : String
    , args : List Type
    }


{-| TODO expanded aliases should probably maintain a reference to the alias name?
-}
type Type
    = TypeConstant
        { name : String

        --, moduleReference : ModuleReference
        --, args : List Type
        }
    | TypeVariable
        { name : String
        }
    | TypeFunction
        -- `from` can actually be mutable
        { from : Type
        , fromIsMutable : Bool
        , to : Type
        }
    | TypeRecord
        (List
            { name : String
            , type_ : Type
            }
        )


type Statement e
    = Definition (ValueDefinition e)
      -- Evaluations are needed for return, mutation and debug
    | Evaluation (Expression e)
    | Assignment
        { lvalue : String
        , op : String
        , body : Expression e
        }


type Expression e
    = NumberLiteral
        e
        { start : Int
        , end : Int
        , number : String
        }
    | Variable
        e
        { start : Int
        , end : Int
        , name : String

        --, moduleReference : ModuleReference
        }
    | Lambda
        e
        { start : Int
        , parameter : String
        , body : List (Statement e)
        }
    | Record
        e
        -- TODO use a Dict instead? Attrs should not be ordered!
        (List
            { name : String
            , value : Expression e
            }
        )
    | Call
        e
        { reference : Expression e
        , argument : Expression e
        , argumentIsMutable : Bool
        }
    | If
        e
        { start : Int
        , condition : Expression e
        , true : Expression e
        , false : Expression e
        }



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
