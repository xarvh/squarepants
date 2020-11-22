module Vier.TypeInference exposing (..)

import Dict exposing (Dict)
import OneOrMore exposing (OneOrMore)
import Vier.Syntax as Syntax


type InferredType
    = Known String
    | Placeholder PlaceholderId
    | Lambda (List InferredType) InferredType



--     | FunctionCall (List InferredType) InferredType


type alias PlaceholderId =
    Int


type alias Context =
    { symbolTypeByName : Dict String InferredType
    , nextPlaceholderId : PlaceholderId
    }


initContext : Context
initContext =
    { symbolTypeByName = Dict.empty
    , nextPlaceholderId = 0
    }



----
---
--


applySubstitutions : Dict PlaceholderId InferredType -> InferredType -> InferredType
applySubstitutions substitutions targetType =
    case targetType of
        Known s ->
            targetType

        Placeholder placeholderId ->
            substitutions
                |> Dict.get placeholderId
                |> Maybe.withDefault targetType

        Lambda paramTypes bodyType ->
            Lambda (List.map (applySubstitutions substitutions) paramTypes) (applySubstitutions substitutions bodyType)


addPlaceholder : String -> ( List InferredType, Context ) -> ( List InferredType, Context )
addPlaceholder name ( typesAccum, context ) =
    let
        placeholderType =
            Placeholder context.nextPlaceholderId
    in
    ( placeholderType :: typesAccum
    , { nextPlaceholderId = context.nextPlaceholderId + 1
      , symbolTypeByName = Dict.insert name placeholderType context.symbolTypeByName
      }
    )


inferExpr : Context -> Syntax.Expression -> ( InferredType, Dict PlaceholderId InferredType )
inferExpr context expr =
    case expr of
        Syntax.StringLiteral _ ->
            ( Known "String"
            , Dict.empty
            )

        Syntax.NumberLiteral _ ->
            ( Known "Number"
            , Dict.empty
            )

        Syntax.Variable variableName ->
            case Dict.get variableName context.symbolTypeByName of
                Just t ->
                    ( t
                    , Dict.empty
                    )

                Nothing ->
                    Debug.todo "variable not declared? o_O"

        Syntax.Lambda { parameters, body } ->
            let
                paramsAsList =
                    OneOrMore.toList parameters

                ( paramsPlaceholderTypes, lambdaState ) =
                    List.foldl addPlaceholder ( [], context ) paramsAsList

                ( inferredBodyType, substitutions ) =
                    inferStatements lambdaState body

                paramTypes =
                    List.map (applySubstitutions substitutions) paramsPlaceholderTypes
            in
            ( Lambda paramTypes inferredBodyType
            , substitutions
            )

        _ ->
            Debug.todo "BLAH"





doStuff statement (types, context) =
  case statement of

            Syntax.Return expr ->
                ( inferExpr context expr :: types
                , context
                )

            Syntax.Definition { name, parameters, body } ->
              ( types
              , Dict.insert context ....?
              }

            Syntax.Evaluate expr ->
              (types, context)

            Sytnax.Pass ->
              (types, context)

            Sytnax.If_Imperative _ ->
              Debug.todo "impif"

            Sytnax.Match_Imperative _ ->
              Debug.todo "imp match"


inferStatements : Context -> OneOrMore Syntax.Statement -> ( Context, Maybe InferredType)
inferStatements parentContext statements =
    case statements of
      -- single statement, return is implicit
      ( Syntax.Evaluate expr, []) ->
        ( Dict.empty
        , Just <| inferExpr parentContext expr
        )

      _ ->
        let
            (returnTypes, context) =
              statements
                |> OneOrMore.toList
                |> List.foldl doStuff ([], parentContext)

            (known, placeholders) =
              List.partition ??? returnTypes

        in


      [] ->



    if more /= [] then
        Debug.todo "STATEMENTS"

    else
        case one of
            Syntax.Evaluate expr ->
                -- TODO This makes sense only because there is a single statement!!
                inferExpr context expr

            Syntax.Return expr ->
                inferExpr context expr

            Syntax.Definition { name, parameters, body } ->
                -- TODO this is wrong
                inferStatements context body
--                 ( Known "Void"
--                 , ()
--                 )

            _ ->
                Debug.todo <| Debug.toString one
