module Compiler.FormattableToCanonicalAst exposing (..)

import Dict exposing (Dict)
import OneOrMore
import Set exposing (Set)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Error)
import Types.FormattableAst as FA


type alias Res ok =
    Result Error ok



----
--- Module
--


translateModule : FA.Module -> Res (CA.Module ())
translateModule faModule =
    translateModuleRec
        faModule
        { typeDefinitions = Dict.empty
        , valueDefinitions = Dict.empty
        }


translateModuleRec : FA.Module -> CA.Module () -> Res (CA.Module ())
translateModuleRec faModule caModule =
    case faModule of
        [] ->
            Ok caModule

        faStat :: faStatTail ->
            caModule
                |> insertStatement faStat
                |> Result.andThen (translateModuleRec faStatTail)


insertStatement : FA.Statement -> CA.Module () -> Res (CA.Module ())
insertStatement faStatement caModule =
    case faStatement of
        FA.Evaluation _ ->
            errorTodo "Root Evaluations don't really do much =|"

        FA.Definition fa ->
            fa
                |> translateDefinition
                |> Result.andThen
                    (\def ->
                        if Dict.member def.name caModule.valueDefinitions then
                            errorTodo <| def.name ++ " declared twice!"

                        else
                            Ok { caModule | valueDefinitions = Dict.insert def.name def caModule.valueDefinitions }
                    )

        FA.Mutation _ ->
            errorTodo "Root Mutations are not allowed o_O"

        FA.TypeAlias fa ->
            -- TODO aliases need to be loaded on their own and applied to all types
            {-
               fa.type_
                   |> translateType
                   |> Result.map
                       (\translatedType ->
                           CA.TypeAlias
                               { name = fa.name
                               , args = fa.args
                               , type_ = translatedType
                               }
                       )
            -}
            errorTodo "aliases not yet implemented"

        FA.TypeDefinition fa ->
            if Dict.member fa.name caModule.typeDefinitions then
                errorTodo <| fa.name ++ " declared twice!"

            else
                case validateTypeDefinition fa of
                    Just error ->
                        errorTodo error

                    Nothing ->
                        let
                            translateConstructor : FA.TypeConstructor -> Res CA.TypeConstructor
                            translateConstructor faCons =
                                faCons.args
                                    |> List.map translateType
                                    |> listResultToResultList
                                    |> Result.map
                                        (\caArgs ->
                                            { name = faCons.name
                                            , args = caArgs
                                            }
                                        )

                            consListToModule : List CA.TypeConstructor -> CA.Module ()
                            consListToModule consList =
                                { caModule
                                    | typeDefinitions =
                                        Dict.insert
                                            fa.name
                                            { name = fa.name
                                            , args = fa.args
                                            , constructors = consList
                                            }
                                            caModule.typeDefinitions
                                }
                        in
                        fa.constructors
                            |> List.map translateConstructor
                            |> listResultToResultList
                            |> Result.map consListToModule


validateTypeDefinition { name, args, constructors } =
    let
        names =
            List.map .name constructors
    in
    if not <| firstCharIsUpper name then
        Just "type names should be uppercase"

    else if List.any (\n -> not <| firstCharIsUpper n) names then
        Just "constructor names should be uppercase"

    else if Set.size (Set.fromList names) < List.length names then
        Just "duplicate constructor names"

    else
        Nothing



----
--- Definition
--


translateDefinition : FA.ValueDefinition -> Res (CA.ValueDefinition ())
translateDefinition fa =
    let
        (FA.PatternAny name) =
            fa.name

        resultMaybeAnnotation =
            fa.maybeAnnotation
                |> Maybe.map (translateAnnotation name fa)
                |> maybeResultToResultMaybe

        resultBody =
            fa.body
                |> OneOrMore.toList
                |> List.map translateStatement
                |> listResultToResultList
    in
    Result.map2
        (\maybeAnnotation body ->
            { name = name
            , mutable = fa.mutable
            , maybeAnnotation = maybeAnnotation
            , body = List.foldr wrapLambda body fa.parameters
            }
        )
        resultMaybeAnnotation
        resultBody


translateAnnotation : String -> FA.ValueDefinition -> FA.Annotation -> Res CA.Type
translateAnnotation defName faDef faAnn =
    if faAnn.name /= defName then
        errorTodo "annotation name does not match"

    else if faDef.mutable /= faAnn.mutable then
        errorTodo "annotation mutability does not match"

    else
        translateType faAnn.type_



----
--- Statement
--


translateStatement : FA.Statement -> Res (CA.Statement ())
translateStatement faStat =
    case faStat of
        FA.Evaluation faExpr ->
            -- TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            -- Debug evaluations should be optimized away in production build
            faExpr
                |> translateExpression
                |> Result.map CA.Evaluation

        FA.Definition fa ->
            fa
                |> translateDefinition
                |> Result.map CA.Definition

        FA.Mutation fa ->
            -- TODO!!
            errorTodo "Mutation not supported yet"

        FA.TypeAlias fa ->
            errorTodo "Aliases can be declared only in the root scope"

        FA.TypeDefinition fa ->
            errorTodo "Types can be declared only in the root scope"



----
--- Expression
--


translateExpression : FA.Expression -> Res (CA.Expression ())
translateExpression faExpr =
    case faExpr of
        FA.NumberLiteral args ->
            Ok <| CA.NumberLiteral () args

        FA.Variable args ->
            { start = args.start
            , end = args.end
            , name = args.name

            -- TODO check that args.willBeMutated is on only if being used in an expression?
            -- Probably needs to be handled at the function call level
            }
                |> CA.Variable ()
                |> Ok

        FA.Lambda fa ->
            let
                ( FA.PatternAny paramsHead, paramsTail ) =
                    fa.parameters
            in
            fa.body
                |> OneOrMore.toList
                |> List.map translateStatement
                |> listResultToResultList
                |> Result.map
                    (\body ->
                        CA.Lambda ()
                            -- TODO start!
                            { start = 0
                            , parameter = paramsHead
                            , body = List.foldr wrapLambda body paramsTail
                            }
                    )

        FA.FunctionCall { reference, arguments } ->
            -- ref arg1 arg2 arg3...
            let
                fold : CA.Argument () -> CA.Expression () -> CA.Expression ()
                fold argument refAccum =
                    { reference = refAccum
                    , argument = argument
                    }
                        |> CA.Call ()
            in
            Result.map2
                (List.foldl fold)
                (translateExpression reference)
                (arguments
                    |> OneOrMore.toList
                    |> List.map translateArgument
                    |> listResultToResultList
                )

        FA.If { start, condition, true, false } ->
            Result.map3
                (\c t f ->
                    CA.If ()
                        { start = start
                        , condition = c
                        , true = t
                        , false = f
                        }
                )
                (translateExpression condition)
                (translateExpression true)
                (translateExpression false)

        FA.Tuple list ->
            case list of
                [ first, second ] ->
                    Result.map2
                        (\f s ->
                            CA.Record ()
                                [ { name = "first", value = f }
                                , { name = "second", value = s }
                                ]
                        )
                        (translateExpression first)
                        (translateExpression second)

                _ ->
                    errorTodo "sorry, I'm supporting only tuples of size 2"

        FA.Binop { left, op, right } ->
            --
            -- Unlike other ML languages, the left operand is the *second* argument
            --
            -- `a + b` == `((+) b) a`
            --
            Result.map2
                (\l r ->
                    CA.Call ()
                        { argument = l
                        , reference =
                            CA.Call ()
                                { argument = r
                                , reference =
                                    CA.Variable ()
                                        -- TODO start, end!!
                                        { start = 0
                                        , end = 0
                                        , name = op
                                        }
                                }
                        }
                )
                (translateArgument left)
                (translateArgument right)

        FA.Lvalue args ->
            errorTodo "mutable values can be used only as arguments for function or mutation operators"

        _ ->
            errorTodo <| "NOT SUPPORTED FOR NOW " ++ Debug.toString faExpr


translateArgument : FA.Expression -> Res (CA.Argument ())
translateArgument faExpr =
    case faExpr of
        FA.Lvalue args ->
            args.name
                |> CA.ArgumentMutable
                |> Ok

        _ ->
            faExpr
                |> translateExpression
                |> Result.map CA.ArgumentExpression



----
--- Type
--


addAttribute : List ( String, FA.Type ) -> List { name : String, type_ : CA.Type } -> Res CA.Type
addAttribute faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            caAttrsAccum
                |> CA.TypeRecord
                |> Ok

        ( name, faType ) :: faTail ->
            case translateType faType of
                Err e ->
                    -- TODO add `name` in the error?
                    Err e

                Ok caType ->
                    addAttribute faTail ({ name = name, type_ = caType } :: caAttrsAccum)


firstCharIsUpper : String -> Bool
firstCharIsUpper s =
    case String.uncons s of
        Nothing ->
            False

        Just ( head, tail ) ->
            Char.isUpper head


translateType : FA.Type -> Res CA.Type
translateType faType =
    case faType of
        FA.TypeConstantOrVariable { name, args } ->
            if firstCharIsUpper name then
                args
                    |> List.map translateType
                    |> listResultToResultList
                    |> Result.map
                        (\caArgs ->
                            { name = name
                            , args = caArgs
                            }
                                |> CA.TypeConstant
                        )

            else if args /= [] then
                -- TODO is this the correct error?
                errorTodo "rank 2 types are not supported"

            else
                { name = name }
                    |> CA.TypeVariable
                    |> Ok

        FA.TypeFunction fa ->
            Result.map2
                (\from to ->
                    CA.TypeFunction
                        { from = from
                        , fromIsMutable = Just fa.fromIsMutable
                        , to = to
                        }
                )
                (translateType fa.from)
                (translateType fa.to)

        FA.TypeTuple types ->
            errorTodo "tuples are not supported. Use `pair` for a tuple-2, or a record instead."

        FA.TypeRecord attrs ->
            addAttribute attrs []



----
--- Helpers
--


wrapLambda : FA.Pattern -> List (CA.Statement ()) -> List (CA.Statement ())
wrapLambda (FA.PatternAny paramName) bodyAccum =
    -- TODO start?
    [ { start = 0
      , parameter = paramName
      , body = bodyAccum
      }
        |> CA.Lambda ()
        |> CA.Evaluation
    ]


errorTodo : String -> Res a
errorTodo s =
    Err
        { kind = Error.Whatever s
        , pos = -1
        }


listResultToResultList : List (Result e o) -> Result e (List o)
listResultToResultList =
    firstErrorRec [] >> Result.map List.reverse


firstErrorRec : List o -> List (Result e o) -> Result e (List o)
firstErrorRec os rs =
    case rs of
        [] ->
            Ok os

        (Err e) :: _ ->
            Err e

        (Ok o) :: rt ->
            firstErrorRec (o :: os) rt


maybeResultToResultMaybe : Maybe (Result e o) -> Result e (Maybe o)
maybeResultToResultMaybe maybeResult =
    case maybeResult of
        Nothing ->
            Ok Nothing

        Just (Ok something) ->
            Ok (Just something)

        Just (Err e) ->
            -- Unfortunately Elm doesn't realize that the two `Err e` have the same type
            -- Is it a limit of how pattern matching is implemented?
            Err e
