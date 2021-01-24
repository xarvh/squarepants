module Compiler.FormattableToCanonicalAst exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Lib
import OneOrMore exposing (OneOrMore)
import SepList exposing (SepList)
import Set exposing (Set)
import Types.CanonicalAst as CA
import Types.Error as Error exposing (Error, Res, errorTodo)
import Types.FormattableAst as FA
import Types.Token as Token


{-| Record Shorthand

      new = { old | x = .x + 1 }

-}
type alias Rs =
    Maybe CA.VariableArgs


do a b =
    Result.andThen b a



----
--- Module
--


translateModule : FA.Module -> Res (CA.Module ())
translateModule faModule =
    translateModuleRec
        faModule
        { unions = Dict.empty
        , values = Dict.empty
        , aliases = Dict.empty
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
            do (translateDefinition Nothing fa) <| \def ->
            if Dict.member def.name caModule.values then
                errorTodo <| def.name ++ " declared twice!"

            else
                Ok { caModule | values = Dict.insert def.name def caModule.values }

        FA.TypeAlias fa ->
            if Dict.member fa.name caModule.unions then
                errorTodo <| fa.name ++ " declared twice!"

            else
                do (translateType fa.type_) <| \caType ->
                let
                    al =
                        { name = fa.name
                        , args = fa.args
                        , ty = caType
                        }
                in
                Ok { caModule | aliases = Dict.insert al.name al caModule.aliases }

        FA.UnionDef fa ->
            if Dict.member fa.name caModule.unions then
                errorTodo <| fa.name ++ " declared twice!"

            else if not <| firstCharIsUpper fa.name then
                errorTodo "type name should be uppercase"

            else
                let
                    constructorListToModule : List CA.UnionConstructor -> CA.Module ()
                    constructorListToModule consList =
                        { caModule
                            | unions =
                                Dict.insert
                                    fa.name
                                    { name = fa.name
                                    , args = fa.args
                                    , constructors = consList
                                    }
                                    caModule.unions
                        }
                in
                fa.constructors
                    |> Lib.list_mapRes translateConstructor
                    |> Result.andThen errorOnDuplicateConstructorNames
                    |> Result.map constructorListToModule


translateConstructor : FA.Type -> Res CA.UnionConstructor
translateConstructor faType =
    case faType of
        FA.TypeName { name } ->
            translateConstructor (FA.TypePolymorphic { name = name, args = [] })

        FA.TypePolymorphic { name, args } ->
            if not <| firstCharIsUpper name then
                errorTodo "constructor name must start with a uppercase letter"

            else
                args
                    |> Lib.list_mapRes translateType
                    |> Result.map
                        (\caArgs ->
                            { name = name
                            , args = caArgs
                            }
                        )

        _ ->
            errorTodo "either this constructor does not start with a name, either there's something off with the operators"


errorOnDuplicateConstructorNames : List CA.UnionConstructor -> Res (List CA.UnionConstructor)
errorOnDuplicateConstructorNames constructors =
    let
        names =
            List.map .name constructors
    in
    if Set.size (Set.fromList names) < List.length names then
        errorTodo "duplicate constructor names"

    else
        Ok constructors



----
--- Structured Names
--


type alias StructuredName =
    { isUpper : Bool
    , path : List CA.Name
    , attrPath : List CA.Name
    }


stringToStructuredName : Rs -> String -> Res StructuredName
stringToStructuredName rs s =
    let
        resStrings =
            case String.split "." s of
                "" :: ls ->
                    case rs of
                        Nothing ->
                            errorTodo "record update shorthands can be used only inside a record update!"

                        Just ref ->
                            Ok <| ref.path :: ref.attrPath ++ ls

                ls ->
                    Ok ls
    in
    case resStrings of
        Err e ->
            Err e

        Ok strings ->
            case uppercaseRec strings [] of
                Err e ->
                    errorTodo e

                Ok ( [], [] ) ->
                    errorTodo "WTF!?"

                Ok ( reversedPath, [] ) ->
                    Ok
                        { isUpper = True
                        , path = List.reverse reversedPath
                        , attrPath = []
                        }

                Ok ( reversedPath, name :: attrPath ) ->
                    Ok
                        { isUpper = False
                        , path = List.reverse (name :: reversedPath)
                        , attrPath = attrPath
                        }


uppercaseRec : List String -> List String -> Result String ( List String, List String )
uppercaseRec ls uppercaseAcc =
    case ls of
        [] ->
            Ok ( uppercaseAcc, [] )

        head :: tail ->
            if head == "" then
                Err "Not sure what to make of the dot here"

            else if firstCharIsUpper head then
                uppercaseRec tail (head :: uppercaseAcc)

            else
                lowercaseRec uppercaseAcc [ head ] tail


lowercaseRec : List String -> List String -> List String -> Result String ( List String, List String )
lowercaseRec uppercaseAcc lowercaseAcc ls =
    case ls of
        [] ->
            Ok ( uppercaseAcc, List.reverse lowercaseAcc )

        head :: tail ->
            if head == "" then
                Err "Not sure what to make of the dot here"

            else if firstCharIsUpper head then
                Err "I was expecting a lower case here"

            else
                lowercaseRec uppercaseAcc (head :: lowercaseAcc) tail



----
--- Definition
--


translateDefinition : Rs -> FA.ValueDef -> Res (CA.ValueDef ())
translateDefinition rs fa =
    let
        (FA.PatternAny name) =
            fa.name

        resultMaybeAnnotation =
            fa.maybeAnnotation
                |> Maybe.map (translateAnnotation name fa)
                |> maybeResultToResultMaybe
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
        (translateStatementBlock rs fa.body)


translateAnnotation : String -> FA.ValueDef -> FA.Annotation -> Res CA.Type
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


translateStatementBlock : Rs -> OneOrMore FA.Statement -> Res (List (CA.Statement ()))
translateStatementBlock rs oom =
    oom
        |> OneOrMore.toList
        -- ugly but works, maybe write it better once we can bootstrap
        |> List.map (translateStatement rs)
        |> listResultToResultList


translateStatement : Rs -> FA.Statement -> Res (CA.Statement ())
translateStatement rs faStat =
    case faStat of
        FA.Evaluation faExpr ->
            -- TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            -- Debug evaluations should be optimized away in production build
            faExpr
                |> translateExpression rs
                |> Result.map CA.Evaluation

        FA.Definition fa ->
            fa
                |> translateDefinition rs
                |> Result.map CA.Definition

        FA.TypeAlias fa ->
            errorTodo "Aliases can be declared only in the root scope"

        FA.UnionDef fa ->
            errorTodo "Types can be declared only in the root scope"



----
--- Expression
--


translateExpression : Rs -> FA.Expression -> Res (CA.Expression ())
translateExpression rs faExpr =
    case faExpr of
        FA.NumberLiteral args ->
            Ok <| CA.NumberLiteral () args

        FA.Variable args ->
            args.name
                |> stringToStructuredName rs
                |> Result.map
                    (\sname ->
                        CA.Variable ()
                            { start = args.start
                            , end = args.end
                            , path = String.join "." sname.path
                            , attrPath = sname.attrPath
                            }
                    )

        FA.Lambda fa ->
            let
                ( FA.PatternAny paramsHead, paramsTail ) =
                    fa.parameters
            in
            fa.body
                |> translateStatementBlock rs
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
                (translateExpression rs reference)
                (arguments
                    |> OneOrMore.toList
                    |> List.map (translateArgument rs)
                    |> listResultToResultList
                )

        FA.If { start, condition, true, false } ->
            Result.map3
                (\c t f ->
                    CA.If ()
                        { start = start
                        , condition = [ CA.Evaluation c ]
                        , true = t
                        , false = f
                        }
                )
                (translateExpression rs condition)
                (translateStatementBlock rs true)
                (translateStatementBlock rs false)

        FA.Binop { group, sepList } ->
            translateBinops rs group sepList

        FA.Lvalue args ->
            errorTodo "mutable values can be used only as arguments for function or mutation operators"

        FA.Record faArgs ->
            do (makeUpdateTarget faArgs.maybeUpdateTarget) <| \caUpdateTarget ->
            do (translateAttrsRec caUpdateTarget.maybeName faArgs.attrs Dict.empty) <| \caAttrs ->
            { maybeUpdateTarget = caUpdateTarget.maybeName
            , attrs = caAttrs
            }
                |> CA.Record ()
                |> caUpdateTarget.wrapper
                |> Ok

        FA.List faItems ->
            let
                cons item list =
                    CA.Call ()
                        { reference = CA.Call () { reference = Core.cons, argument = CA.ArgumentExpression item }
                        , argument = CA.ArgumentExpression list
                        }
            in
            faItems
                -- TODO this is more List.reverse than necessary
                |> Lib.list_mapRes (translateExpression rs)
                |> Result.map (List.foldr cons Core.nil)

        _ ->
            errorTodo <| "FA expression type not supported for now:" ++ Debug.toString faExpr


makeUpdateTarget : Maybe FA.Expression -> Res { maybeName : Rs, wrapper : CA.Expression () -> CA.Expression () }
makeUpdateTarget maybeUpdateTarget =
    case Maybe.map (translateExpression Nothing) maybeUpdateTarget of
        Nothing ->
            Ok { maybeName = Nothing, wrapper = identity }

        Just (Err e) ->
            Err e

        Just (Ok (CA.Variable _ args)) ->
            -- TODO test for lowercase name?
            Ok { maybeName = Just args, wrapper = identity }

        Just (Ok expr) ->
            errorTodo "NI { (expr) with ...} not yet implemented =("


translateAttrsRec :
    Rs
    -> List ( String, Maybe FA.Expression )
    -> Dict CA.Name (CA.Expression ())
    -> Res (Dict CA.Name (CA.Expression ()))
translateAttrsRec maybeUpdateTarget faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            Ok caAttrsAccum

        ( attrName, maybeAttrExpression ) :: faTail ->
            let
                exprRes =
                    case maybeAttrExpression of
                        Just faExpr ->
                            translateExpression maybeUpdateTarget faExpr

                        Nothing ->
                            -- TODO start, end
                            { start = 0
                            , end = 0
                            , path = attrName
                            , attrPath = []
                            }
                                |> CA.Variable ()
                                |> Ok
            in
            do exprRes <| \expr ->
            translateAttrsRec maybeUpdateTarget faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument : Rs -> FA.Expression -> Res (CA.Argument ())
translateArgument rs faExpr =
    case faExpr of
        FA.Lvalue args ->
            do (stringToStructuredName Nothing args.name) <| \sname ->
            { start = args.start
            , end = args.end
            , path = String.join "." sname.path
            , attrPath = sname.attrPath
            }
                |> CA.ArgumentMutable
                |> Ok

        _ ->
            faExpr
                |> translateExpression rs
                |> Result.map CA.ArgumentExpression


translateBinops : Rs -> Token.PrecedenceGroup -> SepList String FA.Expression -> Res (CA.Expression ())
translateBinops rs group ( firstItem, firstTail ) =
    case firstTail of
        [] ->
            translateExpression rs firstItem

        ( firstSep, secondItem ) :: [] ->
            case group of
                Token.Tuple ->
                    Result.map2
                        (\first second ->
                            CA.Record ()
                                { maybeUpdateTarget = Nothing
                                , attrs =
                                    Dict.fromList
                                        [ ( "first", first )
                                        , ( "second", second )
                                        ]
                                }
                        )
                        (translateExpression rs firstItem)
                        (translateExpression rs secondItem)

                _ ->
                    translateSimpleBinop rs firstItem firstSep secondItem

        ( firstSep, secondItem ) :: ( secondSep, thirdItem ) :: thirdTail ->
            let
                -- Use "as"? Yay undocumented syntax...
                secondTail =
                    ( secondSep, thirdItem ) :: thirdTail
            in
            case group of
                Token.Comparison ->
                    if notAllSeparators (sameDirectionAs firstSep) secondTail then
                        -- TODO actually list the seps
                        errorTodo "can't mix comparison ops with different direction"

                    else
                        -- TODO expand `a < b < c` to `a < b && b < c` without calculating b twice
                        errorTodo "NI compops expansion"

                Token.Logical ->
                    if notAllSeparators ((==) firstSep) secondTail then
                        errorTodo "Mixing `and` and `or` is ambiguous. Use parens!"

                    else
                        translateBinopSepList rs firstItem firstTail

                Token.Tuple ->
                    if thirdTail /= [] then
                        errorTodo "Tuples can't have more than 3 items, use a record instead."

                    else
                        Result.map3
                            (\first second third ->
                                CA.Record ()
                                    { maybeUpdateTarget = Nothing
                                    , attrs =
                                        Dict.fromList
                                            [ ( "first", first )
                                            , ( "second", second )
                                            , ( "third", third )
                                            ]
                                    }
                            )
                            (translateExpression rs firstItem)
                            (translateExpression rs secondItem)
                            (translateExpression rs thirdItem)

                Token.Pipe ->
                    if notAllSeparators ((==) firstSep) secondTail then
                        errorTodo "Mixing pipes is ambigous. Use parens."

                    else
                        translateBinopSepList rs firstItem firstTail

                Token.Mutop ->
                    errorTodo "mutops can't be chained"

                _ ->
                    translateBinopSepList rs firstItem firstTail


notAllSeparators : (sep -> Bool) -> List ( sep, item ) -> Bool
notAllSeparators f ls =
    case ls of
        [] ->
            False

        ( sep, item ) :: tail ->
            if f sep then
                notAllSeparators f tail

            else
                True


sameDirectionAs : String -> String -> Bool
sameDirectionAs a b =
    if a == b then
        True

    else
        case a of
            ">" ->
                b == ">="

            ">=" ->
                b == ">"

            "<" ->
                b == "<="

            "<=" ->
                b == "<"

            _ ->
                False


translateBinopSepList : Rs -> FA.Expression -> List ( String, FA.Expression ) -> Res (CA.Expression ())
translateBinopSepList rs leftAccum opsAndRight =
    do (translateExpression rs leftAccum) <| \caLeftAccum ->
    translateBinopSepListRec rs caLeftAccum opsAndRight


translateBinopSepListRec : Rs -> CA.Expression () -> List ( String, FA.Expression ) -> Res (CA.Expression ())
translateBinopSepListRec rs leftAccum opsAndRight =
    case opsAndRight of
        [] ->
            Ok leftAccum

        ( op, faRight ) :: tail ->
            case translateArgument rs faRight of
                Err e ->
                    Err e

                Ok caRight ->
                    translateBinopSepListRec rs (makeBinop (CA.ArgumentExpression leftAccum) op caRight) tail


{-| Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

-}
makeBinop : CA.Argument () -> String -> CA.Argument () -> CA.Expression ()
makeBinop left op right =
    CA.Call ()
        { argument = left
        , reference =
            CA.Call ()
                { argument = right
                , reference =
                    CA.Variable ()
                        -- TODO start, end!!
                        { start = 0
                        , end = 0
                        , path = op
                        , attrPath = []
                        }
                }
        }


translateSimpleBinop : Rs -> FA.Expression -> String -> FA.Expression -> Res (CA.Expression ())
translateSimpleBinop rs left op right =
    Result.map2 (\l r -> makeBinop l op r)
        (translateArgument rs left)
        (translateArgument rs right)



----
--- Type
--


addAttribute : List ( String, FA.Type ) -> Dict String CA.Type -> Res CA.Type
addAttribute faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            { attrs = caAttrsAccum
            , extensible = Nothing
            }
                |> CA.TypeRecord
                |> Ok

        ( name, faType ) :: faTail ->
            case translateType faType of
                Err e ->
                    -- TODO add `name` in the error?
                    Err e

                Ok caType ->
                    addAttribute faTail (Dict.insert name caType caAttrsAccum)


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
        FA.TypeName { name } ->
            translateType <| FA.TypePolymorphic { name = name, args = [] }

        FA.TypePolymorphic { name, args } ->
            do (stringToStructuredName Nothing name) <| \sname ->
            if sname.attrPath /= [] then
                errorTodo "no attribute accessors on types"

            else if sname.isUpper then
                args
                    |> List.map translateType
                    |> listResultToResultList
                    |> Result.map
                        (\caArgs ->
                            CA.TypeConstant
                                { path = String.join "." sname.path
                                , args = caArgs
                                }
                        )

            else if args /= [] then
                -- TODO is this the correct error?
                errorTodo "rank 2 types are not supported"

            else if List.length sname.path /= 1 then
                errorTodo "this is not a valid name for a type variable"

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
            case types of
                [ faFirst, faSecond ] ->
                    Result.map2
                        (\caFirst caSecond ->
                            CA.TypeRecord
                                { extensible = Nothing
                                , attrs =
                                    Dict.fromList
                                        [ ( "first", caFirst )
                                        , ( "second", caSecond )
                                        ]
                                }
                        )
                        (translateType faFirst)
                        (translateType faSecond)

                [ faFirst, faSecond, faThird ] ->
                    Result.map3
                        (\caFirst caSecond caThird ->
                            CA.TypeRecord
                                { extensible = Nothing
                                , attrs =
                                    Dict.fromList
                                        [ ( "first", caFirst )
                                        , ( "second", caSecond )
                                        , ( "third", caThird )
                                        ]
                                }
                        )
                        (translateType faFirst)
                        (translateType faSecond)
                        (translateType faThird)

                _ ->
                    errorTodo "Tuples can only have size 2 or 3. Use a record."

        FA.TypeRecord attrs ->
            addAttribute attrs Dict.empty



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
