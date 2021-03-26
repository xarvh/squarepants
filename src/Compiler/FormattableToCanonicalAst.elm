module Compiler.FormattableToCanonicalAst exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Lib
import OneOrMore exposing (OneOrMore)
import SepList exposing (SepList)
import Set exposing (Set)
import Types.CanonicalAst as CA exposing (Pos)
import Types.Error as Error exposing (Res, errorTodo)
import Types.FormattableAst as FA
import Types.Meta exposing (Meta)
import Types.Token as Token


todoPos : Pos
todoPos =
    ( -1, -1 )


{-| `maybeUpdateTarget` is used for record update shorthands:

      new = { old | x = .x + 1 }

    `nonRootValues` is used to keep track of value that are declared within the
    scope of a function, so they don't need to be expanded with the module name.

    `meta` is read-only stuff, used to expand names.

-}
type alias Env =
    { maybeUpdateTarget : Maybe CA.VariableArgs
    , nonRootValues : Set String
    , ro : ReadOnly
    }


type alias ReadOnly =
    { meta : Meta
    , code : String
    , currentModule : String
    }


initEnv : ReadOnly -> Env
initEnv ro =
    { maybeUpdateTarget = Nothing
    , nonRootValues = Set.empty
    , ro = ro
    }


do a b =
    Result.andThen b a



----
--- Module
--


translateModule : ReadOnly -> FA.Module -> CA.Module Pos -> Res (CA.Module Pos)
translateModule ro faModule caModule =
    case faModule of
        [] ->
            Ok caModule

        faStat :: faStatTail ->
            caModule
                |> insertRootStatement ro faStat
                |> Result.andThen (translateModule ro faStatTail)


insertRootStatement : ReadOnly -> FA.Statement -> CA.Module Pos -> Res (CA.Module Pos)
insertRootStatement ro faStatement caModule =
    case faStatement of
        FA.Evaluation _ ->
            errorTodo "Root Evaluations don't really do much =|"

        FA.Definition fa ->
            do (translateDefinition True (initEnv ro) fa) <| \def ->
            case def.pattern of
                CA.PatternAny defName ->
                    let
                        caName =
                            makeRootName ro.currentModule defName
                    in
                    if Dict.member caName caModule then
                        errorTodo <| defName ++ " declared twice!"

                    else
                        caModule
                            |> Dict.insert caName (CA.Value { def | pattern = CA.PatternAny caName })
                            |> Ok

                _ ->
                    errorTodo "patterns can't be used in root definitions!"

        FA.TypeAlias fa ->
            let
                caName =
                    makeRootName ro.currentModule fa.name
            in
            if Dict.member caName caModule then
                errorTodo <| fa.name ++ " declared twice!"

            else if not <| startsWithUpperChar fa.name then
                errorTodo "type name should be uppercase"

            else
                do (translateType ro fa.type_) <| \caType ->
                caModule
                    |> Dict.insert caName
                        (CA.Alias
                            { name = caName
                            , args = fa.args
                            , ty = caType
                            }
                        )
                    |> Ok

        FA.UnionDef fa ->
            let
                caName =
                    makeRootName ro.currentModule fa.name
            in
            if Dict.member caName caModule then
                errorTodo <| fa.name ++ " declared twice!"

            else if not <| startsWithUpperChar fa.name then
                errorTodo "type name should be uppercase"

            else
                do (Lib.list_foldlRes (translateConstructor ro) fa.constructors Dict.empty) <| \constructors ->
                caModule
                    |> Dict.insert caName
                        (CA.Union
                            { name = caName
                            , args = fa.args
                            , constructors = constructors
                            }
                        )
                    |> Ok


translateConstructor : ReadOnly -> FA.Type -> Dict String (List CA.Type) -> Res (Dict String (List CA.Type))
translateConstructor ro faType constructors =
    case faType of
        FA.TypeName { name } ->
            translateConstructor ro (FA.TypePolymorphic { name = name, args = [] }) constructors

        FA.TypePolymorphic polyArgs ->
            do (stringToStructuredName (initEnv ro) {- TODO -} 0 0 polyArgs.name) <| \sname ->
            case sname of
                StructuredName_Value _ ->
                    errorTodo "constructor name must start with a uppercase letter"

                StructuredName_TypeOrCons consArgs ->
                    if consArgs.mod /= NotSpecified then
                        errorTodo "something's wrong with  the cons name"

                    else
                        let
                            name =
                                makeRootName ro.currentModule consArgs.name
                        in
                        if Dict.member name constructors then
                            -- TODO prevent different types from having constructors of the same name!
                            errorTodo <| "constructor " ++ name ++ " is duplicate"

                        else
                            do (Lib.list_mapRes (translateType ro) polyArgs.args) <| \caArgs ->
                            constructors
                                |> Dict.insert name caArgs
                                |> Ok

        _ ->
            errorTodo "either this constructor does not start with a name, either there's something off with the operators"



----
--- Structured Names
--


type StructuredName
    = StructuredName_Value
        { name : String
        , mod : Mod
        , attrPath : List String
        }
    | StructuredName_TypeOrCons
        { name : String
        , mod : Mod
        }


type Mod
    = NotSpecified
    | AlreadyEmbedded
    | ResolvedTo String


resolveName : (Meta -> Dict String String) -> ReadOnly -> Bool -> Mod -> String -> String
resolveName getter ro declaredInsideFunction mod name =
    case mod of
        ResolvedTo modName ->
            makeRootName modName name

        AlreadyEmbedded ->
            name

        NotSpecified ->
            case Dict.get name (getter ro.meta) of
                Just global ->
                    global

                Nothing ->
                    if declaredInsideFunction then
                        name

                    else
                        makeRootName ro.currentModule name


resolveValueName : ReadOnly -> Bool -> Mod -> String -> String
resolveValueName =
    resolveName .globalValues


resolveTypeName : ReadOnly -> Bool -> Mod -> String -> String
resolveTypeName =
    resolveName .globalTypes


makeRootName : String -> String -> String
makeRootName modName defName =
    modName ++ "." ++ defName


stringToStructuredName : Env -> Int -> Int -> String -> Res StructuredName
stringToStructuredName env start end rawString =
    {-
       .attr
       .attr1.attr2

       value
       value.attr1.attr2

       Type
       Constructor

       Module.Type
       Module.Constructor
       Module.value
       Module.value.attr1.attr2

       Dir/Module.Type
       Dir/Module.Constructor
       Dir/Module.value
       Dir/Module.value.attr1.attr2
    -}
    let
        validateAttrPath : List String -> Res (List String)
        validateAttrPath ap =
            if List.any startsWithUpperChar ap then
                errorTodo "record attributes names must start with a lowercase letter"

            else if List.any (String.contains "/") ap then
                errorTodo "`/` can't be used inside attribute names"

            else if List.any ((==) "") ap then
                errorTodo "Weird `..`?"

            else
                Ok ap

        validateDefName : String -> Res String
        validateDefName name =
            -- TODO can't be ""
            if String.contains "/" name then
                errorTodo "value names can't contain `/`"

            else if name == "" then
                errorTodo "weird double dots?"

            else
                Ok name

        translateModName : String -> Res String
        translateModName moduleName =
            Dict.get moduleName env.ro.meta.bynames
                |> Maybe.withDefault moduleName
                |> Ok
    in
    case String.split "." rawString of
        [] ->
            errorTodo "name is empty string !? should not happen"

        --
        -- `.attr`
        -- `.attr1.attr2`
        --
        -- starts with `.`, so it's a record update shorthand
        --
        "" :: tail ->
            case env.maybeUpdateTarget of
                Nothing ->
                    errorRecordUpdateShorthandOutsideRecordUpdate start end rawString env

                Just ref ->
                    do (validateAttrPath tail) <| \tailPath ->
                    { name = ref.name
                    , mod = AlreadyEmbedded
                    , attrPath = ref.attrPath ++ tailPath
                    }
                        |> StructuredName_Value
                        |> Ok

        first :: rest ->
            if not <| startsWithUpperChar first then
                -- `value`
                -- `value.attr1.attr2`
                do (validateAttrPath rest) <| \attrPath ->
                do (validateDefName first) <| \name ->
                { name = name
                , mod = NotSpecified
                , attrPath = attrPath
                }
                    |> StructuredName_Value
                    |> Ok

            else
                case rest of
                    [] ->
                        -- `SomeType`
                        -- `SomeConstructor`
                        do (validateDefName first) <| \name ->
                        { name = name
                        , mod = NotSpecified
                        }
                            |> StructuredName_TypeOrCons
                            |> Ok

                    second :: tail ->
                        if startsWithUpperChar second then
                            -- `Module.Type`
                            -- `Module.Constructor`
                            if tail /= [] then
                                errorTodo "Type or Constructor can't have attributes!"

                            else
                                do (validateDefName second) <| \defName ->
                                do (translateModName first) <| \modName ->
                                { name = defName
                                , mod = ResolvedTo modName
                                }
                                    |> StructuredName_TypeOrCons
                                    |> Ok

                        else
                            -- `Module.value`
                            -- `Module.value.attr1.attr2`
                            do (validateDefName second) <| \defName ->
                            do (translateModName first) <| \modName ->
                            do (validateAttrPath tail) <| \attrPath ->
                            { name = defName
                            , mod = ResolvedTo modName
                            , attrPath = attrPath
                            }
                                |> StructuredName_Value
                                |> Ok



----
--- Definition
--
{-
   translateRootDefinition : FA.ValueDef -> Res (CA.RootDef ())
   translateRootDefinition fa =
       case fa.pattern of
           FA.PatternAny name ->
               Result.map3
                   (\maybeAnnotation body caParameters ->
                       { name = name
                       , maybeAnnotation = maybeAnnotation
                       , body = List.foldr wrapLambda body caParameters
                       }
                   )
                   (translateMaybeAnnotation fa)
                   (translateStatementBlock Nothing fa.body)
                   (Lib.list_mapRes translatePattern fa.parameters)

           _ ->
               errorTodo "Root definitions can't be patterns"
-}


translateDefinition : Bool -> Env -> FA.ValueDef -> Res (CA.ValueDef Pos)
translateDefinition isRoot env fa =
    do (validateFaDefinition fa) <| \_ ->
    do (translateMaybeAnnotation env.ro fa) <| \maybeAnnotation ->
    do (translatePatternOrFunction env fa.pattern) <| \patternOrFunction ->
    let
        ( name, params ) =
            case patternOrFunction of
                Lib.Left p ->
                    ( p, [] )

                Lib.Right ( n, pas ) ->
                    ( CA.PatternAny n, pas )

        additionalNonRootValues =
            if isRoot then
                params

            else
                name :: params

        localEnv =
            { env | nonRootValues = List.foldl (CA.patternNames >> Set.union) env.nonRootValues additionalNonRootValues }
    in
    do (translateStatementBlock localEnv fa.body) <| \body ->
    { pattern = name
    , mutable = fa.mutable
    , maybeAnnotation = maybeAnnotation
    , body = List.foldr wrapLambda body params
    }
        |> Ok


validateFaDefinition : FA.ValueDef -> Res ()
validateFaDefinition fa =
    let
        maybeName =
            case fa.pattern of
                FA.PatternAny n ->
                    Just n

                _ ->
                    Nothing
    in
    case fa.maybeAnnotation of
        Nothing ->
            Ok ()

        Just annotation ->
            if fa.mutable /= annotation.mutable then
                errorTodo "annotation mutability doesn't match definition mutability"

            else
                case maybeName of
                    Nothing ->
                        Ok ()

                    Just name ->
                        if annotation.name /= name then
                            errorTodo "annotation name doesn't match definition name"

                        else
                            Ok ()


translateMaybeAnnotation : ReadOnly -> FA.ValueDef -> Res (Maybe CA.Type)
translateMaybeAnnotation ro fa =
    case fa.maybeAnnotation of
        Nothing ->
            Ok Nothing

        Just annotation ->
            translateType ro annotation.type_
                |> Result.map Just



----
--- Pattern
--


translatePattern : Env -> FA.Pattern -> Res CA.Pattern
translatePattern env faPattern =
    do (translatePatternOrFunction env faPattern) <| \either ->
    case either of
        Lib.Left caPattern ->
            Ok caPattern

        Lib.Right fn ->
            errorTodo "can't declare a function here!"


translatePatternOrFunction : Env -> FA.Pattern -> Res (Lib.Either CA.Pattern ( String, List CA.Pattern ))
translatePatternOrFunction env fa =
    case fa of
        FA.PatternAny s ->
            translatePatternOrFunction env (FA.PatternApplication s [])

        FA.PatternLiteral l ->
            CA.PatternLiteral l
                |> Lib.Left
                |> Ok

        FA.PatternApplication rawName faArgs ->
            do (stringToStructuredName { env | maybeUpdateTarget = Nothing } {- TODO -} 0 0 rawName) <| \sname ->
            do (Lib.list_mapRes (translatePattern env) faArgs) <| \caArgs ->
            case sname of
                StructuredName_TypeOrCons { name, mod } ->
                    CA.PatternConstructor (resolveValueName env.ro False mod name) caArgs
                        |> Lib.Left
                        |> Ok

                StructuredName_Value { name, mod, attrPath } ->
                    if attrPath /= [] then
                        errorTodo "can't use attribute access inside a pattern"

                    else
                        case mod of
                            AlreadyEmbedded ->
                                errorTodo "can't use attribute shorthands inside a pattern"

                            ResolvedTo _ ->
                                errorTodo "It looks like you are trying to reference some module value, but I need just a new variable name"

                            NotSpecified ->
                                -- it's a function or variable!
                                if caArgs == [] then
                                    name
                                        |> CA.PatternAny
                                        |> Lib.Left
                                        |> Ok

                                else
                                    ( name, caArgs )
                                        |> Lib.Right
                                        |> Ok

        FA.PatternList fas ->
            let
                fold pattern last =
                    CA.PatternConstructor Core.listCons.name [ pattern, last ]
            in
            fas
                |> Lib.list_mapRes (translatePattern env)
                |> Result.map (List.foldr fold (CA.PatternConstructor Core.listNil.name []) >> Lib.Left)

        FA.PatternRecord fas ->
            let
                fold ( name, maybePattern ) dict =
                    if Dict.member name dict then
                        errorTodo <| "duplicate attribute name in pattern: " ++ name

                    else
                        case maybePattern of
                            Nothing ->
                                Dict.insert name (CA.PatternAny name) dict |> Ok

                            Just faPattern ->
                                faPattern
                                    |> translatePattern env
                                    |> Result.map (\caPattern -> Dict.insert name caPattern dict)
            in
            Lib.list_foldlRes fold fas Dict.empty
                |> Result.map (CA.PatternRecord >> Lib.Left)

        FA.PatternCons faHead faTail ->
            Result.map2
                (\caHead caTail -> CA.PatternConstructor Core.listCons.name [ caHead, caTail ] |> Lib.Left)
                (translatePattern env faHead)
                (translatePattern env faTail)

        FA.PatternTuple fas ->
            case fas of
                [ fa1, fa2 ] ->
                    [ ( "first", Just fa1 )
                    , ( "second", Just fa2 )
                    ]
                        |> FA.PatternRecord
                        |> translatePatternOrFunction env

                [ fa1, fa2, fa3 ] ->
                    [ ( "first", Just fa1 )
                    , ( "second", Just fa2 )
                    , ( "third", Just fa3 )
                    ]
                        |> FA.PatternRecord
                        |> translatePatternOrFunction env

                _ ->
                    errorTodo "tuples can be only of size 2 or 3"



----
--- Statement
--


insertDefinedNames : Env -> FA.Statement -> Set String -> Set String
insertDefinedNames env stat names =
    case stat of
        FA.Definition fa ->
            -- TODO is there a clean way to avoid translating the patterns twice?
            case translatePatternOrFunction env fa.pattern of
                Err _ ->
                    names

                Ok (Lib.Left caPattern) ->
                    Set.union names (CA.patternNames caPattern)

                Ok (Lib.Right ( fnName, fnArgs )) ->
                    Set.insert fnName names

        _ ->
            names


translateStatementBlock : Env -> OneOrMore FA.Statement -> Res (List (CA.Statement Pos))
translateStatementBlock env oom =
    let
        stats =
            OneOrMore.toList oom

        localEnv =
            { env | nonRootValues = List.foldl (insertDefinedNames env) env.nonRootValues stats }
    in
    Lib.list_mapRes (translateStatement localEnv) stats


translateStatement : Env -> FA.Statement -> Res (CA.Statement Pos)
translateStatement env faStat =
    case faStat of
        FA.Evaluation faExpr ->
            -- TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            -- Debug evaluations should be optimized away in production build
            faExpr
                |> translateExpression env
                |> Result.map CA.Evaluation

        FA.Definition fa ->
            fa
                |> translateDefinition False env
                |> Result.map CA.Definition

        FA.TypeAlias fa ->
            errorTodo "Aliases can be declared only in the root scope"

        FA.UnionDef fa ->
            errorTodo "Types can be declared only in the root scope"



----
--- Expression
--


translateExpression : Env -> FA.Expression -> Res (CA.Expression Pos)
translateExpression env faExpr =
    case faExpr of
        FA.Literal args ->
            Ok <| CA.Literal ( args.start, args.end ) args.value

        FA.Variable args ->
            if args.binop then
                { isRoot = True
                , name = args.name
                , attrPath = []
                }
                    |> CA.Variable ( args.start, args.end )
                    |> Ok

            else
                do (stringToStructuredName env args.start args.end args.name) <| \sname ->
                let
                    ( name, mod, attrPath ) =
                        case sname of
                            StructuredName_Value a ->
                                ( a.name, a.mod, a.attrPath )

                            StructuredName_TypeOrCons a ->
                                ( a.name, a.mod, [] )

                    declaredInsideFunction =
                        Set.member name env.nonRootValues
                in
                { isRoot = not declaredInsideFunction
                , name = resolveValueName env.ro declaredInsideFunction mod name
                , attrPath = attrPath
                }
                    |> CA.Variable ( args.start, args.end )
                    |> Ok

        FA.Lambda fa ->
            let
                ( faHead, faTail ) =
                    fa.parameters
            in
            do (translatePattern env faHead) <| \caHead ->
            do (Lib.list_mapRes (translatePattern env) faTail) <| \caTail ->
            let
                localEnv =
                    { env
                        | nonRootValues =
                            List.foldl (CA.patternNames >> Set.union) env.nonRootValues (caHead :: caTail)
                    }
            in
            do (translateStatementBlock localEnv fa.body) <| \caBody ->
            { parameter = caHead
            , body = List.foldr wrapLambda caBody caTail
            }
                |> CA.Lambda ( fa.start, -1 )
                |> Ok

        FA.FunctionCall s e { reference, arguments } ->
            -- ref arg1 arg2 arg3...
            let
                fold : CA.Argument Pos -> CA.Expression Pos -> CA.Expression Pos
                fold argument refAccum =
                    { reference = refAccum
                    , argument = argument
                    }
                        |> CA.Call ( s, e )
            in
            Result.map2
                (List.foldl fold)
                (translateExpression env reference)
                (arguments
                    |> OneOrMore.toList
                    |> List.map (translateArgument env)
                    |> listResultToResultList
                )

        FA.If { start, condition, true, false } ->
            Result.map3
                (\c t f ->
                    CA.If ( start, -1 )
                        { condition = [ CA.Evaluation c ]
                        , true = t
                        , false = f
                        }
                )
                (translateExpression env condition)
                (translateStatementBlock env true)
                (translateStatementBlock env false)

        FA.Binop { group, sepList } ->
            translateBinops env group sepList

        FA.Lvalue args ->
            errorTodo "mutable values can be used only as arguments for function or mutation operators"

        FA.Record faArgs ->
            do (makeUpdateTarget env faArgs.maybeUpdateTarget) <| \caUpdateTarget ->
            do (translateAttrsRec { env | maybeUpdateTarget = caUpdateTarget.maybeName } faArgs.attrs Dict.empty) <| \caAttrs ->
            { maybeUpdateTarget = caUpdateTarget.maybeName
            , attrs = caAttrs
            }
                |> CA.Record todoPos
                |> caUpdateTarget.wrapper
                |> Ok

        FA.List faItems ->
            let
                cons item list =
                    CA.Call todoPos
                        { reference = CA.Call todoPos { reference = Core.cons, argument = CA.ArgumentExpression item }
                        , argument = CA.ArgumentExpression list
                        }
            in
            faItems
                -- TODO this is more List.reverse than necessary
                |> Lib.list_mapRes (translateExpression env)
                |> Result.map (List.foldr cons Core.nil)

        FA.Try fa ->
            let
                translatePatternAndStatements ( faPattern, faStatements ) =
                    do (translatePattern env faPattern) <| \caPattern ->
                    do (translateStatementBlock { env | nonRootValues = Set.union (CA.patternNames caPattern) env.nonRootValues } faStatements) <| \block ->
                    Ok ( caPattern, block )
            in
            Result.map3
                (\caValue caPatternsAndStatements caElse ->
                    CA.Try ( fa.start, -1 )
                        { value = caValue
                        , patterns = caPatternsAndStatements ++ caElse
                        }
                )
                (translateExpression env fa.value)
                (Lib.list_mapRes translatePatternAndStatements fa.patterns)
                (case fa.maybeElse of
                    Nothing ->
                        Ok []

                    Just faBlock ->
                        translateStatementBlock env faBlock
                            |> Result.map (\caBlock -> [ ( CA.PatternDiscard, caBlock ) ])
                )

        _ ->
            errorTodo <| "FA expression type not supported for now:" ++ Debug.toString faExpr


makeUpdateTarget : Env -> Maybe FA.Expression -> Res { maybeName : Maybe CA.VariableArgs, wrapper : CA.Expression Pos -> CA.Expression Pos }
makeUpdateTarget env maybeUpdateTarget =
    case Maybe.map (translateExpression { env | maybeUpdateTarget = Nothing }) maybeUpdateTarget of
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
    Env
    -> List ( String, Maybe FA.Expression )
    -> Dict String (CA.Expression Pos)
    -> Res (Dict String (CA.Expression Pos))
translateAttrsRec env faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            Ok caAttrsAccum

        ( attrName, maybeAttrExpression ) :: faTail ->
            let
                exprRes =
                    case maybeAttrExpression of
                        Just faExpr ->
                            translateExpression env faExpr

                        Nothing ->
                            let
                                declaredInsideFunction =
                                    Set.member attrName env.nonRootValues
                            in
                            { name = resolveValueName env.ro declaredInsideFunction NotSpecified attrName
                            , isRoot = not declaredInsideFunction
                            , attrPath = []
                            }
                                |> CA.Variable todoPos
                                |> Ok
            in
            do exprRes <| \expr ->
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument : Env -> FA.Expression -> Res (CA.Argument Pos)
translateArgument env faExpr =
    case faExpr of
        FA.Lvalue args ->
            do (stringToStructuredName { env | maybeUpdateTarget = Nothing } args.start args.end args.name) <| \sname ->
            case sname of
                StructuredName_TypeOrCons _ ->
                    errorTodo "constructors can't be mutable?"

                StructuredName_Value { name, mod, attrPath } ->
                    if mod == NotSpecified && Set.member name env.nonRootValues then
                        { isRoot = False
                        , name = name
                        , attrPath = attrPath
                        }
                            |> CA.ArgumentMutable
                            |> Ok

                    else
                        { sname = sname
                        , env = { env | maybeUpdateTarget = Nothing }
                        , rawName = args.name
                        }
                            |> Debug.toString
                            |> (++) "only values declared inside a function scope can be mutated!"
                            |> errorTodo

        _ ->
            faExpr
                |> translateExpression env
                |> Result.map CA.ArgumentExpression


translateBinops : Env -> Token.PrecedenceGroup -> SepList String FA.Expression -> Res (CA.Expression Pos)
translateBinops env group ( firstItem, firstTail ) =
    case firstTail of
        [] ->
            translateExpression env firstItem

        ( firstSep, secondItem ) :: [] ->
            case group of
                Token.Tuple ->
                    Result.map2
                        (\first second ->
                            CA.Record todoPos
                                { maybeUpdateTarget = Nothing
                                , attrs =
                                    Dict.fromList
                                        [ ( "first", first )
                                        , ( "second", second )
                                        ]
                                }
                        )
                        (translateExpression env firstItem)
                        (translateExpression env secondItem)

                _ ->
                    translateSimpleBinop env firstItem firstSep secondItem

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
                        translateBinopSepList env firstItem firstTail

                Token.Tuple ->
                    if thirdTail /= [] then
                        errorTodo "Tuples can't have more than 3 items, use a record instead."

                    else
                        Result.map3
                            (\first second third ->
                                CA.Record todoPos
                                    { maybeUpdateTarget = Nothing
                                    , attrs =
                                        Dict.fromList
                                            [ ( "first", first )
                                            , ( "second", second )
                                            , ( "third", third )
                                            ]
                                    }
                            )
                            (translateExpression env firstItem)
                            (translateExpression env secondItem)
                            (translateExpression env thirdItem)

                Token.Pipe ->
                    if notAllSeparators ((==) firstSep) secondTail then
                        errorTodo "Mixing pipes is ambigous. Use parens."

                    else
                        translateBinopSepList env firstItem firstTail

                Token.Mutop ->
                    errorTodo "mutops can't be chained"

                _ ->
                    translateBinopSepList env firstItem firstTail


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


translateBinopSepList : Env -> FA.Expression -> List ( String, FA.Expression ) -> Res (CA.Expression Pos)
translateBinopSepList env leftAccum opsAndRight =
    do (translateExpression env leftAccum) <| \caLeftAccum ->
    translateBinopSepListRec env caLeftAccum opsAndRight


translateBinopSepListRec : Env -> CA.Expression Pos -> List ( String, FA.Expression ) -> Res (CA.Expression Pos)
translateBinopSepListRec env leftAccum opsAndRight =
    case opsAndRight of
        [] ->
            Ok leftAccum

        ( op, faRight ) :: tail ->
            case translateArgument env faRight of
                Err e ->
                    Err e

                Ok caRight ->
                    translateBinopSepListRec env (makeBinop (CA.ArgumentExpression leftAccum) op caRight) tail


{-| Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

-}
makeBinop : CA.Argument Pos -> String -> CA.Argument Pos -> CA.Expression Pos
makeBinop left op right =
    case ( left, op, right ) of
        -- TODO don't hardcode the strings, use instead those defined in Prelude
        ( _, ">>", CA.ArgumentExpression rightExpr ) ->
            CA.Call todoPos
                { argument = left
                , reference = rightExpr
                }

        ( CA.ArgumentExpression leftExpr, "<<", _ ) ->
            CA.Call todoPos
                { argument = right
                , reference = leftExpr
                }

        _ ->
            CA.Call todoPos
                { argument = left
                , reference =
                    CA.Call todoPos
                        { argument = right
                        , reference =
                            CA.Variable todoPos
                                { isRoot = True
                                , name = op
                                , attrPath = []
                                }
                        }
                }


translateSimpleBinop : Env -> FA.Expression -> String -> FA.Expression -> Res (CA.Expression Pos)
translateSimpleBinop env left op right =
    Result.map2 (\l r -> makeBinop l op r)
        (translateArgument env left)
        (translateArgument env right)



----
--- Type
--


addAttribute : ReadOnly -> List ( String, FA.Type ) -> Dict String CA.Type -> Res CA.Type
addAttribute ro faAttrs caAttrsAccum =
    case faAttrs of
        [] ->
            { attrs = caAttrsAccum
            , extensible = Nothing
            }
                |> CA.TypeRecord
                |> Ok

        ( name, faType ) :: faTail ->
            case translateType ro faType of
                Err e ->
                    -- TODO add `name` in the error?
                    Err e

                Ok caType ->
                    addAttribute ro faTail (Dict.insert name caType caAttrsAccum)


startsWithUpperChar : String -> Bool
startsWithUpperChar s =
    case String.uncons s of
        Nothing ->
            False

        Just ( head, tail ) ->
            Char.isUpper head


translateType : ReadOnly -> FA.Type -> Res CA.Type
translateType ro faType =
    case faType of
        FA.TypeName { name } ->
            translateType ro <| FA.TypePolymorphic { name = name, args = [] }

        FA.TypePolymorphic a ->
            do (stringToStructuredName (initEnv ro) {- TODO -} 0 0 a.name) <| \sname ->
            case sname of
                StructuredName_Value { name, mod, attrPath } ->
                    if a.args /= [] then
                        -- TODO is this the correct error?
                        errorTodo "rank 2 types are not supported"

                    else if mod /= NotSpecified then
                        errorTodo "this is not a valid name for a type variable"

                    else if attrPath /= [] then
                        errorTodo "no attribute accessors on types"

                    else
                        { name = name }
                            |> CA.TypeVariable
                            |> Ok

                StructuredName_TypeOrCons { name, mod } ->
                    do (Lib.list_mapRes (translateType ro) a.args) <| \caArgs ->
                    { ref = resolveTypeName ro False mod name
                    , args = caArgs
                    }
                        |> CA.TypeConstant
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
                (translateType ro fa.from)
                (translateType ro fa.to)

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
                        (translateType ro faFirst)
                        (translateType ro faSecond)

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
                        (translateType ro faFirst)
                        (translateType ro faSecond)
                        (translateType ro faThird)

                _ ->
                    errorTodo "Tuples can only have size 2 or 3. Use a record."

        FA.TypeRecord attrs ->
            addAttribute ro attrs Dict.empty



----
--- Helpers
--


wrapLambda : CA.Pattern -> List (CA.Statement Pos) -> List (CA.Statement Pos)
wrapLambda pattern bodyAccum =
    [ { parameter = pattern
      , body = bodyAccum
      }
        |> CA.Lambda todoPos
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



----
--- Errors
--


errorRecordUpdateShorthandOutsideRecordUpdate : Int -> Int -> String -> Env -> Res a
errorRecordUpdateShorthandOutsideRecordUpdate start end rawString env =
    Error.makeRes
        env.ro.currentModule
        [ Error.showLines env.ro.code 2 start
        , Error.text <| Error.inlineCode rawString ++ " looks like a record update shorthand, but we are not inside a record update!"
        ]
