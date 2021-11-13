

[#

    * `maybeUpdateTarget` is used for record update shorthands:

      new = { old with x = .x + 1 }

    * `nonRootValues` is used to keep track of value that are declared within the
    scope of a function, so they don't need to be expanded with the module name.

    * `meta` is read-only stuff, used to expand names.

#]
alias Env =
    { maybeUpdateTarget as Maybe CA.VariableArgs
    , nonRootValues as Dict String CA.Pos
    , ro as ReadOnly
    , defsPath as [CA.Pattern]
    , tyvarTranslations as Dict String String
    }


alias ReadOnly =
    { meta as Meta
    , code as String
    , currentModule as String
    }


initEnv ro =
    as ReadOnly: Env
    { maybeUpdateTarget = Nothing
    , nonRootValues = Dict.empty
    , ro = ro
    , defsPath = []
    , tyvarTranslations = Dict.empty
    }


# TODO: if it works, we can change andThen to onOk
onOk =
    Result.andThen


#
# Errors
#


makeError pos msg =
    as Pos: [Text]: Error
    Error.res pos fn errorEnv: msg


#
# Module
#


# TODO: run two passes: the first one to populate the env with the root names (and check they are not duplicate)
# and the second to actually work through the value definitions
[#
                # check that the same names aren't used somewhere else
                duplicateNames =
                    def.pattern
                        >> CA.patternNames
                        >> Dict.intersect caModule.valueNames
                if duplicateNames /= Dict.empty:
                    # TODO: give the clashing names positions
                    makeError (CA.patternPos def.pattern) [ "Duplicate names: " .. Text.join ", " (Dict.keys duplicateNames) ]
                else
#]


translateModule ro faModule caModule =
    as ReadOnly: FA.Module: CA.AllDefs: Res CA.AllDefs
    try faModule as
        []:
            Ok caModule

        faStat :: faStatTail:
            caModule
                >> insertRootStatement ro faStat
                >> onOk (translateModule ro faStatTail)


insertRootStatement ro faStatement caModule =
    as ReadOnly: FA.Statement: CA.Module: Res CA.Module
    try faStatement as
        FA.Evaluation expr:
            makeError (CA.expressionPos expr) [ "Root Evaluations don't really do much =|" ]

        FA.Definition fa:
            translateDefinition True (initEnv ro) fa >> onOk fn def:
            if def.mutable:
                makeError (CA.patternPos def.pattern) [ "Mutable values can be declared only inside functions." ]

            else
                try def.pattern & def.maybeAnnotation as
                    CA.PatternAny pos name & Just type:
                        try Dict.get name caModule.exposedValues as
                            Just exposedValue:
                                # TODO add location of old place
                                makeError pos [ "there is already an exposed value called `" .. name .. "`" ]
                            Nothing:
                                { caModule with
                                , exposedValues = Dict.insert name { valueDefsKey = def.pattern, type } .exposedValues
                                , valueDefs = Dict.insert def.pattern def .valueDefs
                                }
                                    >> Ok
                    _:
                        { caModule with
                        , valueDefs = Dict.insert def.pattern def .valueDefs
                        }
                            >> Ok

        FA.TypeAlias fa:
            At name pos =
                fa.name

            if Dict.member name caModule.aliases or Dict.member name caModule.unions:
                makeError pos [ name .. " declared twice!" ]

            else if not startsWithUpperChar name:
                makeError pos "type name should be uppercase"

            else
                translateType ro fa.ty >> onOk fn type:
                Ok { caModule with aliases = Dict.insert name { name, args = fa.args, type } .aliases }

        FA.UnionDef fa:
            At name pos =
                fa.name

            if Dict.member name caModule.aliases or Dict.member name caModule.unions:
                makeError pos [ name .. " declared twice!" ]

            else if not startsWithUpperChar fa.name:
                makeError pos [ "type name should be uppercase" ]

            else
                Lib.list_foldlRes (translateConstructor ro) fa.constructors Dict.empty >> onOk fn constructors:
                Ok { caModule with unions = Dict.insert name { name, args = fa.args, constructors } .unions }


translateConstructor ro faType constructors =
    as ReadOnly: FA.Type: Dict String [CA.Type]: Res (Dict String [CA.Type])

    try faType as
        FA.TypeName pos name:
            translateConstructor ro (FA.TypePolymorphic pos name []) constructors

        FA.TypePolymorphic pos fa_name fa_args:
            stringToStructuredName (initEnv ro) pos fa_name >> onOk fn sname:
            try sname as
                StructuredName_Value _:
                    makeError pos [ "constructor name must start with a uppercase letter" ]

                StructuredName_TypeOrCons consArgs:
                    if consArgs.mod /= NotSpecified:
                        makeError [ "something's wrong with  the cons name" ]

                    else
                        name =
                            consArgs.name

                        if Dict.member name constructors:
                            # TODO prevent different types from having constructors of the same name!
                            makeError pos [ "constructor " .. name .. " is duplicate" ]

                        else
                            Lib.list_mapRes (translateType ro) fa_args >> onOk fn caArgs:
                            constructors
                                >> Dict.insert name caArgs
                                >> Ok

        _:
            errorTodo "either this constructor does not start with a name, either there's something off with the operators"



##
# Structured Names
#


union StructuredName =
    , StructuredName_Value { name as String , mod as Mod , attrPath as List String }
    , StructuredName_TypeOrCons { name as String , mod as Mod }


union Mod =
    , NotSpecified
    , AlreadyEmbedded
    , ResolvedTo String


resolveName getter ro declaredInsideFunction mod name =
    as (Meta: Dict String String): ReadOnly: Bool: Mod: String: String
    try mod as
        ResolvedTo modName:
            makeRootName modName name

        AlreadyEmbedded:
            name

        NotSpecified:
            try Dict.get name (getter ro.meta) as
                Just global:
                    global

                Nothing:
                    if declaredInsideFunction:
                        name

                    else
                        makeRootName ro.currentModule name


resolveValueName =
    as ReadOnly: Bool: Mod: String: String
    resolveName (fn x: x.globalValues)


resolveTypeName =
    as ReadOnly: Bool: Mod: String: String
    resolveName (fn x: x.globalTypes)


makeRootName modName defName =
    as String: String: String
    modName .. "." .. defName


stringToStructuredName env pos rawString =
    as Env: FA.Pos: String: Res StructuredName
    [#
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
    #]
    validateAttrPath ap =
        as List String: Res (List String)
        if List.any startsWithUpperChar ap:
            errorTodo "record attributes names must start with a lowercase letter"

        else if List.any (String.contains "/") ap:
            errorTodo "`/` can't be used inside attribute names"

        else if List.any ((==) "") ap:
            errorTodo "Weird `..`?"

        else
            Ok ap

    validateDefName name =
        as String: Res String
        # TODO can't be ""
        if String.contains "/" name:
            errorTodo "value names can't contain `/`"

        else if name == "":
            errorTodo "weird double dots?"

        else
            Ok name

    translateModName moduleName =
        as String: Res String
        Dict.get moduleName env.ro.meta.bynames
            >> Maybe.withDefault moduleName
            >> Ok

    try String.split "." rawString as
        []:
            errorTodo "name is empty string !? should not happen"

        #
        # `.attr`
        # `.attr1.attr2`
        #
        # starts with `.`, so it's a record update shorthand
        #
        "" :: tail:
            try env.maybeUpdateTarget as
                Nothing:
                    errorRecordUpdateShorthandOutsideRecordUpdate pos rawString env

                Just ref:
                    validateAttrPath tail >> onOk fn tailPath:
                    { name = ref.name
                    , mod = AlreadyEmbedded
                    , attrPath = ref.attrPath .. tailPath
                    }
                        >> StructuredName_Value
                        >> Ok

        first :: rest:
            if not startsWithUpperChar first:
                # `value`
                # `value.attr1.attr2`
                validateAttrPath rest >> onOk fn attrPath:
                validateDefName first >> onOk fn name:
                { name = name
                , mod = NotSpecified
                , attrPath = attrPath
                }
                    >> StructuredName_Value
                    >> Ok

            else
                try rest as
                    []:
                        # `SomeType`
                        # `SomeConstructor`
                        validateDefName first >> onOk fn name:
                        { name = name
                        , mod = NotSpecified
                        }
                            >> StructuredName_TypeOrCons
                            >> Ok

                    second :: tail:
                        if startsWithUpperChar second:
                            # `Module.Type`
                            # `Module.Constructor`
                            if tail /= []:
                                Error.faSimple env.ro.currentModule pos "Type or Constructor can't have attributes!"

                            else
                                validateDefName second >> onOk fn defName:
                                translateModName first >> onOk fn modName:
                                { name = defName
                                , mod = ResolvedTo modName
                                }
                                    >> StructuredName_TypeOrCons
                                    >> Ok

                        else
                            # `Module.value`
                            # `Module.value.attr1.attr2`
                            validateDefName second >> onOk fn defName:
                            translateModName first >> onOk fn modName:
                            validateAttrPath tail >> onOk fn attrPath:
                            { name = defName
                            , mod = ResolvedTo modName
                            , attrPath = attrPath
                            }
                                >> StructuredName_Value
                                >> Ok



##
#- Definition
#


insertParamNames param =
    as CA.Parameter: Dict String CA.Pos: Dict String CA.Pos
    try param as
        CA.ParameterMutable pos n:
            Dict.insert n pos

        CA.ParameterPattern pa:
            CA.patternNames pa >> Dict.union


translateDefinition isRoot env fa =
    as Bool: Env: FA.ValueDef: Res CA.ValueDef
    translatePatternOrFunction env fa.pattern >> onOk fn patternOrFunction:

    namePattern & params =
        try patternOrFunction as
            POF_Pattern p:
                p & []

            POF_Function n pas:
                CA.PatternAny (tp env.ro fa.pos) n & pas

    nonRootValues1 =
        if isRoot:
            env.nonRootValues

        else
            Dict.join (CA.patternNames namePattern) env.nonRootValues

    localEnv0 =
        { env with
            , nonRootValues = List.foldl insertParamNames nonRootValues1 params
            , defsPath = namePattern :: .defsPath
        }

    # translateMaybeAnnotation adds scoped tyvars to env
    translateMaybeAnnotation localEnv0 fa >> onOk fn ( localEnv1 & maybeAnnotation ):
    translateStatementBlock localEnv1 fa.body >> onOk fn body:
    Ok
        { pattern = namePattern
        , native = False
        , mutable = fa.mutable
        , parentDefinitions = env.defsPath
        , maybeAnnotation = maybeAnnotation
        , body = List.foldr (wrapLambda env.ro fa.pos) body params
        }


translateMaybeAnnotation env0 fa =
    as Env: FA.ValueDef: Res ( Env & Maybe CA.Annotation )
    try fa.maybeAnnotation as
        Nothing:
            Ok ( env0 & Nothing )

        Just annotation:
            translateType env0.ro annotation.ty >> onOk fn rawTy:
            thing =
                M.do (translateTyvars rawTy) >> onOk fn ty_:
                M.do (M.list_map translateTyvarName annotation.nonFn) >> onOk fn nonFn_:
                M.return ( ty_ & nonFn_ )

            ( ty & nonFn ) & env1  =
                M.run env0 thing

            # TODO check that nonFn contains only variables acutally present in ty
            Ok << ( env1 & Just { pos = tp env1.ro annotation.pos , ty = ty , nonFn = List.foldl (fn v: Dict.insert v CA.F) Dict.empty nonFn })


type alias EnvMonad a =
    M Env a


translateTyvars type_ =
    as CA.Type: EnvMonad CA.Type
    try type_ as
        CA.TypeConstant p ref args:
            M.do (M.list_map translateTyvars args) >> onOk fn a:
            M.return << CA.TypeConstant p ref a

        CA.TypeFunction p from fromIsMutable to:
            M.do (translateTyvars from) >> onOk fn f:
            M.do (translateTyvars to) >> onOk fn t:
            M.return << CA.TypeFunction p f fromIsMutable t

        CA.TypeAlias p path ty:
            M.do (translateTyvars ty) >> onOk fn t:
            M.return << CA.TypeAlias p path t

        CA.TypeRecord p extensible attrs:
            M.do (M.dict_map (fn k: translateTyvars) attrs) >> onOk fn a:
            M.do (M.maybe_map translateTyvarName extensible) >> onOk fn e:
            M.return << CA.TypeRecord p e a

        CA.TypeVariable p name:
            M.do (translateTyvarName name) >> onOk fn n:
            M.return << CA.TypeVariable p n


translateTyvarName name =
    as String: EnvMonad String
    M.do (M.get .tyvarTranslations) >> onOk fn tyvarTranslations:
    try Dict.get name tyvarTranslations as
        Just translatedName:
            M.return translatedName

        Nothing:
            M.do (M.get .defsPath) >> onOk fn defsPath:

            uniqueName =
                String.join "#" << name :: defsPath

            M.do (M.update (fn env: { env with tyvarTranslations = Dict.insert name uniqueName .tyvarTranslations })) >> onOk fn _:
            M.return uniqueName



##
#- Pattern-y stuff
#


translateParameter env faParam =
    as Env: FA.Pattern: Res CA.Parameter
    try faParam as
        FA.PatternAny pos True name:
            Ok << CA.ParameterMutable (tp env.ro pos) name

        _:
            translatePattern env faParam >> onOk fn caPattern:
            Ok << CA.ParameterPattern caPattern



##
#- Pattern
#


translatePattern env faPattern =
    as Env: FA.Pattern: Res CA.Pattern
    translatePatternOrFunction env faPattern >> onOk fn either:
    try either as
        POF_Pattern caPattern:
            Ok caPattern

        POF_Function fun params:
            errorCantDeclareAFunctionHere env fun params faPattern


union POF =
    , POF_Pattern CA.Pattern
    , POF_Function String (List CA.Parameter)


translatePatternOrFunction env fa =
    as Env: FA.Pattern: Res POF
    try fa as
        FA.PatternAny pos True s:
            # TODO this happens (to me) when I use `=` in place of `:=`, so maybe change the message?
            faError env.ro pos "This is the wrong place to use `@`"

        FA.PatternAny pos False s:
            translatePatternOrFunction env (FA.PatternApplication pos s [])

        FA.PatternLiteral pos l:
            CA.PatternLiteral (tp env.ro pos) l
                >> POF_Pattern
                >> Ok

        FA.PatternApplication pos rawName faArgs:
            stringToStructuredName { env with maybeUpdateTarget = Nothing } pos rawName >> onOk fn sname:
            try sname as
                StructuredName_TypeOrCons { name, mod }:
                    Lib.list_mapRes (translatePattern env) faArgs >> onOk fn caArgs:
                    CA.PatternConstructor (tp env.ro pos) (resolveValueName env.ro False mod name) caArgs
                        >> POF_Pattern
                        >> Ok

                StructuredName_Value { name, mod, attrPath }:
                    if attrPath /= []:
                        errorTodo "can't use attribute access inside a pattern"

                    else
                        try mod as
                            AlreadyEmbedded:
                                errorTodo "can't use attribute shorthands inside a pattern"

                            ResolvedTo _:
                                errorTodo "It looks like you are trying to reference some module value, but I need just a new variable name"

                            NotSpecified:
                                # it's a function or variable!
                                if faArgs == []:
                                    (if name == "_":
                                        CA.PatternDiscard (tp env.ro pos)

                                    else
                                        CA.PatternAny (tp env.ro pos) name
                                    )
                                        >> POF_Pattern
                                        >> Ok

                                else
                                    Lib.list_mapRes (translateParameter env) faArgs >> onOk fn caParams:
                                    POF_Function name caParams
                                        >> Ok

        FA.PatternList pos fas:
            fold pattern last =
                # TODO pos is probably inaccurate
                CA.PatternConstructor (tp env.ro pos) Core.listCons.name [ pattern, last ]

            fas
                >> Lib.list_mapRes (translatePattern env)
                >> Result.map (List.foldr fold (fn x: x >> CA.PatternConstructor (tp env.ro pos) Core.listNil.name []) >> POF_Pattern)

        FA.PatternRecord pos recordArgs:
            if recordArgs.extends /= Nothing:
                errorTodo "can't use `with` inside patterns"

            else
                fold ( name & maybePattern ) dict =
                    if Dict.member name dict:
                        errorTodo << "duplicate attribute name in pattern: " .. name

                    else
                        try maybePattern as
                            Nothing:
                                Dict.insert name (CA.PatternAny (tp env.ro pos) name) dict >> Ok

                            Just faPattern:
                                faPattern
                                    >> translatePattern env
                                    >> Result.map (fn caPattern: Dict.insert name caPattern dict)

                Lib.list_foldlRes fold recordArgs.attrs Dict.empty
                    >> Result.map (fn x: x >> CA.PatternRecord (tp env.ro pos) >> POF_Pattern)

        FA.PatternCons pos pas:
            Lib.list_mapRes (translatePattern env) pas >> onOk fn caPas:
            try List.reverse caPas as
                last :: rest:
                    List.foldl (fn item list: CA.PatternConstructor (tp env.ro pos) Core.listCons.name [ item, list ]) last rest
                        >> POF_Pattern
                        >> Ok

                []:
                    makeError pos [ "should not happen: empty cons pattern" ]

        FA.PatternTuple pos fas:
            try fas as
                [ fa1, fa2 ]:
                    { extends = Nothing
                    , attrs =
                        [ "first" & Just fa1
                        , "second" & Just fa2
                        ]
                    }
                        >> FA.PatternRecord pos
                        >> translatePatternOrFunction env

                [ fa1, fa2, fa3 ]:
                    { extends = Nothing
                    , attrs =
                        [ "first" & Just fa1
                        , "second" & Just fa2
                        , "third" & Just fa3
                        ]
                    }
                        >> FA.PatternRecord pos
                        >> translatePatternOrFunction env

                _:
                    makeError pos [ "tuples can be only of size 2 or 3" ]



##
#- Statement
#


insertDefinedNames env stat names =
    as Env: FA.Statement: Dict String CA.Pos: Dict String CA.Pos
    try stat as
        FA.Definition fa:
            # TODO is there a clean way to avoid translating the patterns twice?
            try translatePatternOrFunction env fa.pattern as
                Err _:
                    names

                Ok (POF_Pattern caPattern):
                    Dict.union names (CA.patternNames caPattern)

                Ok (POF_Function fnName fnArgs):
                    Dict.insert fnName todoPos names

        _:
            names


translateStatementBlock env stats =
    as Env: List FA.Statement: Res (List CA.Statement)

    localEnv =
        { env with nonRootValues = List.foldl (insertDefinedNames env) env.nonRootValues stats }

    Lib.list_mapRes (translateStatement localEnv) stats


translateStatement env faStat =
    as Env: FA.Statement: Res CA.Statement
    try faStat as
        FA.Evaluation faExpr:
            # TODO Non-return, non-mutable, non-debug evaluations should produce an error.
            # Debug evaluations should be optimized away in production build
            faExpr
                >> translateExpression env
                >> Result.map CA.Evaluation

        FA.Definition fa:
            fa
                >> translateDefinition False env
                >> Result.map CA.Definition

        FA.TypeAlias fa:
            errorTodo "Aliases can be declared only in the root scope"

        FA.UnionDef fa:
            errorTodo "Types can be declared only in the root scope"



##
#- Expression
#


translateExpression env faExpr =
    as Env: FA.Expression: Res CA.Expression
    try faExpr as
        FA.Literal pos v:
            Ok << CA.Literal (tp env.ro pos) v

        FA.Variable pos { isBinop } faName:
            if isBinop:
                { isRoot = True
                , name = faName
                , attrPath = []
                }
                    >> CA.Variable (tp env.ro pos)
                    >> Ok

            else
                stringToStructuredName env pos faName >> onOk fn sname:
                name & mod & attrPath =
                    try sname as
                        StructuredName_Value a:
                            a.name & a.mod & a.attrPath

                        StructuredName_TypeOrCons a:
                            a.name & a.mod & []

                declaredInsideFunction =
                    Dict.member name env.nonRootValues

                { isRoot = not declaredInsideFunction
                , name = resolveValueName env.ro declaredInsideFunction mod name
                , attrPath = attrPath
                }
                    >> CA.Variable (tp env.ro pos)
                    >> Ok

        FA.Lambda pos faParams faBody:
            Lib.list_mapRes (translateParameter env) faParams >> onOk fn caParams:
            localEnv =
                { env with nonRootValues = List.foldl insertParamNames .nonRootValues caParams }
            try caParams as
                []:
                    Debug.todo "TODO should not happen but should be fixable?"

                caHead :: caTail:
                    translateStatementBlock localEnv faBody >> onOk fn caBody:
                    CA.Lambda (tp env.ro pos) caHead (List.foldr (wrapLambda env.ro pos) caBody caTail)
                        >> Ok

        FA.FunctionCall pos reference arguments:
            # ref arg1 arg2 arg3...
            fold argument refAccum =
                as CA.Argument: CA.Expression: CA.Expression
                CA.Call (tp env.ro pos) refAccum argument

            Result.map2
                (List.foldl fold)
                (translateExpression env reference)
                (Lib.list_mapRes (translateArgument env) arguments)

        FA.If pos { condition, true, false }:
            Result.map3
                (fn c t f:
                    CA.If (tp env.ro pos)
                        { condition = [ CA.Evaluation c ]
                        , true = t
                        , false = f
                        }
                )
                (translateExpression env condition)
                (translateStatementBlock env true)
                (translateStatementBlock env false)

        FA.Unop pos op faOperand:
            p =
                tp env.ro pos
            translateExpression env faOperand >> onOk fn caOperand:
            CA.Call p
                (CA.Variable p
                    { isRoot = True
                    , name = op.symbol
                    , attrPath = []
                    }
                )
                (CA.ArgumentExpression caOperand)
            >> Ok

        FA.Binop pos group sepList:
            translateBinops env pos group sepList

        FA.Mutable pos name:
            faError env.ro pos << name .. ": mutable values can be used only as arguments for function or mutation operators"

        FA.Record pos faArgs:
            makeUpdateTarget env faArgs.extends >> onOk fn caUpdateTarget:
            translateAttrsRec { env with maybeUpdateTarget = caUpdateTarget.maybeName } faArgs.attrs Dict.empty >> onOk fn caAttrs:
            caAttrs
                >> CA.Record (tp env.ro pos) caUpdateTarget.maybeName
                >> caUpdateTarget.wrapper
                >> Ok

        FA.List pos faItems:
            caPos =
                tp env.ro pos

            cons item list =
                CA.Call caPos
                    (CA.Call caPos
                        Core.cons
                        (CA.ArgumentExpression item)
                    )
                    (CA.ArgumentExpression list)

            faItems
                # TODO this is more List.reverse than necessary
                >> Lib.list_mapRes (translateExpression env)
                >> Result.map (List.foldr cons Core.nil)

        FA.Try pos fa:
            translatePatternAndStatements ( faPattern & faStatements ) =
                translatePattern env faPattern >> onOk fn caPattern:
                translateStatementBlock { env with nonRootValues = Dict.union (CA.patternNames caPattern) env.nonRootValues } faStatements >> onOk fn block:
                Ok ( caPattern & block )

            Result.map3
                (fn caValue caPatternsAndStatements caElse:
                    CA.Try (tp env.ro pos) caValue (caPatternsAndStatements .. caElse)
                )
                (translateExpression env fa.value)
                (Lib.list_mapRes translatePatternAndStatements fa.patterns)
                (try fa.maybeElse as
                    Nothing:
                        Ok []

                    Just faBlock:
                        translateStatementBlock env faBlock
                            >> Result.map (fn caBlock: [ CA.PatternDiscard (tp env.ro pos) & caBlock  ])
                )


makeUpdateTarget env maybeUpdateTarget =
    as Env: Maybe FA.Expression: Res { maybeName as Maybe CA.VariableArgs, wrapper as CA.Expression: CA.Expression }
    try Maybe.map (translateExpression { env with maybeUpdateTarget = Nothing }) maybeUpdateTarget as
        Nothing:
            Ok { maybeName = Nothing, wrapper = identity }

        Just (Err e):
            Err e

        Just (Ok (CA.Variable _ args)):
            # TODO test for lowercase name?
            Ok { maybeName = Just args, wrapper = identity }

        Just (Ok expr):
            errorTodo "NI { (expr) with ...} not yet implemented =("


translateAttrsRec env faAttrs caAttrsAccum =
    as Env: [String & Maybe FA.Expression]: Dict String CA.Expression: Res (Dict String CA.Expression)
    try faAttrs as
        []:
            Ok caAttrsAccum

        attrName & maybeAttrExpression :: faTail:
            exprRes =
                try maybeAttrExpression as
                    Just faExpr:
                        translateExpression env faExpr

                    Nothing:
                        declaredInsideFunction =
                            Dict.member attrName env.nonRootValues
                        { name = resolveValueName env.ro declaredInsideFunction NotSpecified attrName
                        , isRoot = not declaredInsideFunction
                        , attrPath = []
                        }
                            >> CA.Variable todoPos
                            >> Ok
            exprRes >> onOk fn expr:
            translateAttrsRec env faTail (Dict.insert attrName expr caAttrsAccum)


translateArgument env faExpr =
    as Env: FA.Expression: Res CA.Argument
    try faExpr as
        FA.Mutable pos faName:
            stringToStructuredName { env with maybeUpdateTarget = Nothing } pos faName >> onOk fn sname:
            try sname as
                StructuredName_TypeOrCons _:
                    errorTodo "constructors can't be mutable?"

                StructuredName_Value { name, mod, attrPath }:
                    if mod == NotSpecified and Dict.member name env.nonRootValues:
                        { isRoot = False
                        , name = name
                        , attrPath = attrPath
                        }
                            >> CA.ArgumentMutable (tp env.ro pos)
                            >> Ok

                    else
                        [ "only values declared inside a function scope can be mutated!"
                        , Debug.toString sname
                        , faName
                        ]
                            >> String.join "\n"
                            >> faError env.ro pos

        _:
            faExpr
                >> translateExpression env
                >> Result.map CA.ArgumentExpression


translateBinops env pos group ( firstItem & firstTail ) =
    as Env: Pos: Op.Precedence: SepList Binop FA.Expression: Res CA.Expression
    try firstTail as
        []:
            translateExpression env firstItem

        firstSep & secondItem  :: []:
            try group as
                Op.Tuple:
                    Result.map2
                        (fn first second:
                            Dict.empty
                                >> Dict.insert "first" first
                                >> Dict.insert "second" second
                                >> CA.Record (tp env.ro pos) Nothing
                        )
                        (translateExpression env firstItem)
                        (translateExpression env secondItem)

                _:
                    translateSimpleBinop env pos firstItem firstSep secondItem

        firstSep & secondItem  :: secondSep & thirdItem :: thirdTail:
            # Use "as"? Yay undocumented syntax...
            secondTail =
                secondSep & thirdItem :: thirdTail
            try group as
                Op.Comparison:
                    if notAllSeparators (sameDirectionAs firstSep) secondTail:
                        # TODO actually list the seps
                        makeError pos [ "can't mix comparison ops with different direction" ]

                    else
                        # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                        makeError pos [ "NI compops expansion" ]

                Op.Logical:
                    if notAllSeparators ((==) firstSep) secondTail:
                        makeError pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]

                    else
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                Op.Tuple:
                    if thirdTail /= []:
                        makeError pos [ "Tuples can't have more than 3 items, use a record instead." ]

                    else
                        Result.map3
                            (fn first second third:
                                Dict.empty
                                    >> Dict.insert "first" first
                                    >> Dict.insert "second" second
                                    >> Dict.insert "third" third
                                    >> CA.Record (tp env.ro pos) Nothing
                            )
                            (translateExpression env firstItem)
                            (translateExpression env secondItem)
                            (translateExpression env thirdItem)

                Op.Pipe:
                    if notAllSeparators ((==) firstSep) secondTail:
                        makeError pos [ "Mixing pipes is ambigous. Use parens." ]

                    else if firstSep.associativity == Op.Right:
                        translateBinopSepList_rightAssociative env pos firstItem firstTail

                    else
                        translateBinopSepList_leftAssociative env pos firstItem firstTail

                Op.Mutop:
                    makeError pos [ "mutops can't be chained" ]

                _:
                    translateBinopSepList_rightAssociative env pos firstItem firstTail


notAllSeparators f ls =
    as (sep: Bool): [ sep & item]: Bool
    try ls as
        []:
            False

        sep & item :: tail:
            if f sep:
                notAllSeparators f tail

            else
                True


sameDirectionAs a b =
    as Binop: Binop: Bool
    if a.symbol == b.symbol:
        True

    else
        try a.symbol as
            ">":
                b.symbol == ">="

            ">=":
                b.symbol == ">"

            "<":
                b.symbol == "<="

            "<=":
                b.symbol == "<"

            _:
                False


translateBinopSepList_rightAssociative env pos left opsAndRight =
    as Env: FA.Pos: FA.Expression: [ Binop & FA.Expression ]: Res CA.Expression
    translateExpression env left >> onOk fn caLeft:
    try opsAndRight as
        []:
            Ok caLeft

        op & right :: tail:
            translateBinopSepList_rightAssociative env pos right tail >> onOk fn caRight:
            makeBinop (tp env.ro pos) (CA.ArgumentExpression caLeft) op (CA.ArgumentExpression caRight)
                >> Ok


translateBinopSepList_leftAssociative env pos leftAccum opsAndRight =
    as Env: FA.Pos: FA.Expression: [ Binop & FA.Expression ]: Res CA.Expression

    translateExpression env leftAccum >> onOk fn caLeftAccum:
    translateBinopSepListRec env pos caLeftAccum opsAndRight


translateBinopSepListRec env pos leftAccum opsAndRight =
    as Env: FA.Pos: CA.Expression: [ Binop & FA.Expression ]: Res CA.Expression
    try opsAndRight as
        []:
            Ok leftAccum

        op & faRight :: tail:
            translateArgument env faRight >> onOk fn caRight:
            translateBinopSepListRec env pos (makeBinop (tp env.ro pos) (CA.ArgumentExpression leftAccum) op caRight) tail


[# Unlike other ML languages, the left operand is the _second_ argument

`a + b` == `((+) b) a`

#]
makeBinop caPos left op right =
    as CA.Pos: CA.Argument: Binop: CA.Argument: CA.Expression
    try left & op.symbol & right as

        # TODO don't hardcode the strings, use instead those defined in Prelude
        _ & ">>" & CA.ArgumentExpression rightExpr :
            CA.Call caPos rightExpr left

        CA.ArgumentExpression leftExpr & "<<" & _:
            CA.Call caPos leftExpr right

        _:
            CA.Call caPos
                (CA.Call caPos
                    (CA.Variable caPos
                        { isRoot = True
                        , name = op.symbol
                        , attrPath = []
                        }
                    )
                    right
                )
                left


translateSimpleBinop env pos left op right =
    as Env: FA.Pos: FA.Expression: Binop: FA.Expression: Res CA.Expression
    Result.map2 (fn l r: makeBinop (tp env.ro pos) l op r)
        (translateArgument env left)
        (translateArgument env right)



##
#- Type
#


addAttributes ro pos faAttrs caAttrsAccum =
    as ReadOnly: FA.Pos: [ String & Maybe FA.Type ]: Dict String CA.Type: Res CA.Type
    try faAttrs as
        []:
            CA.TypeRecord (tp ro pos) Nothing caAttrsAccum
                >> Ok

        name & maybeFaType :: faTail:
            faType =
                Maybe.withDefault (FA.TypeName [# TODO #] ( -1 & -1 ) name) maybeFaType

            translateType ro faType >> onOk fn caType:
            addAttributes ro pos faTail (Dict.insert name caType caAttrsAccum)


startsWithUpperChar s =
    as String: Bool
    try String.uncons s as
        Nothing:
            False

        Just ( head & tail ):
            Char.isUpper head


translateType ro faType =
    as ReadOnly: FA.Type: Res CA.Type
    try faType as
        FA.TypeName p name:
            translateType ro << FA.TypePolymorphic p name []

        FA.TypePolymorphic pos rawName args:
            stringToStructuredName (initEnv ro) pos rawName >> onOk fn sname:
            try sname as
                StructuredName_Value { name, mod, attrPath }:
                    if args /= []:
                        # TODO is this the correct error?
                        errorTodo "rank 2 types are not supported"

                    else if mod /= NotSpecified:
                        errorTodo "this is not a valid name for a type variable"

                    else if attrPath /= []:
                        errorTodo "no attribute accessors on types"

                    else
                        name
                            >> CA.TypeVariable todoPos
                            >> Ok

                StructuredName_TypeOrCons { name, mod }:
                    Lib.list_mapRes (translateType ro) args >> onOk fn caArgs:
                    caArgs
                        >> CA.TypeConstant (tp ro pos) (resolveTypeName ro False mod name)
                        >> Ok

        FA.TypeFunction pos fa_from fromIsMut fa_to:
            Result.map2
                (fn ca_from ca_to: CA.TypeFunction (tp ro pos) ca_from fromIsMut ca_to)
                (translateType ro fa_from)
                (translateType ro fa_to)

        FA.TypeTuple pos types:
            try types as
                [ faFirst, faSecond ]:
                    Result.map2
                        (fn caFirst caSecond:
                            Dict.empty
                                >> Dict.insert "first" caFirst
                                >> Dict.insert "second" caSecond
                                >> CA.TypeRecord (tp ro pos) Nothing
                        )
                        (translateType ro faFirst)
                        (translateType ro faSecond)

                [ faFirst, faSecond, faThird ]:
                    Result.map3
                        (fn caFirst caSecond caThird:
                            Dict.empty
                                >> Dict.insert "first" caFirst
                                >> Dict.insert "second" caSecond
                                >> Dict.insert "third" caThird
                                >> CA.TypeRecord (tp ro pos) Nothing
                        )
                        (translateType ro faFirst)
                        (translateType ro faSecond)
                        (translateType ro faThird)

                _:
                    errorTodo "Tuples can only have size 2 or 3. Use a record."

        FA.TypeRecord p recordArgs:
            if recordArgs.extends /= Nothing:
                errorExperimentingWithNoExtensibleTypes ro p

            else
                addAttributes ro p recordArgs.attrs Dict.empty


errorExperimentingWithNoExtensibleTypes ro pos =
    as ReadOnly: FA.Pos: Res a
    Error.faSimple
        ro.currentModule
        pos
        "For now extensible types are disabled, I want to see if it's good to do without them"



##
#- Helpers
#


wrapLambda ro faWholePos param bodyAccum =
    as ReadOnly: FA.Pos: CA.Parameter: List CA.Statement: List CA.Statement
    end =
        Tuple.second faWholePos

    paramPos =
        try param as
            CA.ParameterMutable pos name:
                pos

            CA.ParameterPattern pa:
                CA.patternPos pa

    lambdaPos =
        posReplaceEnd end paramPos

    [ bodyAccum
        >> CA.Lambda lambdaPos param
        >> CA.Evaluation
    ]


posReplaceEnd end pos =
    as Int: CA.Pos: CA.Pos
    try pos as
        CA.P m s e:
            CA.P m s end

        _:
            pos


firstErrorRec os rs =
    as List o: List (Result e o): Result e (List o)
    try rs as
        []:
            Ok os

        (Err e) :: _:
            Err e

        (Ok o) :: rt:
            firstErrorRec (o :: os) rt


maybeResultToResultMaybe maybeResult =
    as Maybe (Result e o): Result e (Maybe o)
    try maybeResult as
        Nothing:
            Ok Nothing

        Just (Ok something):
            Ok (Just something)

        Just (Err e):
            # Unfortunately Elm doesn't realize that the two `Err e` have the same type
            # Is it a limit of how pattern matching is implemented?
            Err e



##
#- Errors
#


errorRecordUpdateShorthandOutsideRecordUpdate pos rawString env =
    as FA.Pos: String: Env: Res a
    Error.faSimple
        env.ro.currentModule
        pos
        ("`" .. rawString .. "` looks like a record update shorthand, but we are not inside a record update!")


errorCantDeclareAFunctionHere env name params originalPattern =
    as Env: String: List CA.Parameter: FA.Pattern: Res a
    Error.faSimple
        env.ro.currentModule
        (FA.patternPos originalPattern)
        "it seems like there is a function declaration inside a pattern?"


