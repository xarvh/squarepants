

alias Env =
    {
    , module as TA.Module
    , normalizeEnv as Compiler/Normalize.Env

    , genVarCounter as Int

    , availableResultLocals as Set TA.Type

    , localsTaToWa as Dict Name WA.VarName
    , localsWa as Set WA.VarName
    }



#
#
#

location as fn Env, Pos: Text =
    fn env, pos:

    x = Error.posToHuman { fsPath = env.module.fsPath, content = env.module.asText } pos
    x.location



#
# Translation
#


generateName as fn Env: WA.VarName & Env =
    fn env:

    n = env.genVarCounter + 1

    WA.generateName n & { env with genVarCounter = n }



union PickedName =
    , TrivialPattern Name TA.FullType
    , GenerateName
    , NoNamedVariables



pickMainName as fn TA.Pattern: PickedName =
    fn pattern:
    try pattern as

        , TA.PatternAny pos { maybeName = Just name, type }:
            TrivialPattern name type

        , _:
            if TA.patternNames pattern /= Dict.empty then
                GenerateName
            else
                NoNamedVariables



translatePattern as fn TA.Pattern, WA.Expression: [ TA.FullType & Name & WA.Expression ] =
    fn pattern, accessExpr:

    translatePatternRec pattern accessExpr []
# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        TA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []


translatePatternRec as fn TA.Pattern, WA.Expression, [ TA.FullType & Name & WA.Expression ]: [ TA.FullType & Name & WA.Expression ] =
    fn pattern, accessExpr, accum:
    try pattern as
        , TA.PatternAny _ { maybeName = Nothing, type }:
            accum

        , TA.PatternAny _ { maybeName = Just name, type }:
            (type & name & accessExpr) :: accum

        , TA.PatternLiteralNumber _ _:
            accum

        , TA.PatternLiteralText _ _:
            accum

        , TA.PatternConstructor _ path pas:
            accum >> List.indexedFor __ pas fn index, pa, a:
                translatePatternRec pa (WA.ConstructorAccess index accessExpr) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                translatePatternRec pa (WA.RecordAccess name accessExpr) a


testPattern as fn TA.Pattern, WA.Expression, [WA.Expression]: [WA.Expression] =
    fn pattern, valueToTest, accum:
    try pattern as

        , TA.PatternAny _ _:
            accum

        , TA.PatternLiteralText _ text:
            WA.CompareWithLiteralText text valueToTest :: accum

        , TA.PatternLiteralNumber _  num:
            WA.CompareWithLiteralNumber num valueToTest :: accum

        , TA.PatternConstructor _ (USR umr name) pas:
            (WA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (WA.ConstructorAccess index valueToTest) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                testPattern pa (WA.RecordAccess name valueToTest) a


translateArgAndType as fn Env, TA.Argument: TranslateBase WA.Argument =
    fn env, taArg:

    try taArg as
        , TA.ArgumentExpression fullType exp:
            expOut =
                translateExpression env exp
            {
            , recycledInStats = expOut.recycledInStats
            , stats = expOut.stats
            , recycledInValue = expOut.recycledInValue
            , value = WA.ArgumentSpend fullType expOut.value
            }

        , TA.ArgumentRecycle pos rawType attrPath name:
            {
            , recycledInStats = Set.empty
            , stats = []
            , recycledInValue = Set.ofOne name
            , value = WA.ArgumentRecycle rawType attrPath name
            }



translateExpression as fn Env, TA.Expression: Env & [WA.Statement] & WA.Expression =
    fn env, expression:

    try expression as
        , TA.LiteralNumber _ num:
            env & [] & WA.LiteralNumber num

        , TA.LiteralText _ text:
            todo "add error"
            env & [] & WA.Variable (RefLocal "ERROR")

        , TA.Variable _ ref:
            env & [] & WA.Variable ref

        , TA.Constructor _ usr:
            env & [] & WA.Constructor usr

        , TA.RecordAccess _ attrName exp:
            e & s & e = translateExpression env exp
            e & s & WA.RecordAccess attrName e

        , TA.Fn pos taPars body bodyT:
            translateFn env taPars body

        , TA.Record pos extends attrs:
            translateRecord pos env extends attrs

        , TA.Call pos ref argsAndTypes:
#            try Inline.inline env.inlineEnv ref argsAndTypes as
#                , Inline.Circular circulars:
#                    add error
#                    env & [] & WA.Error
#
#                , Inline.Inlined inlinedExpression:
#                    translateExpression env inlinedExpression
#
#                , Inline.NotInlined:
                    translateCall env pos ref argsAndTypes

        , TA.If pos ar:
            translateIf env pos ar.condition ar.true ar.false

        , TA.Try pos { value, valueType, patternsAndExpressions }:
            translateTryAs env pos value valueType patternsAndExpressions

        , TA.LetIn valueDef e bodyType:
            translateLetIn env valueDef e bodyType

        , TA.DestroyIn name e:
            translateExpression env e



translateFn as fn Env, [TA.Parameter], TA.Expression: TranslateOut =
    fn env, taPars, taBody:

    bodyOut =
        translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } taBody

    eaBody =
        # TODO use WA.Evaluation instead of WA.Return if bodyT is `None`?
        List.concat [ bodyOut.stats, [ WA.Return bodyOut.value ] ]

    recycledInBody =
        Set.join bodyOut.recycledInStats bodyOut.recycledInValue

    #
    # To get recycledInValue we take all vars that have been recycled in the body, and remove those that are actually declared as function params.
    #
    # If any param needs to be unwrapped, we add a statement for it.
    #
    { with recycledInValue, body, pars } =
        List.forReversed
            {
            , env
            , recycledInValue = recycledInBody
            , body = eaBody
            , pars = []
            }
            taPars
            translateAndAddParameter

    {
    , recycledInStats = Set.empty
    , stats = []
    , recycledInValue
    , value = todo "WA.Fn eaPars wrappedBody"
    }



alias ParameterAcc =
    {
    , env as Env
    , recycledInValue as Set Name
    , body as [WA.Statement]
    , pars as [Bool & Maybe Name]
    }



translateAndAddParameter as fn TA.Parameter, ParameterAcc: ParameterAcc =
    fn par, acc:

    try par as
        , TA.ParameterRecycle pos rawType name:
            { acc with
            , recycledInValue = Set.remove name .recycledInValue
            , pars = [True & Just name, ...(.pars)]
            }

        , TA.ParameterPattern fullType pa:
            try pickMainName pa as
                , NoNamedVariables:
                    { acc with pars = [False & Nothing, ...(.pars)] }

                , TrivialPattern argName type:
                    { acc with pars = [False & Just argName, ...(.pars)] }

                , GenerateName:
                    mainName & env =
                        generateName acc.env

                    namesAndExpressions =
                        translatePattern pa (WA.Variable (RefLocal mainName))

                    body =
                        List.for acc.body namesAndExpressions fn (type & name & value), stats:
                            [ WA.VarDefinition { name, type, value }, ...stats]

                    { acc with
                    , env
                    , body
                    , pars = [False & Just mainName, ...(.pars)]
                    }



translateRecord as fn Pos, Env, Maybe TA.Expression, Dict Name TA.Expression: TranslateOut =
    fn pos, env, taMaybeExtends, taAttrs:

    [#
        Given

            r1 = { a = Random.int @seed1, b = Random.int @seed1 }
            r2 = { b = Random.int @seed2, a = Random.int @seed2 }

        If seed1 and seed2 are initially the same, r1 and r2 must also be the same.

        This is obtained by
          1) Relying on Dict to order attributes alphabetically
          2) Ensuring that when we break let..ins into statements+expression, we don't mangle the recycling order

    #]
    attrOutsByName =
        Dict.map (fn k, v: translateExpression env v) taAttrs


    [#
        The other problem is that of let..ins inside the record declaration.

            r =
              {
              , a = (x = Random.int @seed1; x)
              , b = (x = Random.int @seed1; x)
              }

        The problems are:
          1) If we pull out the let..ins, we have two different definitions for `x`.
              Every definition should be made unique.

          2) If both stats and expr recycle, we still need to maintain recycle order after
              stats and expr have been separated and mixed with stats and expr from other
              attrs.
              An algorithm to manage this would be:

                for any attr
                    if ANY recycle in expr is used by any of the subsequent stats
                        assign expr to a var, then use the var to init the record
                    else
                        no problem, use expr directly
    #]


    #
    # NOTE: Right now we do not allow let..ins inside record attribute definitions.
    # TODO: this should be enforced by CanonicalAst!!!
    #
    # NOTE: We are relying on Dict to sort attributes alphabetically to ensure consistency of recycling order.
    #
    attrsOut =
        Dict.forReversed (none []) attrOutsByName fn name, o, acc:
            merge acc o fn attrs, value: [name & value, ...attrs]

    maybeExtendsOut =
        try taMaybeExtends as
            , Nothing:
                none Nothing

            , Just taExtends:
                translateExpression env taExtends
                >> map Just __

    out =
        merge maybeExtendsOut attrsOut fn maybeExt, att: WA.LiteralRecord maybeExt att

    if out.stats /= [] then
        todo << location env pos .. " Compiler TODO: let..in inside record literal is not YET implemented. =("
    else
        out



[#
    We assume that EVERY expression in the call can potentially mess with uniques.

    Since the order of uniques manipulation is important, we need to guarantee the
    execution order of every expression in the call.


    (ref a1 .. aN)

    |
    |
    |
    V

    ref/statements

    a1/statements
    gen1 = a1/expr

    ...

    aL/statements   // This is the last argument with statements
    genL = aL/expr

    ref(gen1, ..., genL, aL+1/expr, ... aN/expr)
#]
translateCall as fn Env, Pos, TA.Expression, [TA.Argument]: Env & [WA.Statement] & WA.Expression =
    fn env0, pos, ref, taArgs:

    env1 & refStats & refExpr =
        translateExpression env0 ref

    (env2 as Env) & (hoistedArgs as [[WA.Statement] & WA.Argument]) & (simpleArgs as [WA.Argument])=
        List.forReversed (env1 & []) taArgs fn arg, envX0 & hoisted & simple:

            envX1 & argStats & argExpr =
                translateArgAndType envX0 arg

            if hoisted == [] and argStats == [] then
                envX1 & hoisted & [argExpr, ...simple]
            else
                envX1 & [argStats & argExpr, ...hoisted] & simple

    env3 & hoistedStats =
        todo "env3"

    hoistedArgs =
        todo "hoistedArgs"

    waArgs =
        todo "waArgs"

    stats =
        [
        , refStats
        , hoistedStats
        ]
        >> List.concat

    env3 & stats & WA.Call ref waArgs


[#

    (if condition then true else false)

    |
    |
    |
    V

    $condition/statements

    var result/type;

    if ($condition/value) {
        $true/statements
        result/type = $true/value
    } else {
        $false/statements
        result/type = $false/value
    }

    result/type;

#]
translateIf as fn Env, Pos, TA.Expression, TA.Expression, TA.Expression: Env & [WA.Statement] & WA.Expression =
    fn env0, pos, taCondition, taTrue, taFalse:

    env1 & conditionStats & conditionExpr =
        translateExpression env0 ar.condition

    _ & trueStats & trueExpr =
        translateExpression env1 ar.true

    _ & falseStats & falseExpr =
        translateExpression env1 ar.false

    env2 & resultStats & resultLocal =
        getResultLocal (todo "type!!!") env1

    stats =
        [
        , conditionOut.statements
        , resultStats
        , WA.IfStatement conditionOut.value
            [
            , true.statements
            , WA.Assignment resultLocal true.value
            ]
            [
            , false.statements
            , WA.Assignment resultLocal false.value
            ]
        ]

    env2 & stats & WA.VarName resultLocal


translateTryAs as fn Env, Pos, TA.Expression, TA.FullType, [TA.Pattern & TA.Expression]: TranslateOut =
    fn env0, pos, value, valueType, patternsAndExpressions:

    # 1. create a name for the value (unless it's already a variable)
    { valueExpr, valueStats, recycledInValue0, env1 } =
        try value & valueType.uni as
            , TA.Variable _ ref & Imm:
                {
                , valueExpr = WA.Variable ref
                , valueStats = []
                , recycledInValue0 = Set.empty
                , env1 = env0
                }

            , _:
                valueOut =
                    translateExpression env0 value

                if valueOut.stats /= [] then
                    todo "try..as stats!?"
                else
                    None

                tryName & env1 =
                    generateName env0

                definition =
                    WA.VarDefinition
                        {
                        , name = tryName
                        , type = valueType
                        , value = valueOut.value
                        }

                {
                , valueExpr = WA.Variable (RefLocal tryName)
                , valueStats = [definition]
                , recycledInValue0 = valueOut.recycledInValue
                , env1
                }

    # 2. if-elses
    resultName & env2 =
        generateName env1

    [#
        let generated;

        if condition1
            statements
            generated = value
        else if condition2
            statements
            generated = condition2
        else
            crash "missing case: ..."

        generated;

    #]

    addTryPatternAndBlock as fn TA.Pattern & TA.Expression, WA.Statement & Set Name: WA.Statement & Set Name =
        fn pattern & block, nextTryStatement & recycled0:

        testIfPatternMatches as WA.Expression =
            testPattern pattern valueExpr []
            >> List.reverse
            >> WA.PatternMatchConditions

        namesAndExpressions as [ TA.FullType & Name & WA.Expression ] =
            translatePattern pattern valueExpr

        blockOut =
            translateExpression env2 block

        whenConditionMatches as [WA.Statement] =

            patternUnpackingStats =
                List.map (fn type & name & v: WA.VarDefinition { name, type, value = v }) namesAndExpressions

            [
            , patternUnpackingStats
            , blockOut.stats
            , [WA.Assignment resultName blockOut.value]
            ]
            >> List.concat

        recycled1 =
            recycled0
            >> Set.join __ blockOut.recycledInStats
            >> Set.join __ blockOut.recycledInValue

        WA.IfStatement testIfPatternMatches whenConditionMatches nextTryStatement & recycled1


    # 3. end
    default =
        WA.MissingPattern (location env2 pos) valueExpr

    ifElseStat & recycledInValue1 =
        List.forReversed (default & recycledInValue0) patternsAndExpressions addTryPatternAndBlock

    {
    , recycledInStats = Set.empty
    , stats = List.concat [valueStats, [ifElseStat]]
    , recycledInValue = recycledInValue1
    , value = WA.Variable (RefLocal resultName)
    }



translateLetIn as fn Env, TA.ValueDef, TA.Expression, TA.FullType: TranslateOut =
    fn env, valueDef, e, bodyType:

    letOut =
        translateExpression env valueDef.body

    inOut =
        translateExpression env e

    setupStats =
        try pickMainName valueDef.pattern as
            , NoNamedVariables:
                [ WA.Execution letOut.value ]

            , TrivialPattern defName type:
                [ WA.VarDefinition defName letOut.value ]

            , GenerateName:
                mainName & newEnv =
                    # TODO check if valueDef.body is just a variable
                    generateName env

                mainNameDef =
                    WA.VarDefinition
                        {
                        , name = mainName
                        , type = valueDef.type
                        , value = letOut.value
                        }

                patternUnpackingStats =
                    WA.Variable (RefLocal mainName)
                    >> translatePattern valueDef.pattern
                    >> List.map namesAndExpressions fn type & name & v:
                        WA.VarDefinition { name, type, value = v }

                [mainNameDef, ...patternUnpackingStats]

    stats =
        [
        , letOut.stats
        , setupStats
        , inOut.stats
        ]

    recycledInStats =
        letOut.recycledInStats
        >> Set.join letOut.recycledInValue
        >> Set.join letIn.recycledInStats

    value =
        inOut.value

    recycledInValue =
        inOut.recycledInValue

    {
    , stats
    , recycledInStats
    , value
    , recycledInValue
    }




#
#
#



translateRootValueDef as fn Env, TA.ValueDef, ByUsr WA.GlobalDefinition: ByUsr WA.GlobalDefinition =
    fn env, def, accum:

    deps =
        def.directValueDeps

    try pickMainName def.pattern as

        , NoNamedVariables:
            accum

        , TrivialPattern name type:
            usr =
                USR env.module.umr name

            Dict.insert usr
                {
                , usr
                , expr = translateExpression env def.body
                , deps
                }
                accum

        , GenerateName:
            mainName & newEnv =
                generateName env

            mainUsr =
                USR env.module.umr mainName

            mainDef as WA.GlobalDefinition =
                {
                , usr = mainUsr
                , expr = translateExpression newEnv def.body
                , deps
                }

            accum
            >> Dict.insert mainUsr mainDef __
            >> List.for __ (translatePattern def.pattern (WA.Variable (RefGlobal mainUsr))) fn (type & name & expr), z:
                subUsr = USR env.module.umr name
                Dict.insert subUsr { usr = subUsr, expr, deps = Set.ofOne mainUsr } z


#
# Main
#


circularIsError as fn ByUsr WA.GlobalDefinition, [USR]: Bool =
    fn globalDefsByName, usrs:

    zzz =
      fn usr:
      try Dict.get usr globalDefsByName as
          , Nothing:
              # native or some special stuff?
              False

          , Just globalDef:
              try globalDef.expr as
                  , WA.Fn _ _: False
                  , _: True

    List.any zzz usrs


circularToError as fn [USR]: Error =
    fn usrs:

    Error.Raw
        [ "circular dependency: "
        , ... List.map toHuman usrs
        ]


translateAll as fn UMR, [TA.Module]: Res { entryUsr as USR, defs as [WA.GlobalDefinition] } =
    fn entryModule, modules:

    Debug.benchStart None

    globalDefsByName as ByUsr WA.GlobalDefinition =
        Dict.empty >> List.for __ modules fn module, d:
            Dict.for d module.valueDefs fn _, def, a:
                env = { genVarCounter = 0, module }
                translateRootValueDef env def a

    circulars & reorderedNames =
        RefHierarchy.reorder (fn globalDef: globalDef.deps) globalDefsByName

    Debug.benchStop "makeEmittable"

    errors =
        List.filter (circularIsError globalDefsByName __) circulars

    if errors /= [] then
        errors
        >> List.map circularToError __
        >> Error.Nested
        >> Err

    else
        {
        , defs = List.filterMap (fn name: Dict.get name globalDefsByName) reorderedNames
        , entryUsr = USR entryModule "main"
        }
        >> Ok
