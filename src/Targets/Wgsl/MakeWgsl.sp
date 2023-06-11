

alias Env =
    {
    , genVarCounter as Int
    , module as TA.Module
    }




location as fn Env, Pos: Text =
    fn env, pos:

    x = Error.posToHuman { fsPath = env.module.fsPath, content = env.module.asText } pos
    x.location




#
# Translation
#


union PickedName =
    , TrivialPattern Name TA.FullType
    , GenerateName
    , NoNamedVariables


generateName as fn Env: Name & Env =
    fn env:
    Text.fromNumber (env.genVarCounter + 1) & { env with genVarCounter = 1 + .genVarCounter }


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


[#

    TODO: Explain why are we doing this batshit mess


#]
alias TranslateBase value =
    {
    , recycledInStats as Set Name
    , stats as [WA.Statement]
    , recycledInValue as Set Name
    , value as value
    }


alias TranslateOut =
    TranslateBase WA.Expression


none as fn value: TranslateBase value =
    fn value:
    {
    , recycledInStats = Set.empty
    , stats = []
    , recycledInValue = Set.empty
    , value
    }


map as fn (fn a: b), TranslateBase a: TranslateBase b =
    fn f, a:
    todo "map"


merge as fn TranslateBase a, TranslateBase b, (fn a, b: c): TranslateBase c =
    fn ta, tb, f:
    {
    , recycledInStats = Set.join ta.recycledInStats tb.recycledInStats
    , stats = List.concat [ ta.stats, tb.stats ]
    , recycledInValue = Set.join ta.recycledInValue tb.recycledInValue
    , value = f ta.value tb.value
    }





translateExpression as fn Env, TA.Expression: TranslateOut =
    fn env, expression:

    try expression as
        , TA.LiteralNumber _ num:
            none << WA.LiteralNumber num

        , TA.LiteralText _ text:
            todo "add error"
            none << WA.Variable (RefLocal "ERROR")

        , TA.Variable _ ref:
            none << WA.Variable ref

        , TA.Constructor _ usr:
            none << WA.Constructor usr

        , TA.RecordAccess _ attrName exp:
            translateExpression env exp
            >> map (WA.RecordAccess attrName __)

        , TA.Fn pos taPars body bodyT:
            translateFn env taPars body

        , TA.Record _ extends attrs:
            translateRecord env extends attrs

        , TA.Call _ ref argsAndTypes:
            translateCall env ref argsAndTypes

        , TA.If _ ar:
            conditionOut =
                translateExpression env ar.condition

            trueOut =
                translateExpression env ar.true

            falseOut =
                translateExpression env ar.false

            if trueOut.stats == [] and falseOut.stats == [] then
                {
                , recycledInStats = conditionOut.recycledInStats
                , stats = conditionOut.stats
                , recycledInValue = Set.join conditionOut.recycledInValue (Set.join trueOut.recycledInStats falseOut.recycledInStats)
                , value = WA.IfExpression conditionOut.value trueOut.value falseOut.value
                }
            else
                todo (location env pos .. " let..in inside if branches not yet implemented =(")
                [#
                generatedVar
                IfStatement
                evaluate generatedVar

                {
                , recycledInStats = conditionOut.recycledInStats
                , stats = conditionOut.stats
                , recycledInValue = Set.join conditionOut.recycledInValue (Set.join trueOut.recycledInStats falseOut.recycledInStats)
                , value =
                    WA.Conditional
                        conditionOut.value
                        (List.concat trueOut.stats [ WA.Evaluation ... ])
                        ()
                }
                #]

        , TA.Try pos { value, valueType, patternsAndExpressions }:
            translateTryAs env value valueType patternsAndExpressions

        , TA.LetIn valueDef e bodyType:
            translateLetIn env valueDef e bodyType

        , TA.DestroyIn name e:
            translateExpression env e



translateFn as fn Env, [TA.Parameter], TA.Expression: TranslateOut =

    bodyOut =
        translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } body

    eaBody =
        # TODO use WA.Evaluation instead of WA.Return if bodyT is `None`?
        List.concat [ bodyOut.stats, [ WA.Return bodyOut.expr ] ]

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

    { recycledInStats = Set.none, stats = [], recycledInValue, expr = WA.Fn eaPars wrappedBody }



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
                            [VarDefinition { name, type, value }, ...stats]

                    { acc with
                    , env
                    , body
                    , pars = [False & Just mainName, ...(.pars)]
                    }



translateRecord as fn Env, Maybe TA.Expression, Dict Name TA.Expression: TranslateOut =
    fn env, taMaybeExtends, taAttrs:

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


    vvvv The code below does NOT deal with let..ins from different attributes defining overlapping names


    attrsEnv & attrsOut =
        Dict.for (env & none Dict.empty) attrOutsByName fn name, out, (envX & acc):

            ooo =
                if out.recycledInValue /= Set.empty and no recycled in subsequent stats then
                    envX & out

                else
                    varName & newEnv =
                        generateName envX

                    { out with
                    , recycledInStats = Set.join .recycledInStats .recycledInValue
                    , stats = List.concat .stats [ WA.VarDefinition varName ??? .expr ]
                    , recycledInValue = Set.empty
                    , value = WA.Variable (RefLocal varName)
                    }

            newEnv & merge acc ooo fn attrs, value: Dict.insert name value attrs
    #]

    eaMaybeExtend =
        try taMaybeExtends as
            , Nothing:
                none Nothing

            , Just taExtends:
                extendsOut =
                    translateExpression attrsEnv taExtends

                {
                , recycledInStats = extendsOut.recycledInStats
                , stats = extendsOut.stats
                , recycledInValue = extendsOut.recycledInValue
                , value = Just extendsOut.value
                }

    out =
        merge extendsOut attrsOut fn ext, att: WA.LiteralRecord (Just ext) att

    if out.stats /= [] then
        todo << location env pos .. " Compiler TODO: let..in inside record literal is not YET implemented. =("
    else
        out



translateCall as fn Env, TA.Expression, [TA.Argument]: TranslateOut =
    fn env, ref, args:

    refOut =
        translateExpression env ref

    argOuts =
        List.map (translateArgAndType env __) args

    [#

        for any arg
            if ANY recycle in exp is used by any of the subsequent stats
                use a var
            else
                no problem, use exp directly

        Just like records however, we need to ensure that each let..in in each argument does not have
        clashing names.

    #]

    if List.any (fn o: o.stats /= []) argOuts then
        todo << location env pos .. " Compiler TODO: let..in inside function call argument is not YET implemented. =("
    else
        {
        , recycledInStats = Set.empty
        , stats = refOut.stats
        , recycledInValue = List.for refOut.recycledInValue argOuts fn o, a: Set.join a o.recycledInValue
        , value = WA.Call refOut.value (List.map (fn o: o.value) argOuts)
        }



translateTryAs as fn Env, TA.Expression, TA.FullType, [TA.Pattern & TA.Expression]: TranslateOut =
    fn env0, value, valueType, patternsAndExpressions:

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
                        , value = valueOut.expr
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
            testPattern pattern valueExpression []
            >> List.reverse
            >> WA.PatternMatchConditions

        namesAndExpressions as [ TA.FullType & Name & WA.Expression ] =
            translatePattern pattern valueExpression

        whenConditionMatches as [WA.Statement] =

            patternUnpackingStats =
                List.map namesAndExpressions fn type & name & v:
                    WA.VarDefinition { name, type, value = v }

            blockOut =
                translateExpression env2 block

            [
            , patternUnpackingStats
            , blockOut.stats
            , WA.Assignment resultName blockOut.value
            ]
            >> List.concat

        recycled1 =
            recycled0
            >> Set.join __ blockOut.recycledInStats
            >> Set.join __ blockOut.recycledInValue

        recycled1 & WA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression


    # 3. end
    default =
        WA.MissingPattern (location env pos) valueExpression

    recycledInValue1 & ifElseStat =
        List.forReversed (recycledInValue0 & default) patternsAndExpressions addTryPatternAndBlock

    {
    , recycledInStats = Set.empty
    , stats = List.concat [valueStats, [ifElseStat]]
    , recycledInValue = recycledInValue1
    , value = WA.Variable resultName
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
