

alias Env =
    {
    , genVarCounter as Int
    , module as TA.Module
    }




location as fn Env, Pos: Text =
    fn env, pos:

    (Error.posToHuman { fsPath = env.module.fsPath, content = env.module.asText } pos).location




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


translatePattern as fn TA.Pattern, EA.Expression: [ TA.FullType & Name & EA.Expression ] =
    fn pattern, accessExpr:

    translatePatternRec pattern accessExpr []
# TODO we can do this optimization only once we can replace the pattern variable name with the accessExpr variable in the block that uses it
#    try pattern as
#        TA.PatternAny _ _ _: []
#        _: translatePatternRec pattern accessExpr []


translatePatternRec as fn TA.Pattern, EA.Expression, [ TA.FullType & Name & EA.Expression ]: [ TA.FullType & Name & EA.Expression ] =
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
                translatePatternRec pa (EA.ConstructorAccess index accessExpr) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                translatePatternRec pa (EA.RecordAccess name accessExpr) a


testPattern as fn TA.Pattern, EA.Expression, [EA.Expression]: [EA.Expression] =
    fn pattern, valueToTest, accum:
    try pattern as

        , TA.PatternAny _ _:
            accum

        , TA.PatternLiteralText _ text:
            EA.CompareWithLiteralText text valueToTest :: accum

        , TA.PatternLiteralNumber _  num:
            EA.CompareWithLiteralNumber num valueToTest :: accum

        , TA.PatternConstructor _ (USR umr name) pas:
            (EA.IsConstructor name valueToTest :: accum)
            >> List.indexedFor __ pas fn index, argPattern, a:
                testPattern argPattern (EA.ConstructorAccess index valueToTest) a

        , TA.PatternRecord _ attrs:
            accum >> Dict.for __ attrs fn name, (pa & type), a:
                testPattern pa (EA.RecordAccess name valueToTest) a



translateArgAndType as fn Env, TA.Argument: EA.Argument =
    fn env, taArg:

    try taArg as
        , TA.ArgumentExpression fullType exp:
            EA.ArgumentSpend fullType (translateExpression env exp)

        , TA.ArgumentRecycle pos rawType attrPath name:
            EA.ArgumentRecycle rawType attrPath name


[#

    TODO: Explain why are we doing this batshit mess


#]
alias TranslateBase value =
    {
    , recycledInStats as Set Name
    , stats as [EA.Statement]
    , recycledInValue as Set Name
    , value as value
    }


alias TranslateOut =
    TranslateBase EA.Expression


none as fn value: TranslateBase value =
    fn value:
    {
    , recycledInStats = Set.empty
    , stats as []
    , recycledInValue = Set.empty
    , value
    }


merge as fn TranslateBase a: TranslateBase b, (fn a, b: c): TranslateBase c =
    fn ta, tb, f:
    {
    , recycledInStats = Set.join ta.recycledInStats tb.recycledInStats
    , stats = List.concat ta.stats tb.stats
    , recycledInValue = Set.join ta.recycledInValue tb.recycledInValue
    , value = f ta.value tb.value
    }





translateExpression as fn Env, TA.Expression: TranslateOut =
    fn env, expression:

    try expression as
        , TA.LiteralNumber _ num:
            none << EA.LiteralNumber num

        , TA.LiteralText _ text:
            none << EA.LiteralText text

        , TA.Variable _ ref:
            none << EA.Variable ref

        , TA.Constructor _ usr:
            none << EA.Constructor usr

        , TA.RecordAccess _ attrName exp:
            { translateExpression env exp with expr = EA.RecordAccess attrName .expr }

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
                , value = EA.IfExpression conditionOut.value trueOut.value falseOut.value
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
                    EA.Conditional
                        conditionOut.value
                        (List.concat trueOut.stats [ EA.Evaluation ... ])
                        ()
                }
                #]

        , TA.Try pos { value, valueType, patternsAndExpressions }:
            translateTryAs env value valueType patternsAndExpressions

        , TA.LetIn valueDef e bodyType:
            try pickMainName valueDef.pattern as
                , NoNamedVariables:
                    EA.LetIn
                        {
                        , maybeName = Nothing
                        , type = valueDef.type
                        , letExpression = translateExpression env valueDef.body
                        , inExpression = translateExpression env e
                        }

                , TrivialPattern defName type:
                    EA.LetIn
                        {
                        , maybeName = Just defName
                        , type
                        , letExpression = translateExpression env valueDef.body
                        , inExpression = translateExpression env e
                        }

                , GenerateName:
                    mainName & newEnv =
                        # TODO check if valueDef.body is just a variable
                        generateName env

                    namesAndExpressions =
                        translatePattern valueDef.pattern (EA.Variable (RefLocal mainName))

                    wrapWithUnpackedPatternVar as fn (TA.FullType & Name & EA.Expression), EA.Expression: EA.Expression =
                        fn (type & name & letExpression), inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just name
                            , type
                            , letExpression
                            , inExpression
                            }

                    wrapWithActualLetIn as fn EA.Expression: EA.Expression =
                        fn inExpression:
                        EA.LetIn
                            {
                            , maybeName = Just mainName
                            , type = valueDef.type
                            , letExpression = translateExpression newEnv valueDef.body
                            , inExpression
                            }

                    translateExpression newEnv e
                    >> List.forReversed __ namesAndExpressions wrapWithUnpackedPatternVar
                    >> wrapWithActualLetIn

        , TA.DestroyIn name e:
            translateExpression env e



translateFn as fn Env, [TA.Parameter], TA.Expression: TranslateOut =

    bodyOut =
        translateExpression { env with genVarCounter = List.length taPars + .genVarCounter } body

    eaBody =
        # TODO use EA.Evaluation instead of EA.Return if bodyT is `None`?
        List.concat [ bodyOut.stats, [ EA.Return bodyOut.expr ] ]

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

    { recycledInStats = Set.none, stats = [], recycledInValue, expr = EA.Fn eaPars wrappedBody }



alias ParameterAcc =
    {
    , env as Env
    , recycledInValue as Set Name
    , body as [EA.Statement]
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
                        translatePattern pa (EA.Variable (RefLocal mainName))

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
                    , stats = List.concat .stats [ EA.VarDefinition varName ??? .expr ]
                    , recycledInValue = Set.empty
                    , value = EA.Variable (RefLocal varName)
                    }

            newEnv & merge acc ooo fn attrs, value: Dict.insert name value attrs
    #]

    eaMaybeExtend =
      try taMaybeExtends as
          Nothing:
              none Nothing

        Just taExtends:
            extendsOut =
                translateExpression attrsEnv taExtends

              {
              , recycledInStats = extendsOut.recycledInStats
              , stats = extendsOut.stats
              , recycledInValue = extendsOut.recycledInValue
              , value = Just extendsOut.value
              }

    out =
        merge extendsOut attrsOut fn ext, att: EA.LiteralRecord (Just ext) att

    if out.stats /= [] then
        todo (location env pos .. " Compiler TODO: let..in inside record literal is not YET implemented. =("
    else
        out



translateCall as fn Env, TA.Expression, [TA.Argumen]: TranslateOut =
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
        todo (location env pos .. " Compiler TODO: let..in inside function call argument is not YET implemented. =("
    else
        {
        , recycledInStats = Set.empty
        , stats = refOut.stats
        , recycledInValue = List.for refOut.recycledInValue argOuts fn o, a: Set.join a o.recycledInValue
        , value = EA.Call refOut.value (List.map (fn o: o.value) argOuts)
        }



translateTryAs as fn Env, TA.Expression, TA.FullType, [TA.Pattern & TA.Expression]: TranslateOut =
    fn env0, value, valueType, patternsAndExpressions:

    # 1. create a name for the value (unless it's already a variable)
    { valueExpr, valueStats, recycledInValue0 & env1 } =
        try value & valueType.uni as
            , TA.Variable _ ref & Imm:
                {
                , valueExpr = EA.Variable ref
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
                    EA.VarDefinition
                        {
                        , name = tryName
                        , type = valueType
                        , value = valueOut.expr
                        }

                {
                , valueExpr = EA.Variable (RefLocal tryName)
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

    addTryPatternAndBlock as fn TA.Pattern & TA.Expression, EA.Statement & Set Name: EA.Statement & Set Name =
        fn pattern & block, nextTryStatement & recycled0:

        testIfPatternMatches as EA.Expression =
            testPattern pattern valueExpression []
            >> List.reverse
            >> EA.PatternMatchConditions

        namesAndExpressions as [ TA.FullType & Name & EA.Expression ] =
            translatePattern pattern valueExpression

        whenConditionMatches as [EA.Statement] =

            patternUnpackingStats =
                List.map namesAndExpressions fn type & name & value:
                    EA.VarDefinition { name, type, value }

            blockOut =
                translateExpression env2 block

            [
            , patternUnpackingStats
            , blockOut.stats
            , EA.Assignment resultName blockOut.value
            ]
            >> List.concat

        recycled1 =
            recycled0
            >> Set.join __ blockOut.recycledInStats
            >> Set.join __ blockOut.recycledInValue

        recycled1 & EA.Conditional testIfPatternMatches whenConditionMatches nextTryExpression


    # 3. end
    default =
        EA.MissingPattern (location env pos) valueExpression

    recycledInValue1 & ifElseStat =
        List.forReversed (recycledInValue0 & default) patternsAndExpressions addTryPatternAndBlock

    {
    , recycledInStats = Set.empty
    , stats = List.concat [valueStats, [ifElseStat]]
    , recycledInValue = recycledInValue1
    , value = EA.Variable resultName
    }



#
#
#



translateRootValueDef as fn Env, TA.ValueDef, ByUsr EA.GlobalDefinition: ByUsr EA.GlobalDefinition =
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

            mainDef as EA.GlobalDefinition =
                {
                , usr = mainUsr
                , expr = translateExpression newEnv def.body
                , deps
                }

            accum
            >> Dict.insert mainUsr mainDef __
            >> List.for __ (translatePattern def.pattern (EA.Variable (RefGlobal mainUsr))) fn (type & name & expr), z:
                subUsr = USR env.module.umr name
                Dict.insert subUsr { usr = subUsr, expr, deps = Set.ofOne mainUsr } z


#
# Main
#


circularIsError as fn ByUsr EA.GlobalDefinition, [USR]: Bool =
    fn globalDefsByName, usrs:

    zzz =
      fn usr:
      try Dict.get usr globalDefsByName as
          , Nothing:
              # native or some special stuff?
              False

          , Just globalDef:
              try globalDef.expr as
                  , EA.Fn _ _: False
                  , _: True

    List.any zzz usrs


circularToError as fn [USR]: Error =
    fn usrs:

    Error.Raw
        [ "circular dependency: "
        , ... List.map toHuman usrs
        ]


translateAll as fn UMR, [TA.Module]: Res { entryUsr as USR, defs as [EA.GlobalDefinition] } =
    fn entryModule, modules:

    Debug.benchStart None

    globalDefsByName as ByUsr EA.GlobalDefinition =
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
