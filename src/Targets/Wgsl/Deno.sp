#
# Denotational semantics
#
# This is the first half of "normalization by evaluation", which we
# use to remove all need for anonymous functions and pointers-to-function,
# which WGSL does not support.
#
# We also use this pass to unpack patterns.
#
# Note: I'm still not sure what a "denotational semantic" is. =(
#


alias Env =
    Dict Ref Value



union Value =
    , Closure Env Pos [TA.Parameter] TA.Expression TA.FullType
    , Neutral Neutral


union ArgValue =
    , ArgSpend Value
    , ArgRecycle [Name] Name


# This describes something that can't be further evaluated?
union Neutral =
    , NVar Pos Ref
    , NUni [Name] Name

    # the reference will never be a Value because then it could be a Closure, and then we could evaluate it!
    , NCall Neutral [ArgValue]

    , NNum Number
    , NCons USR
    , NRecord (Maybe Value) (Dict Name Value)
    , NAccessConstructorArgument Int Value
    , NAccessRecordAttribute Name Value
    , NIf Value Value Value
    , NDestroy Name Value


eval as fn Env, TA.Expression: Value =
    fn env, expression:

    try expression as
        , TA.Variable pos ref:
            try Dict.get ref env as
                # If the var is not specified, then it's one that should not be eliminated
                , Nothing: Neutral (NVar pos ref)
                , Just v: v

        , TA.Fn pos pars body fullType:
            Closure env pos pars body fullType

        , TA.Call pos ref args:

            argValues =
                List.map (evalArgs env __) args

            try eval env ref as
                , Neutral neutral:
                    Neutral (NCall neutral argValues)

                , Closure env pos pars body fullType:
                    List.map2 (fn p, a: p & a) pars argValues
                    >> List.for env __ insertArgInEnv
                    >> eval __ body

        , TA.LetIn def body type:
            # TODO inline only if (used only once) or (type is function)
            # TODO what about uniques?
            # TODO check for recursives?
            env
            >> insertPatternInEnv [] def.pattern (eval env def.body) __
            >> eval __ body

        , TA.LiteralNumber pos n:
            Neutral << NNum n

        , TA.LiteralText pos text:
            todo "text not supported"

        , TA.Constructor pos usr:
            Neutral << NCons usr

        , TA.Record pos maybeExt attrs:
            NRecord
                (Maybe.map (eval env __) maybeExt)
                (Dict.map (fn k, v: eval env v) attrs)
            >> Neutral

        , TA.RecordAccess pos name expr:
            expr
            >> eval env __
            >> NAccessRecordAttribute name __
            >> Neutral

        , TA.If pos { condition, true, false }:
            NIf
                (eval env condition)
                (eval env true)
                (eval env false)
            >> Neutral

        , TA.Try pos { value, valueType, patternsAndExpressions }:
            todo "..."

        , TA.DestroyIn name expr:
            NDestroy name (eval env expr)
            >> Neutral

        , TA.Error pos:
            todo "compiler bug"


evalArgs as fn Env, TA.Argument: ArgValue =
    fn env, taArg:

    try taArg as
        , TA.ArgumentRecycle pos rawType attrPath varName:
            ArgRecycle attrPath varName

        , TA.ArgumentExpression fullType expr:
            ArgSpend (eval env expr)




insertArgInEnv as fn TA.Parameter & ArgValue, Env: Env =
    fn par & arg, env:

    try par & arg as

        , TA.ParameterRecycle posP rawType nameP & ArgRecycle attrPath nameA:
            Dict.insert (RefLocal nameP) (Neutral << NUni attrPath nameA) env

        , TA.ParameterPattern fullTypeP pattern & ArgSpend expression:
            insertPatternInEnv [] pattern expression env

        , _:
            todo "compiler bug"



insertPatternInEnv as fn [fn Value: Value], TA.Pattern, Value, Env: Env =
    fn accessors, pattern, value, env:

    try pattern as
        , TA.PatternAny pos { maybeName, type }:

            try maybeName as
                , Nothing:
                    env

                , Just name:
                    Dict.insert (RefLocal name) (List.for value accessors fn a, e: a e) env

        , TA.PatternLiteralNumber _ _:
            env

        , TA.PatternLiteralText _ _:
            env

        , TA.PatternConstructor pos usr args:
            List.indexedFor env args fn index, arg, envX:
                insertPatternInEnv [fn v: Neutral (NAccessConstructorArgument index v), ...accessors] arg value envX

        , TA.PatternRecord pos attrs:
            Dict.for env attrs fn name, arg & ty, envX:
                insertPatternInEnv [fn v: Neutral (NAccessRecordAttribute name v), ...accessors] arg value envX





[#
fresh as fn [Name], Name: Name =
    fn allNames, name:
    if List.member name allNames then
        name .. "'"
    else
        name



reify as fn [Name], Value: TA.Expression =
    fn allNames, value:

    try value as
        , Closure env pos pars body fullType:

            freshPars & localAllNames & localEnv =
                List.forReversed ([] & allNames & env) pars fn par, pas & an & e:
                  f = fresh an par
                  [f, ...pas] & [f, ...an] & [par & Neutral (NVar f), ...e]

            bodyValue = eval localEnv body
            Fn freshPars (reify localAllNames bodyValue)

        , Neutral neutral:
            reifyNeutral allNames neutral



reifyNeutral as fn [Name], Neutral: TA.Expression =
    fn allNames, neutral:
    try neutral as

        , NVar pos ref:
            TA.Variable pos ref

        , NCall refNeutral argValues:
            todo "TA.Call (reifyNeutral allNames refNeutral) (List.map (reify allNames __) argValues)"



nf as fn Env, TA.Expression: TA.Expression =
    fn env, expr:
    reify [] (eval env expr)

#]
