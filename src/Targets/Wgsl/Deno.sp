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

    # the reference will never be a Value because then it could be a Closure, and then we could evaluate it!
    , NCall Neutral [ArgValue]



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
        , TA.ParameterPattern fullTypeP pattern & ArgSpend expression:
            todo "..."


        , TA.ParameterRecycle posP rawType nameP & ArgRecycle attrPath nameA:
            todo "..."

        , _:
            todo "compiler bug"



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
