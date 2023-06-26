#
# Normalization by Evaluation
#
#
# This is used to remove all user function calls and closures necessary,
# which in turn is necessary to emit to WGSL which does not support
# first-class functions.
#


alias Env =
    [ Ref & Value ]



# Where does this come from?
union Value =
    , Closure Env Pos [TA.Parameter] TA.Expression TA.FullType
    , Neutral Neutral



# This describes something that can't be further evaluated?
union Neutral =
    , NVar Ref

    # the reference cannot be a Value because then it could be a Closure, and then we could evaluate it!
    , NCall Neutral [Value]



eval as fn Env, TA.Expression: Value =
    fn env, expression:

    try expression as
        , TA.Variable _ ref:
            try List.find (fn n & v: n == ref) env as
                # If the var is not specified, then it's one that should not be eliminated
                , Nothing: Neutral (NVar ref)
                , Just (n & v): v

        , TA.Fn pos pars body fullType:
            Closure env pos pars body fullType

        , TA.Call pos ref args:

            argValues =
                List.map evalArgs args

            try eval env ref as
                , Neutral neutral:
                    Neutral (NCall neutral argValues)

                , Closure env pos pars body fullType:
                    List.map2 (fn p, a: p & a) pars argValues
                    >> List.for env __ insertArgInEnv
                    >> eval __ body



fresh as fn [Name], Name: Name =
    fn allNames, name:
    if List.member name allNames then
        name .. "'"
    else
        name



reify as fn [Name], Value: Expression =
    fn allNames, value:

    try value as
        , Closure env pars body:

            freshPars & localAllNames & localEnv =
                List.forReversed ([] & allNames & env) pars fn par, pas & an & e:
                  f = fresh an par
                  [f, ...pas] & [f, ...an] & [par & Neutral (NVar f), ...e]

            bodyValue = eval localEnv body
            Fn freshPars (reify localAllNames bodyValue)

        , Neutral neutral:
            reifyNeutral allNames neutral



reifyNeutral as fn [Name], Neutral: Expression =
    fn allNames, neutral:
    try neutral as

        , NVar name:
            Var name

        , NCall refNeutral argValues:
            Call (reifyNeutral allNames refNeutral) (List.map (reify allNames __) argValues)



nf as fn Env, Expression: Expression =
    fn env, expr:
    reify [] (eval env expr)

