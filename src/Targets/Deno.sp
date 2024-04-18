#
# Denotational semantics
#
# This is the first half of "normalization by evaluation", which we
# use to remove all need for anonymous functions and pointers-to-function,
# which a lot of lower-level targets do support.
#
# Note: I'm still not sure what a "denotational semantic" is. =(
# I read this stuff somewhere and I haven't noted down the source. >_<
#

[# TODO: we need to enforce recycling order!

    @counter = 0

    next =
        fn _:
        @counter += 1
        cloneUni @counter


    #
    # case 1
    #
    add =
        fn a, b:
        b - a

    # this will yield 1
    x =
        add (next 'none) (next 'none)

    # but this will yield -1
    x_ =
        (next 'none) - (next 'none)

    # solution?
    g1 = next 'none
    g2 = next 'none
    x__ =
        g2 - g1


    #
    # case 2
    #
    dup =
        fn a:
        a + a

    # this will yield (n + n)
    y =
        dup (next 'none)

    # this will yield (n + n + 1)
    y_ =
        (next 'none) + (next 'none)

    # solution?
    g3 = next 'none
    x__ =
        g3 + g3


    #
    # -------->
    #
    Functions whose body recycles/reassign a value are never inlined.
    Instead, their result is stored in a generated variable.
#]

Env =
    Dict Ref Value


#
# TODO if this works well, maybe I can merge it with EA?
#
var Value =
    , 'valueClosure Env Pos [ EA.Parameter ] EA.Expression
    , 'valueNeutral Neutral


var ArgValue =
    , 'argSpend Value
    , 'argRecycle [ Name ] Name


# This describes something that can't be further evaluated?
var Terminal =
    , # literals
      'literalText Text
    , 'literalNumber Number
    , # variables
      'localVariable Name
    , 'globalVariable TranslatedUsr
    , 'placeholderVariable Int
    , # the reference will never be a Value because then it could be a 'closure, and then we could evaluate it!
      'call Terminal [ ArgValue ]
    , 'conditional Terminal Value Value
    , 'and [ Terminal ]
    , 'isLiteralText Text Terminal
    , 'isLiteralNumber Text Terminal
    , 'letIn
          {
          , inExpression as Value
          # The let-expression must never be a function, if it is a function we *must* inline it instead!
          , letExpression as Terminal
          , maybeName as Maybe Name
          , type as TA.FullType
          }
    , 'constructor TranslatedUsr
    , 'constructorAccess Int Terminal
    , 'isConstructor USR Terminal
    , 'literalRecord (Maybe Terminal) [ AttrName & Value ]
    , 'recordAccess AttrName Terminal
    , 'missingPattern Text Terminal
    , 'introspect Self.Self


#
#
# Emittable to Deno
#
#

Env =
    {
    , globalsToEliminate as Dict USR Value
    , localsToEliminate as Dict Name Value
    , placeholdersToEliminate as Dict Int Value
    }


eval as fn Env, TA.Expression: Value =
    fn env, expression:
    try expression as

        EA.'fn pars body:
            'valueClosure env pos pars body

        EA.'call ref arguments:
            argValues =
                List.map (evalArg env __) arguments

            try eval env ref as

                'valueNeutral neutral:
                    'valueNeutral ('call neutral argValues)

                'valueClosure env pos pars body:
                    List.map2 (fn p, a: p & a) pars argValues
                    >> List.for env __ insertArgInEnv
                    >> eval __ body

        EA.'letIn { inExpression, letExpression, maybeName, type }:
            xxxx

        EA.'localVariable name:
            try Dict.get ref env.localsToEliminate as
                'nothing: 'valueNeutral ('localVariable pos ref)
                'just v: v

        EA.'globalVariable translatedUsr:
            try Dict.get ref env.globalsToEliminate as
                'nothing: 'valueNeutral ('globalVariable translatedUsr)
                'just v: v

        EA.'placeholderVariable index:
            try Dict.get ref env.placeholdersToEliminate as
                'nothing: 'valueNeutral ('placeholderVariable index)
                'just v: v

        EA.'conditional test true false:
            'conditional (eval env test) (eval env true) (eval env false) >> 'valueNeutral

        EA.'and exprs:
            exprs
            >> List.map (eval env __)
            >> 'valueNeutral

        EA.'literalText text:
            'literalText text >> 'valueNeutral

        EA.'literalNumber n:
            'literalNumber n >> 'valueNeutral


[#
        EA.'isLiteralText Text Expression
        EA.'isLiteralNumber Number Expression
        EA.'constructor TranslatedUsr
        EA.'constructorAccess Int Expression
        EA.'isConstructor USR Expression
        EA.'literalRecord (Maybe Expression) [ AttrName & Expression ]
        EA.'recordAccess AttrName Expression
        EA.'missingPattern Text Expression
        EA.'introspect Self.Self
        #]

