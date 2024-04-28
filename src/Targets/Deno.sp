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
    Functions whose body recycles/reassign a non-argument value are never inlined.
    Instead, their call result is stored in a generated variable.




---->
functions can be inlined at the call point

    dup =
        fn a:
        a + a

    y =
        dup (next 'none)

instead of replacing non-recycle arguments directly with the expression, we want to always assign them to a variable:

    y =
        g1 = 'none
        g2 = next g1
        g3 = dup g2

    y =
        g1 =
            'none

        g2 =
            g21 = 1
            increment @counter g21
            cloneUni @counter

        g3 =
            g2 + g2

        g3

* If a variable is used 0 times:
    remove the variable
    f the expression is pure, remove also the expression

* If a variable is used 1 time, and its expression is pure
    inline it

* non-pure expressions can be moved up
    (each definition can have a stack of prior statements?)

    increment @counter 1

    g2 =
        cloneUni @counter

    y =
        g2 + g2



------------------>

    !counter =
        0

    next =
        fn _:
        @counter += 1
        cloneUni @counter

    trip =
        fn a, b:
        a + a + b

    y =
        trip (next 'none) (next 'none)

1. transform arguments into variables
2. inline
3. if a variable is not used, remove its definition.
    if, in addition, its expression is pure, remove the expression
3. if a variable is used exactly once and its expression is pure, inline the variable.

"is pure" -> "does not contain any reassignment OR Debug.* function"

    1.
        g1 =
            g11 = 'none
            next g11

        g2 =
            g21 = 'none
            next g21

        trip g1 g2

    2.
        g1 =
            g11 = 'none
            inc @counter 1
            cloneUni @counter

        g2 =
            g21 = 'none
            inc @counter 1
            cloneUni @counter

        g1 + g1 + g2

    3.
        g11 = 'none
        g1 =
            inc @counter 1
            cloneUni @counter

        g21 = 'none
        g2 =
            inc @counter 1
            cloneUni @counter

        y =
            g1 + g1 + g2



----> But what if `next` is passed as a parameter?
        => Then we need to inline any function that takes at least one function argument



try eaExpr as
    EA.'call ref args:

        -> All recursive refs should be pulled out prior to this step <-

        inline? =
              any of the arguments is a function
              OR
              ref is defined locally

        if inline:

            if we don't have the exact function definition because that is passed as an argument, we error

            for every argument
                recurse on the argument, and assign to a generated var

            insert the function's body, with the generated vars replacing the argument names


        ----> What about recursive functions!?
            * SpirV supports recursion
            * But what if I have a 1) locally defined 2) impure 3) recursive function?

            zot =
                fn x, y, z:

                !counter =
                    0

                rec =
                    fn a:
                    inc @counter 1
                    some non-tail-call expression

                rec ...




----> For SpirV I need to remove locally defined functions
How do I remove them if they are recursive?


    doExpression =
        fn env, @state, expr:

        rec =
            fn e:
            if something e then
                bleh
            else
                rec @state env e

        try expr as
            'blah ->
                List.for Dict.empty attrs (rec ... )


  => Devo tirarla fuori:



    rec =
        fn env, @state, expr, e:
        if something e then
            bleh
        else
            rec @state env e


    doExpression =
        fn env, @state, expr:

        try expr as
            'blah ->
                List.for Dict.empty attrs (rec env @state expr ... )




========================================================

SpirV requires extraction, so let's forget inlining for now and focus on that.

Extracting a rec function means that any function passed must be passed with its full closure, which also means monomorphization.

=======================================================


Ok, no, we just need to get rid of closures passed as arguments.

This means that to pass a function A as an argument to another function B, we must also pass A's closure.
In turn, this requires to monomorphize B.

Then we can think about inlining, but at this point that is just an optimization.


    We have a function call
        We need a monomorphized version of the function ref
        Then we need to replace every argument that contains a function with 



=======================================================

function calls:

    if ref is local
        if recursive and takes function arguments
            NEEDS CLOSURE PASSING
        else if recursive
            pull it out
        else
            inline it
    else if ref is root
        if is recursive and takes function arguments
            NEEDS CLOSURE PASSING
        else if takes function arguments
            inline
    else
        need to use a function pointer



----> I can just say "LOL ffns don't support recursion", at least for now.


How do I inline bullshit like


    f =
        (if blah then
            fn x, z, y: z + y
        else
            functionFactory meh
        )

    f 1 2 3


1. I need to aggressively inline even root functions


    f =
        (if blah then
            fn x, z, y: z + y
        else
            (fn m: fn x, z, y: z + m) meh
        )

    f 1 2 3


    f =
        ((fn test, t, f: ) blah
            (fn x, z, y: z + y)
            (fn x, z, y: z + meh)
        )

    f 1 2 3


lambda calculus for conditionals (https://medium.com/@marinalimeira/understanding-conditional-expressions-with-%CE%BB-calculus-e25393fef86c)


    f = blah (fn x, z, y: z + y) (fn x, z, y: z + meh)











#]

Env =
    Dict Ref Value


#
# TODO if this otimization works well, maybe I can merge this AST with EA's?
#
var Value =
    , 'valueClosure CallMode Env Pos [ EA.Parameter ] EA.Expression
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
            # TODO inline only if (used only once) or (type is function)
            # TODO what about uniques?
            # TODO check for recursives?
            env
            >> insertPatternInEnv [] def.pattern (eval env def.body) __
            >> eval __ body

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

