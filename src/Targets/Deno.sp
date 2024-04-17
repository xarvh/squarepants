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



var Value =
    , 'valueClosure Env Pos [EA.Parameter] EA.Expression TA.FullType
    , 'valueNeutral Neutral


var ArgValue =
    , 'argSpend Value
    , 'argRecycle [Name] Name


# This describes something that can't be further evaluated?
var Neutral =
    , Var Pos Ref
    , Recycle [Name] Name

    # the reference will never be a Value because then it could be a Closure, and then we could evaluate it!
    , Call Neutral [ArgValue]

    , Num Number
    , IsNum Number Value

    , Cons USR
    , IsCons Name Value
    , AccessConstructorArgument Int Value

    , Record (Maybe Value) (Dict Name Value)
    , AccessRecordAttribute Name Value

    , If Value Value Value
    , Try Value [(fn Value: [Value]) & Value]
    , Destroy Name Value



# This describes something that can't be further evaluated?
var Terminal =
    # literals
    , 'literalText Text
    , 'literalNumber Number

    # variables
    , 'localVariable Name
    , 'globalVariable TranslatedUsr
    , 'placeholderVariable Int

    # the reference will never be a Value because then it could be a 'closure, and then we could evaluate it!
    , 'call Terminal [ ArgValue ]
    , 'conditional Terminal Value Value
    , 'and [ Terminal ]
    , 'shallowEqual Terminal Terminal
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


