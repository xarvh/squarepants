
#
# TODO remove references to Compiler/MakeEmittable
#

alias Env = {
    , mutables as Set JA.Name
    , errorEnv as Error.Env
    , overrides as Dict EA.Name Override
    }


#alias Override =
#    Env: [EA.Expression & EA.Mutability]: JA.Expr

union Override = Override (Env: [EA.Expression & EA.Mutability]: JA.Expr)


# Adding a None argument to prevent a spurious circular dependency error
coreOverrides as Dict EA.Name Override =

    corelib as Text: Text: Meta.UniqueSymbolReference =
        m: n:
        Meta.USR (Meta.UMR Meta.Core m) n

    [
    , Prelude.unaryPlus.usr & unaryPlus
    , Prelude.unaryMinus.usr & unaryMinus
    #
    , Prelude.add.usr & binop "+"
    , Prelude.multiply.usr & binop "*"
    , Prelude.subtract.usr & binop "-"
#    , Prelude.mutableAssign.usr & binop "="
    , Prelude.mutableAdd.usr & binop "+="
    , Prelude.mutableSubtract.usr & binop "-="
    , Prelude.textConcat.usr & binop "+"
    , Prelude.greaterThan.usr & binop ">"
    , Prelude.lesserThan.usr & binop "<"
    , Prelude.greaterOrEqualThan.usr & binop ">="
    , Prelude.lesserOrEqualThan.usr & binop "<="
    , Prelude.or_.usr & binop "||"
    , Prelude.and_.usr & binop "&&"
    #
    , CoreTypes.true & constructor "true"
    , CoreTypes.false & constructor "false"
    , CoreTypes.noneValue & constructor "null"
    #
    , Prelude.divide.usr & function  "sp_divide"
    , Prelude.listCons.usr & function  "sp_cons"
    , Prelude.equal.usr & function  "sp_equal"
    , Prelude.notEqual.usr & function  "sp_not_equal"
    , corelib "Basics" "modBy" & function "basics_modBy"
    , corelib "Basics" "round" & function "Math.round"
    #
    , Prelude.debugLog.usr & function "sp_log"
    , Prelude.debugTodo.usr & function "sp_todo"
    , Prelude.debugToHuman.usr & function "sp_toHuman"
    , Prelude.debugBenchStart.usr & function "sp_benchStart"
    , Prelude.debugBenchStop.usr & function "sp_benchStop"
    , Prelude.compare.usr & function "basics_compare"
    #
    , corelib "Text" "fromNumber" & function  "text_fromNumber"
    , corelib "Text" "toNumber" & function  "text_toNumber"
    , corelib "Text" "split" & function  "text_split"
    , corelib "Text" "length" & function  "text_length"
    , corelib "Text" "slice" & function  "text_slice"
    , corelib "Text" "startsWith" & function  "text_startsWith"
    , corelib "Text" "startsWithRegex" & function  "text_startsWithRegex"
    , corelib "Text" "replaceRegex" & function  "text_replaceRegex"
    , corelib "Text" "trimLeft" & function  "text_trimLeft"
    , corelib "Text" "dropLeft" & function  "text_dropLeft"
    , corelib "Text" "forEach" & function  "text_forEach"
    #
    , corelib "Hash" "empty" & function  "hash_empty"
    , corelib "Hash" "insert" & function  "hash_insert"
    , corelib "Hash" "remove" & function  "hash_remove"
    , corelib "Hash" "get" & function  "hash_get"
    , corelib "Hash" "for" & function  "hash_for"
    , corelib "Hash" "each" & function  "hash_each"
    #
    , corelib "Array" "push" & function  "array_push"
    , corelib "Array" "pop" & function  "array_pop"
    , corelib "Array" "get" & function  "array_get"
    , corelib "Array" "set" & function  "array_set"
    , corelib "Array" "sortBy" & function  "array_sortBy"
    , corelib "Array" "fromList" & function  "array_fromList"
    , corelib "Array" "toList" & function  "array_toList"
    #
    , corelib "List" "sortBy" & function  "list_sortBy"
    ]
    >> Dict.fromList
    >> Dict.mapKeys Compiler/MakeEmittable.translateUsr


unaryPlus as Override =
    Override env: arguments:

    try arguments as
        [ arg ]:
            # Num.unaryPlus n == n
            translateArg {nativeBinop = False } env arg

        []:
            # Num.unaryPlus == a: a
            JA.SimpleLambda ["a"] (JA.Var "a")

        _:
            todo "compiler bug: wrong number of arguments for binop"


unaryMinus as Override =
    Override env: arguments:

    try arguments as
        [ arg ]:
            # Num.unaryMinus n == -n
            JA.Unop "-" (translateArg { nativeBinop = False } env arg)

        []:
            # Num.unaryMinus == n: -n
            JA.SimpleLambda ["v"] (JA.Unop "-" (JA.Var "v"))

        _:
            todo "compiler bug: wrong number of arguments for binop"


binop as Text: Override =
    jsOp:

    Override env: arguments:

    try arguments as
        [ right, left ]:
            # Num.subtract b a == a - b
            JA.Binop jsOp
                (translateArg { nativeBinop = True} env left)
                (translateArg { nativeBinop = True} env right)

        [ right ]:
            # Num.subtract b == a: a - b
            JA.SimpleLambda ["a"] (JA.Binop jsOp (JA.Var "a") (translateArg { nativeBinop = True } env right))

        []:
            # Num.subtract == b: a: a - b
            JA.SimpleLambda ["b"] (JA.SimpleLambda ["a"] (JA.Binop jsOp (JA.Var "a") (JA.Var "b")))

        _:
            todo "compiler bug: wrong number of arguments for binop"


constructor as Text: Override =
    jsValue:

    Override env: arguments:

    JA.Var jsValue


function as Text: Override =
    jaName:

    Override env: arguments:

    JA.Var jaName
    >> wrapCalls env arguments


translateVariable as Env: Name: [Name]: [EA.Expression & EA.Mutability]: JA.Expr =
    env: valueName: attrPath: eaArgs:


    try Dict.get valueName env.overrides as
        Just (Override override):
            override env eaArgs
            >> accessAttrs attrPath

        Nothing:
            if Set.member valueName env.mutables then
                JA.Var valueName
                >> unwrapMutable
                >> accessAttrs attrPath
                >> clone

            else
                JA.Var valueName
                >> accessAttrs attrPath
                >> wrapCalls env eaArgs


wrapCalls as Env: [EA.Expression & EA.Mutability]: JA.Expr: JA.Expr =
    env: exprAndMutability: baseExpr:

#    if baseExpr == JA.Var "hash_insert" then
#        log "---->" baseExpr
#        None
#    else
#        None

    baseExpr >> List.for exprAndMutability arg: accum:
#        JA.Call accum [translateExpressionToExpression env (Tuple.first arg)]
        JA.Call accum [translateArg { nativeBinop = False } env arg]



accessAttrs as [Text]: JA.Expr: JA.Expr =
    attrPath: e:
    List.for attrPath JA.AccessWithDot e


accessAttrsButTheLast as Text: [Text]: JA.Expr: ( JA.Expr & Text ) =
    attrHead: attrTail: e:

    fold =
        attr: ( expr & last ):
        ( JA.AccessWithDot last expr & attr)

    List.for attrTail fold ( e & attrHead )


translateArg as { nativeBinop as Bool }: Env: (EA.Expression & EA.Mutability): JA.Expr =
    stuff: env: (eaExpression & mutability):
    try mutability as
        EA.Immutable:
            translateExpressionToExpression env eaExpression

        EA.Mutable:
            try eaExpression as
                EA.Variable name attrPath:
                    if stuff.nativeBinop then
                        #SP: @x.a.b += blah
                        #JS: x.obj[x.attr].a.b += blah
                        JA.Var name
                        >> unwrapMutable
                        >> accessAttrs attrPath

                    else
                        try attrPath as
                            []:
                                #SP: doStuff @x
                                #JS: doStuff(x)
                                JA.Var name

                            head :: tail:
                                #SP: doStuff @x.a.b.c
                                #JS: doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })
                                JA.Var name
                                >> unwrapMutable
                                >> accessAttrsButTheLast head tail
                                >>
                                   (( wrappedExpr & lastAttrName ):
                                        Dict.empty
                                            >> Dict.insert "obj" wrappedExpr
                                            >> Dict.insert "attr" (literalString lastAttrName)
                                            >> JA.Record
                                   )

                _:
                    todo "translateArg: this should not happen!"



[#
# Mutation
#

  CA.Definition: `x @= 0`

     => `const x = { obj : { m : 0 }, attr : 'm' };`



  CA.Variable: `x`

     => `$clone$(x.obj[x.attr])`

     (when we have a non-variable type we can optimize this)



  CA.Call (mutation primitive): `@x += 1`

      CA.ArgumentMutable, fn is primitive: `@x := 0`

        => `x.obj[x.attr] := 0`


      CA.ArgumentMutable, fn is NOT primitive
        if attrPath == [] then `doStuff @x`

          => `doStuff(x)`


        else: `doStuff @x.a.b.c`

          => `doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })



#]




wrapMutable as EA.Mutability: JA.Expr: JA.Expr =
    mutability: expr:

    try mutability as
        EA.Mutable:
            Dict.empty
            >> Dict.insert "attr" (literalString "$")
            >> Dict.insert "obj" (JA.Record << Dict.singleton "$" expr)
            >> JA.Record

        EA.Immutable:
            expr


maybeCloneMutable as EA.Mutability: JA.Expr: JA.Expr =
    mutability: expr:

    try mutability as
        EA.Immutable:
            expr

        EA.Mutable:
            clone expr


unwrapMutable as JA.Expr: JA.Expr =
    ja_x:
#    ja_x =
#        JA.Var x
    # $x.obj[$x.attr]
    JA.AccessWithBrackets (JA.AccessWithDot "attr" ja_x) (JA.AccessWithDot "obj" ja_x)


clone as JA.Expr: JA.Expr =
    expr:
    JA.Call (JA.Var "sp_clone") [ expr ]



[#
 # Name translations
 #


   We need to give JS names to
     - the variables declared within the SP code
     - the variables we generate as part of the SP->JS translation

   and we need to ensure that these names do not clash with each other
   and do not clash with JS reserved keywords.

   So when we need a new naming schema, we follow this:

   * no $ anywhere
     => it's a native JS value

   * starts and ends with a single $
     => $clone$

   * starts with a single $
     * followed by a letter
       => it corresponds to an SP path or local value
     * followed by a number
       => it's a constructor argument

     * starts with double $$
       => $$try
       => main name of an unpacked pattern

#]


constructorArgumentName as Int: JA.Name =
    i:
    "$" .. Text.fromNumber i



#
#
#


accessArrayIndex as Int: JA.Expr: JA.Expr =
    index:

    index
    >> Text.fromNumber
    >> JA.Literal
    >> JA.AccessWithBrackets


literalString as Text: JA.Expr =
    str:
    escaped =
        str
            >> Text.replace "\n" "\\n"
            >> Text.replace "\"" "\\\""

    "\"" .. escaped .. "\""
    >> JA.Literal



#
#
#
union TranslatedExpression =
    , Block [JA.Statement]
    , Inline JA.Expr


translateExpressionToExpression as Env: EA.Expression: JA.Expr =
    env: expr:
    try translateExpression env expr as
        Inline e:
            e

        Block block:
            JA.Call (JA.BlockLambda [] block) []


translateExpression as Env: EA.Expression: TranslatedExpression =
    env: eaExpression:

    try eaExpression as
        EA.Call (EA.Call (EA.Variable name attrPath) argAndMut1) argAndMut2:
            translateVariable env name attrPath [argAndMut1, argAndMut2]
            >> Inline

        EA.Call (EA.Variable name attrPath) argAndMut:
            translateVariable env name attrPath [argAndMut]
            >> Inline

        EA.Variable name attrPath:
            translateVariable env name attrPath []
            >> Inline

        EA.Call ref arg:
            JA.Call
                (translateExpressionToExpression env ref)
                [ translateArg { nativeBinop = False } env arg ]
            >> Inline

        EA.LetIn { maybeName, mutability, letExpression, inExpression }:
            localEnv =
                try maybeName & mutability as
                    Just name & EA.Mutable:
                        { env with mutables = Set.insert name .mutables }
                    _:
                        env

            inStatements =
                try translateExpression localEnv inExpression as
                    Block stats: stats
                    Inline jaExpression: [JA.Return jaExpression]

            try maybeName as
                Nothing:
                    try translateExpression env letExpression as
                        Inline expr:
                            Block << JA.Eval expr :: inStatements

                        Block stats:
                            Block << List.concat [stats, inStatements]

                Just name:
                    letStatement =
                        letExpression
                        >> translateExpressionToExpression env
                        >> maybeCloneMutable mutability
                        >> wrapMutable mutability
                        >> JA.Define name

                    Block << letStatement :: inStatements


        EA.Lambda (maybeName & mutability) body:

            args =
                try maybeName as
                    Just name: [ name ]
                    Nothing: []

            # TODO these two defs are the same as for the LetIn, would be nice to have them in a function
            localEnv =
                try maybeName & mutability as
                    Just name & EA.Mutable:
                        { env with mutables = Set.insert name .mutables }
                    _:
                        env

            statements as [JA.Statement] =
                try translateExpression localEnv body as
                    Inline expr: [JA.Return expr]
                    Block block: block

            Inline << JA.BlockLambda args statements


        EA.LiteralText string:
            literalString string
            >> Inline

        EA.LiteralNumber num:
            Text.fromNumber num
            >> JA.Literal
            >> Inline

        EA.Conditional test true false:
            JA.Conditional
                (translateExpressionToExpression env test)
                (translateExpressionToExpression env true)
                (translateExpressionToExpression env false)
            >> Inline

        EA.And eaTests:
            jaTests =
                List.map (translateExpressionToExpression env) eaTests

            try List.reverse jaTests as
                []:
                    JA.Literal "true"
                    >> Inline

                head :: tail:
                    head
                    >> List.for tail (test: expr: JA.Binop "&&" test expr)
                    >> Inline

        EA.ShallowEqual a b:
            JA.Binop "==="
                (translateExpressionToExpression env a)
                (translateExpressionToExpression env b)
            >> Inline

        EA.LiteralArray items:
            items
            >> List.map (translateExpressionToExpression env)
            >> JA.Array
            >> Inline

        EA.ArrayAccess index array:
            array
            >> translateExpressionToExpression env
            >> accessArrayIndex index
            >> Inline

        EA.Constructor name:
            translateVariable env name [] []
            >> Inline

        EA.ConstructorAccess argIndex value:
            accessArrayIndex (argIndex + 1) (translateExpressionToExpression env value)
            >> Inline

        EA.IsConstructor name eaValue:
            jaValue =
                translateExpressionToExpression env eaValue

            # TODO use CoreTypes.true and CoreTypes.false not to hardcode the names?
            jaExpr as JA.Expr =
                try name as

                    "True":
                        jaValue

                    "False":
                        JA.Unop "!" jaValue

                    _:
                        JA.Binop "===" (accessArrayIndex 0 jaValue) (literalString name)

            Inline jaExpr

        EA.LiteralRecord maybeExtend attrNamesAndValues:
            obj =
                Dict.empty
                >> List.for attrNamesAndValues (name & value):
                    Dict.insert name (translateExpressionToExpression env value)
                >> JA.Record

            try maybeExtend as
                Nothing:
                    Inline obj

                Just extend:
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateExpressionToExpression env extend
                        , obj
                        ]
                        >> Inline

        EA.RecordAccess attrName value:
            JA.AccessWithDot attrName (translateExpressionToExpression env value)
            >> Inline

        EA.MissingPattern pos value:
            human =
                Error.posToHuman env.errorEnv pos

            [ JA.Literal "'Missing pattern in try..as'"
            , JA.Literal ("'" .. human.location .. "'")
            , JA.Call (JA.Literal "sp_toHuman") [ translateExpressionToExpression env value ]
            # JA.Call (JA.Literal "console.log") [ JA.Call (JA.Literal "JSON.stringify") [ JA.Var tryName ]]
            ]
            >> JA.Call (JA.Literal "sp_throw")
            >> Inline


translateConstructor as Meta.UniqueSymbolReference & CA.Constructor: JA.Statement =
    (usr & caCons):

    Meta.USR umr slug =
        usr

    usrAsText =
        Compiler/MakeEmittable.translateUsr usr


    argNames as [Text] =
        caCons.args
        >> List.indexedMap index: name: constructorArgumentName (index + 1)

    # `(($1) => ($2) => ($3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString slug

    arrayTail =
        List.map JA.Var argNames

    array =
        JA.Array (arrayHead :: arrayTail)

    array
    >> List.forReversed argNames (argName: expr: (JA.SimpleLambda [ argName ] expr))
    >> JA.Define usrAsText


translateDef as Env: EA.GlobalDefinition: Maybe JA.Statement =
    env: def:

    try Dict.get def.name env.overrides as
        Just _:
            Nothing

        Nothing:
            JA.Define def.name (translateExpressionToExpression env def.expr)
            >> Just


alias TranslateAllPars = {
    , errorEnv as Error.Env
    , caConstructors as [Meta.UniqueSymbolReference & CA.Constructor]
    , eaDefs as [EA.GlobalDefinition]
    , platformOverrides as [Meta.UniqueSymbolReference & Text]
    }

translateAll as TranslateAllPars : [JA.Statement] =
    pars:

    { errorEnv, caConstructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [JA.Statement]=
        List.map translateConstructor caConstructors

    env as Env = {
      , mutables = Set.empty
      , errorEnv
      , overrides = coreOverrides >> List.for platformOverrides (usr & runtimeName):
          Dict.insert (Compiler/MakeEmittable.translateUsr usr) (function runtimeName)
      }

    jaStatements as [JA.Statement] =
        List.filterMap (translateDef env) eaDefs

    List.concat [ jaConstructors, jaStatements ]

