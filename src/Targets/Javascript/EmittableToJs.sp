
#
# TODO remove references to Compiler/MakeEmittable
#

alias Env = {
    , errorEnv as Error.Env
    , overrides as Dict EA.Name Override
    }


#alias Override =
#    Env: [EA.Expression & EA.Mutability]: JA.Expr

union Override = Override (Env: [EA.Expression & Bool]: JA.Expr)


coreOverrides as Compiler/MakeEmittable.State@: Dict EA.Name Override =
    emState@:

    corelib as Text: Text: USR =
        m: n:
        USR (UMR Meta.Core m) n

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
    >> Dict.mapKeys (Compiler/MakeEmittable.translateUsr @emState)


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


translateVariable as Env: Name: [Name]: [EA.Expression & Bool]: JA.Expr =
    env: valueName: attrPath: eaArgs:


    try Dict.get valueName env.overrides as
        Just (Override override):
            override env eaArgs
            >> accessAttrs attrPath

        Nothing:
                JA.Var valueName
                >> accessAttrs attrPath
                >> wrapCalls env eaArgs


wrapCalls as Env: [EA.Expression & Bool]: JA.Expr: JA.Expr =
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


translateArg as { nativeBinop as Bool }: Env: (EA.Expression & Bool): JA.Expr =
    stuff: env: (eaExpression & isMutable):
    try isMutable as
        False:
            translateExpressionToExpression env eaExpression

        True:
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




wrapMutable as Bool: JA.Expr: JA.Expr =
    isMutable: expr:

    try isMutable as
        True:
            Dict.empty
            >> Dict.insert "attr" (literalString "$")
            >> Dict.insert "obj" (JA.Record << Dict.singleton "$" expr)
            >> JA.Record

        False:
            expr


maybeCloneMutable as Bool: JA.Expr: JA.Expr =
    isMutable: expr:

    try isMutable as
        False:
            expr

        True:
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
#        EA.Call (EA.Call (EA.Variable name attrPath) argAndMut1) argAndMut2:
#            translateVariable env name attrPath [argAndMut1, argAndMut2]
#            >> Inline
#
#        EA.Call (EA.Variable name attrPath) argAndMut:
#            translateVariable env name attrPath [argAndMut]
#            >> Inline

        EA.Variable name attrPath:
            translateVariable env name attrPath []
            >> Inline

        EA.Call ref args:
            JA.Call
                (translateExpressionToExpression env ref)
                (List.map (translateArg { nativeBinop = False } env) args)
            >> Inline

        EA.LetIn { maybeName, letExpression, inExpression }:
            inStatements =
                try translateExpression env inExpression as
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
                        >> JA.Define name

                    Block << letStatement :: inStatements


        EA.Fn eaArgs body:

            args =
                eaArgs >> List.indexedMap index: maybeName:
                    try maybeName as
                        Just name: name
                        Nothing: "_" .. Text.fromNumber index

            statements as [JA.Statement] =
                try translateExpression env body as
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


translateConstructor as Compiler/MakeEmittable.State@: USR & TA.Type: JA.Statement =
    emState@: (usr & taType):

    USR _ slug =
        usr

    # `(($1) => ($2) => ($3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString slug

    definitionBody =
        try taType as
            TA.TypeFn _ pars out:

                argNames as [Text] =
                    pars >> List.indexedMap index: name: constructorArgumentName (index + 1)

                arrayHead :: List.map JA.Var argNames
                >> JA.Array
                >> JA.SimpleLambda argNames

            _:
                JA.Array [ arrayHead ]

    usrAsText =
        Compiler/MakeEmittable.translateUsr @emState usr

    JA.Define usrAsText definitionBody


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
    , caConstructors as [USR & TA.Type]
    , eaDefs as [EA.GlobalDefinition]
    , platformOverrides as [USR & Text]
    }

translateAll as Compiler/MakeEmittable.State@: TranslateAllPars: [JA.Statement] =
    emState@: pars:

    { errorEnv, caConstructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [JA.Statement]=
        List.map (translateConstructor @emState) caConstructors

    env as Env = {
      , errorEnv
      , overrides = coreOverrides @emState >> List.for platformOverrides (usr & runtimeName):
          Dict.insert (Compiler/MakeEmittable.translateUsr @emState usr) (function runtimeName)
      }

    jaStatements as [JA.Statement] =
        List.filterMap (translateDef env) eaDefs

    List.concat [ jaConstructors, jaStatements ]

