
#
# TODO remove references to Compiler/MakeEmittable
#

alias Env =
    {
    , errorEnv as Error.Env
    , overrides as Dict EA.Name Override
    }


recycleTempVariable =
    JA.Var "__re__"


union Override = Override
    {
    , call as Env: [EA.Argument]: JA.Expr
    , value as Env: JA.Expr
    }


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
    , Prelude.mutableAssign.usr & binop "="
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
    , corelib "Basics" "cloneImm" & function "basics_cloneImm"
    , corelib "Basics" "cloneUni" & function "basics_cloneUni"
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
    , corelib "Hash" "fromList" & function  "hash_fromList"
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
    {
    , value = env: todo "unaryPlus has no raw value"
    , call =
        env: arguments:
        try arguments as
            [ EA.ArgumentSpend arg ]:
                # Num.unaryPlus n == n
                translateExpressionToExpression env arg

            _:
                todo "compiler bug: wrong number of arguments for unop"
    }
    >> Override


unaryMinus as Override =
    {
    , value = env: todo "unaryMinus has no raw value"
    , call =
        env: arguments:
        try arguments as
            [ EA.ArgumentSpend arg ]:
                # Num.unaryMinus n == -n
                JA.Unop "-" (translateExpressionToExpression env arg)

            _:
                todo "compiler bug: wrong number of arguments for unop"
    }
    >> Override


binop as Text: Override =
    jsOp:
    {
    , value = env: todo << "binop " .. jsOp .. " has no raw value"
    , call =
        env: arguments:
        try arguments as
            [ right, left ]:
                JA.Binop jsOp
                    (translateArg { nativeBinop = True } env right)
                    (translateArg { nativeBinop = True } env left)

            _:
                todo << "compiler bug: wrong number of arguments for binop" .. toHuman { jsOp, arguments }
    }
    >> Override


constructor as Text: Override =
    jsValue:
    {
    , value = env: JA.Var jsValue
    , call = env: args: makeCall env (JA.Var jsValue) args
    }
    >> Override



function as Text: Override =
    jaName:
    {
    , value = env: JA.Var jaName
    , call = env: args: makeCall env (JA.Var jaName) args
    }
    >> Override



translateVariable as Env: Name: JA.Expr =
    env: variableName:

    try Dict.get variableName env.overrides as
        Just (Override { call, value }):
            value env

        Nothing:
            JA.Var variableName


accessAttrs as [Text]: JA.Expr: JA.Expr =
    attrPath: e:
    List.for attrPath JA.AccessWithDot e


translateArg as { nativeBinop as Bool }: Env: EA.Argument: JA.Expr =
    stuff: env: eaExpression:

    try eaExpression as
        EA.ArgumentSpend e:
            translateExpressionToExpression env e

        EA.ArgumentRecycle attrPath name:
            accessAttrs attrPath (JA.Var name)


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


makeCall as Env: JA.Expr: [EA.Argument]: JA.Expr =
    env: jaRef: args:

    call =
        args
        >> List.map (translateArg { nativeBinop = False } env)
        >> JA.Call jaRef

    asRecycled as EA.Argument: Maybe JA.Expr =
        arg:
        try arg as
            EA.ArgumentSpend _: Nothing
            EA.ArgumentRecycle attrPath name:
                translateVariable env name
                >> accessAttrs attrPath
                >> Just

    recycledArgs =
        List.filterMap asRecycled args

    if recycledArgs == [] then
        #
        # ref(arg1, arg2, ...)
        #
        call
    else
        #
        # (t = ref(arg1, re1, arg3, re2, ....), re1 = t[1], re2 = t[2], t[0]);
        #
        [
        # t = ref(arg1, re0, arg3, re1, ....)
        , [ JA.Binop "=" recycleTempVariable call ]

        # re1 = t[1], re2 = t[2]
        , recycledArgs >> List.indexedMap index: arg:
            bracketIndex =
                index + 1
                >> Text.fromNumber
                >> JA.Literal
            JA.Binop "=" arg (JA.AccessWithBrackets bracketIndex recycleTempVariable)

        # t[2]
        , [ JA.AccessWithBrackets (JA.Literal "0") recycleTempVariable ]
        ]
        >> List.concat
        >> JA.Comma



translateExpression as Env: EA.Expression: TranslatedExpression =
    env: eaExpression:

    try eaExpression as
        EA.Variable name:
            translateVariable env name
            >> Inline

        EA.Call ref args:
            maybeNativeOverride =
                try ref as
                    EA.Variable name: Dict.get name env.overrides
                    _: Nothing

            try maybeNativeOverride as
                Just (Override { call, value = _ }):
                    call env args
                    >> Inline

                Nothing:
                    makeCall env (translateExpressionToExpression env ref) args
                    >> Inline

        EA.Fn eaArgs body:

            argsWithNames as [Bool & Name] =
                eaArgs >> List.indexedMap index: (re & maybeName):
                    try maybeName as
                        Just name: re & name
                        Nothing: re & "_" .. Text.fromNumber index

            recycledPars as [JA.Expr] =
                argsWithNames
                >> List.filter Tuple.first
                >> List.map ((_ & name): JA.Var name)

            statementsRaw as [JA.Statement] =
                try translateExpression env body as
                    Inline expr: [JA.Return expr]
                    Block block: block

            #
            # Per EA.Call above, recycling functions must return also the new values for the recycled variables
            #
            statementsFinal =
                if recycledPars == [] then
                    statementsRaw
                else
                    # Replace all `return x` statements with `return [x, ...recycledPars]`
                    addRecycled =
                        stat:
                        try stat as
                            JA.Return e: JA.Return (JA.Array (e :: recycledPars))
                            _: stat

                    List.map addRecycled statementsRaw

            Inline << JA.BlockLambda (List.map Tuple.second argsWithNames) statementsFinal


        EA.LetIn { maybeName, letExpression, inExpression, type }:

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
                        >> JA.Define (type.uni == Uni) name

                    Block << letStatement :: inStatements


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
            translateVariable env name
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



translateConstructor as Compiler/MakeEmittable.State@: USR & TA.FullType: JA.Statement =
    emState@: (usr & full):

    taType =
        full.raw

    USR _ slug =
        usr

    # `(($1, $2, $3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString slug

    definitionBody =
        try taType as
            TA.TypeFn pars out:

                argNames as [Text] =
                    pars >> List.indexedMap index: name: constructorArgumentName (index + 1)

                arrayHead :: List.map JA.Var argNames
                >> JA.Array
                >> JA.SimpleLambda argNames

            _:
                JA.Array [ arrayHead ]

    usrAsText =
        Compiler/MakeEmittable.translateUsr @emState usr

    JA.Define False usrAsText definitionBody


translateDef as Env: EA.GlobalDefinition: Maybe JA.Statement =
    env: def:

    try Dict.get def.name env.overrides as
        Just _:
            Nothing

        Nothing:
            JA.Define False def.name (translateExpressionToExpression env def.expr)
            >> Just


alias TranslateAllPars =
    {
    , errorEnv as Error.Env
    , constructors as [USR & TA.FullType]
    , eaDefs as [EA.GlobalDefinition]
    , platformOverrides as [USR & Text]
    }

translateAll as Compiler/MakeEmittable.State@: TranslateAllPars: [JA.Statement] =
    emState@: pars:

    { errorEnv, constructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [JA.Statement] =
        List.map (translateConstructor @emState) constructors

    env as Env =
      {
      , errorEnv
      , overrides = coreOverrides @emState >> List.for platformOverrides (usr & runtimeName):
          Dict.insert (Compiler/MakeEmittable.translateUsr @emState usr) (function runtimeName)
      }

    jaStatements as [JA.Statement] =
        List.filterMap (translateDef env) eaDefs

    List.concat [ jaConstructors, jaStatements ]

