
#
# TODO remove references to Compiler/MakeEmittable
#

alias Env =
    {
    , overrides as Dict USR Override
    }


recycleTempVariable =
    JA.Var "__re__"


union Override = Override
    {
    , call as fn Env, [EA.Argument]: JA.Expr
    , value as fn Env: JA.Expr
    }


coreOverrides as fn None: Dict USR Override =
    fn None:

    corelib as fn Text, Text: USR =
        fn m, n:
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
    , todo "if type is whatever, then override???"
#    , CoreTypes.true & constructor "true"
#    , CoreTypes.false & constructor "false"
#    , CoreTypes.noneValue & constructor "null"
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
    , corelib "Array" "each" & function  "array_each"
    , corelib "Array" "push" & function  "array_push"
    , corelib "Array" "pop" & function  "array_pop"
    , corelib "Array" "get" & function  "array_get"
    , corelib "Array" "set" & function  "array_set"
    , corelib "Array" "sortBy" & function  "array_sortBy"
    , corelib "Array" "fromList" & function  "array_fromList"
    , corelib "Array" "toList" & function  "array_toList"
    #
    , corelib "List" "sortBy" & function "list_sortBy"
    #
    , corelib "Self" "load" & loadOverride
    , corelib "Self" "introspect" & introspectOverride
    , corelib "Self" "internalRepresentation" & function "JSON.stringify"
    ]
    >> Dict.fromList


unaryPlus as Override =
    {
    , value = fn env: todo "unaryPlus has no raw value"
    , call =
        fn env, arguments:
        try arguments as
            , [ EA.ArgumentSpend fullType arg ]:
                # Num.unaryPlus n == n
                translateExpressionToExpression env arg

            , _:
                todo "compiler bug: wrong number of arguments for unop"
    }
    >> Override


unaryMinus as Override =
    {
    , value = fn env: todo "unaryMinus has no raw value"
    , call =
        fn env, arguments:
        try arguments as
            , [ EA.ArgumentSpend fullType arg ]:
                # Num.unaryMinus n == -n
                JA.Unop "-" (translateExpressionToExpression env arg)

            , _:
                todo "compiler bug: wrong number of arguments for unop"
    }
    >> Override


binop as fn Text: Override =
    fn jsOp:
    {
    , value = fn env: todo << "binop " .. jsOp .. " has no raw value"
    , call =
        fn env, arguments:
        try arguments as
            , [ right, left ]:
                JA.Binop jsOp
                    (translateArg { nativeBinop = True } env right)
                    (translateArg { nativeBinop = True } env left)

            , _:
                todo << "compiler bug: wrong number of arguments for binop" .. toHuman { jsOp, arguments }
    }
    >> Override


constructor as fn Text: Override =
    fn jsValue:
    {
    , value = fn env: JA.Var jsValue
    , call = fn env, args: makeCall env (JA.Var jsValue) args
    }
    >> Override



function as fn Text: Override =
    fn jaName:
    {
    , value = fn env: JA.Var jaName
    , call = fn env, args: makeCall env (JA.Var jaName) args
    }
    >> Override




#
# Dynamic loading
#
introspectOverride as Override =

    call =
        fn env, eaArgs:

        try eaArgs as
            , [ EA.ArgumentSpend { with raw } e ]:

                expression as JA.Expr =
                    e
                    >> Self.internalRepresentation
                    >> JA.Literal

                type as JA.Expr =
                    raw
                    >> Self.internalRepresentation
                    >> JA.Literal

                nonFn as JA.Expr =
                    JA.Array [] # TODO!!!

                value as JA.Expr =
                    translateExpressionToExpression env e

                [
                , "expression" & expression
                , "raw" & type
                , "nonFn" & nonFn
                , "value" & value
                ]
                >> Dict.fromList
                >> JA.Record

            , _:
                todo "introspectOverride BUG?!"


    {
    , value = fn env: todo "TODO: monomorphization is not yet implemented so `introspect` can only be called directly"
    , call
    }
    >> Override


loadOverride as Override =

    call =
        fn env, eaArgs:

        jaArgs =
            List.map (translateArg { nativeBinop = False } env __) eaArgs

        requestedTypeHumanized as JA.Expr =
            try eaArgs as
                , [ compiledProgram, EA.ArgumentSpend { with raw = TA.TypeFn [TA.ParSp { with raw = compiledType}] _ } _ ]:
                    !hash = Hash.fromList []
                    compiledType
                    >> TA.normalizeType @hash __
                    # TODO: once we have proper encoders, we can use those
                    >> toHuman
                    >> literalString

                , _:
                    todo "loadOverride BUG?!"

        JA.Call (JA.Var "self_load") [ requestedTypeHumanized, ...jaArgs ]

    {
    , value = fn env: todo "TODO: load as value... I guess we need monomorphization?"
    , call
    }
    >> Override


#
# Translation
#
maybeOverrideUsr as fn Env, USR: JA.Expr =
    fn env, usr:

    try Dict.get usr env.overrides as
        , Just (Override { call, value }):
            value env

        , Nothing:
            JA.Var (translateUsr usr)


accessAttrs as fn [Text], JA.Expr: JA.Expr =
    fn attrPath, e:
    List.for e attrPath JA.AccessWithDot


translateArg as fn { nativeBinop as Bool }, Env, EA.Argument: JA.Expr =
    fn stuff, env, eaExpression:

    try eaExpression as
        , EA.ArgumentSpend fullType e:
            translateExpressionToExpression env e

        , EA.ArgumentRecycle rawType attrPath name:
            accessAttrs attrPath (translateName name >> JA.Var)


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


constructorArgumentName as fn Int: JA.Name =
    fn i:
    "$" .. Text.fromNumber i



#
#
#


accessArrayIndex as fn Int, JA.Expr: JA.Expr =
    fn index, j:

    index
    >> Text.fromNumber
    >> JA.Literal
    >> JA.AccessWithBrackets __ j


literalString as fn Text: JA.Expr =
    fn str:
    escaped =
        str
            >> Text.replace "\n" "\\n" __
            >> Text.replace "\"" "\\\"" __

    "\"" .. escaped .. "\""
    >> JA.Literal



#
#
#
union TranslatedExpression =
    , Block [JA.Statement]
    , Inline JA.Expr


translateExpressionToExpression as fn Env, EA.Expression: JA.Expr =
    fn env, expr:
    try translateExpression env expr as
        , Inline e:
            e

        , Block block:
            JA.Call (JA.BlockLambda [] block) []


makeCall as fn Env, JA.Expr, [EA.Argument]: JA.Expr =
    fn env, jaRef, args:

    call =
        args
        >> List.map (translateArg { nativeBinop = False } env __) __
        >> JA.Call jaRef __

    asRecycled as fn EA.Argument: Maybe JA.Expr =
        fn arg:
        try arg as
            , EA.ArgumentSpend _ _: Nothing
            , EA.ArgumentRecycle rawType attrPath name:
                name
                >> translateName
                >> JA.Var
                >> accessAttrs attrPath __
                >> Just

    recycledArgs =
        List.filterMap asRecycled args

    if recycledArgs == [] then
        #
        # ref(arg1, arg2, ...)
        #
        call
    else
        zzz =
            fn index, arg:

            bracketIndex =
                index + 1
                >> Text.fromNumber
                >> JA.Literal

            JA.Binop "=" arg (JA.AccessWithBrackets bracketIndex recycleTempVariable)


        #
        # (t = ref(arg1, re1, arg3, re2, ....), re1 = t[1], re2 = t[2], t[0]);
        #
        [
        # t = ref(arg1, re0, arg3, re1, ....)
        , [ JA.Binop "=" recycleTempVariable call ]

        # re1 = t[1], re2 = t[2]
        , recycledArgs >> List.indexedMap zzz __

        # t[2]
        , [ JA.AccessWithBrackets (JA.Literal "0") recycleTempVariable ]
        ]
        >> List.concat
        >> JA.Comma



translateExpression as fn Env, EA.Expression: TranslatedExpression =
    fn env, eaExpression:

    try eaExpression as
        , EA.Variable (RefLocal name):
            name
            >> translateName
            >> JA.Var
            >> Inline

        , EA.Variable (RefGlobal usr):
            maybeOverrideUsr env usr
            >> Inline

        , EA.Call ref args:
            maybeNativeOverride =
                try ref as
                    , EA.Variable (RefGlobal usr): Dict.get usr env.overrides
                    , _: Nothing

            try maybeNativeOverride as
                , Just (Override { call, value = _ }):
                    call env args
                    >> Inline

                , Nothing:
                    makeCall env (translateExpressionToExpression env ref) args
                    >> Inline

        , EA.Fn eaArgs body:

            argsWithNames as [Bool & Text] =
                zzz =
                    fn index, (re & maybeName):
                    try maybeName as
                        , Just name: re & translateName name
                        , Nothing: re & "_" .. Text.fromNumber index

                List.indexedMap zzz eaArgs

            recycledPars as [JA.Expr] =
                argsWithNames
                >> List.filter Tuple.first __
                >> List.map (fn (_ & name): JA.Var name) __

            statementsRaw as [JA.Statement] =
                try translateExpression env body as
                    , Inline expr: [JA.Return expr]
                    , Block block: block

            #
            # Per EA.Call above, recycling functions must return also the new values for the recycled variables
            #
            statementsFinal =
                if recycledPars == [] then
                    statementsRaw
                else
                    # Replace all `return x` statements with `return [x, ...recycledPars]`
                    addRecycled =
                        fn stat:
                        try stat as
                            , JA.Return e: JA.Return (JA.Array (e :: recycledPars))
                            , _: stat

                    List.map addRecycled statementsRaw

            Inline << JA.BlockLambda (List.map Tuple.second argsWithNames) statementsFinal


        , EA.LetIn { maybeName, letExpression, inExpression, type }:

            inStatements =
                try translateExpression env inExpression as
                    , Block stats: stats
                    , Inline jaExpression: [JA.Return jaExpression]

            try maybeName as
                , Nothing:
                    try translateExpression env letExpression as
                        , Inline expr:
                            Block << JA.Eval expr :: inStatements

                        , Block stats:
                            Block << List.concat [stats, inStatements]

                , Just name:
                    letStatement =
                        letExpression
                        >> translateExpressionToExpression env __
                        >> JA.Define (type.uni == Uni) (translateName name) __

                    Block << letStatement :: inStatements


        , EA.LiteralText string:
            literalString string
            >> Inline

        , EA.LiteralNumber num:
            Text.fromNumber num
            >> JA.Literal
            >> Inline

        , EA.Conditional test true false:
            JA.Conditional
                (translateExpressionToExpression env test)
                (translateExpressionToExpression env true)
                (translateExpressionToExpression env false)
            >> Inline

        , EA.And eaTests:
            jaTests =
                List.map (translateExpressionToExpression env __) eaTests

            try List.reverse jaTests as
                , []:
                    JA.Literal "true"
                    >> Inline

                , head :: tail:
                    head
                    >> List.for __ tail (fn test, expr: JA.Binop "&&" test expr)
                    >> Inline

        , EA.ShallowEqual a b:
            JA.Binop "==="
                (translateExpressionToExpression env a)
                (translateExpressionToExpression env b)
            >> Inline

        , EA.LiteralArray items:
            items
            >> List.map (translateExpressionToExpression env __) __
            >> JA.Array
            >> Inline

        , EA.ArrayAccess index array:
            array
            >> translateExpressionToExpression env __
            >> accessArrayIndex index __
            >> Inline

        , EA.Constructor name:
            todo "maybeOverrideUsr env usr"
            >> Inline

        , EA.ConstructorAccess argIndex value:
            accessArrayIndex (argIndex + 1) (translateExpressionToExpression env value)
            >> Inline

        , EA.IsConstructor name eaValue:
            jaValue =
                translateExpressionToExpression env eaValue

            # TODO use CoreTypes.true and CoreTypes.false not to hardcode the names?
            jaExpr as JA.Expr =
                try name as

                    , "True":
                        jaValue

                    , "False":
                        JA.Unop "!" jaValue

                    , _:
                        JA.Binop "===" (accessArrayIndex 0 jaValue) (literalString name)

            Inline jaExpr

        , EA.LiteralRecord maybeExtend attrNamesAndValues:
            obj =
                Dict.empty
                >> List.for __ attrNamesAndValues fn (name & value), d:
                    Dict.insert name (translateExpressionToExpression env value) d
                >> JA.Record

            try maybeExtend as
                , Nothing:
                    Inline obj

                , Just extend:
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateExpressionToExpression env extend
                        , obj
                        ]
                        >> Inline

        , EA.RecordAccess attrName value:
            JA.AccessWithDot attrName (translateExpressionToExpression env value)
            >> Inline

        , EA.MissingPattern location value:
            [ JA.Literal "'Missing pattern in try..as'"
            , JA.Literal ("'" .. location .. "'")
            , JA.Call (JA.Literal "sp_toHuman") [ translateExpressionToExpression env value ]
            ]
            >> JA.Call (JA.Literal "sp_throw") __
            >> Inline



translateConstructor as fn USR & TA.FullType: JA.Statement =
    fn (usr & full):

    taType =
        full.raw

    USR _ slug =
        usr

    # `(($1, $2, $3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString slug

    definitionBody =
        try taType as
            , TA.TypeFn pars out:

                argNames as [Text] =
                    pars >> List.indexedMap (fn index, name: constructorArgumentName (index + 1)) __

                arrayHead :: List.map JA.Var argNames
                >> JA.Array
                >> JA.SimpleLambda argNames __

            , _:
                JA.Array [ arrayHead ]

    usrAsText =
        translateUsr usr

    JA.Define False usrAsText definitionBody


translateDef as fn Env, EA.GlobalDefinition: Maybe JA.Statement =
    fn env, def:

    try Dict.get def.usr env.overrides as
        , Just _:
            Nothing

        , Nothing:
            JA.Define False (translateUsr def.usr) (translateExpressionToExpression env def.expr)
            >> Just


translateSource as fn Meta.Source: Text =
    fn src:

    try src as
        , Meta.Core:
            "core"

        , Meta.Posix:
            "posix"

        , Meta.Browser:
            "browser"

        , Meta.SourceDirId id:
            id


translateUsr as fn USR: Text =
    fn (USR (UMR source modulePath) name):
    "$" .. translateSource source .. "$" .. Text.replace "/" "$" modulePath .. "$" .. name


translateName as fn Name: Text =
    fn n:
    "$" .. n


alias TranslateAllPars =
    {
    , constructors as [USR & TA.FullType]
    , eaDefs as [EA.GlobalDefinition]
    , platformOverrides as [USR & Text]
    }

translateAll as fn TranslateAllPars: [JA.Statement] =
    fn pars:

    { constructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [JA.Statement] =
        List.map (translateConstructor __) constructors

    env as Env =
      {
      , overrides = List.for (coreOverrides None) platformOverrides fn (usr & runtimeName), d:
          Dict.insert usr (function runtimeName) d
      }

    jaStatements as [JA.Statement] =
        List.filterMap (translateDef env __) eaDefs

    List.concat [ jaConstructors, jaStatements ]

