Env =
    {
    , overrides as Dict USR Override
    }


recycleTempVariable =
    JA.'var "__re__"


var Override =
    , 'override
          {
          , call as fn Env, [ EA.Argument ]: JA.Expr
          , value as fn Env: JA.Expr
          }


coreOverrides as fn None: Dict USR Override =
    fn 'none:
    corelib as fn Text, Text: USR =
        fn m, n:
        'USR ('UMR Meta.'core m) n

    [
    , Prelude.unaryPlus.usr & unaryPlus
    , Prelude.unaryMinus.usr & unaryMinus
    #
    , Prelude.add.usr
    & binop "+"
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
    , CoreTypes.trueUsr
    & constructor "true"
    , CoreTypes.falseUsr & constructor "false"
    , CoreTypes.noneConsUsr & constructor "null"
    #
    , Prelude.divide.usr
    & function "sp_divide"
    , Prelude.listCons.usr & function "sp_cons"
    , Prelude.equal.usr & function "sp_equal"
    , Prelude.notEqual.usr & function "sp_not_equal"
    , corelib "Basics" "modBy" & function "basics_modBy"
    , corelib "Basics" "round" & function "Math.round"
    , corelib "Basics" "cloneImm" & function "basics_cloneImm"
    , corelib "Basics" "cloneUni" & function "basics_cloneUni"
    #
    , Prelude.debugLog.usr
    & function "sp_log"
    , Prelude.debugTodo.usr & function "sp_todo"
    , Prelude.debugToHuman.usr & function "sp_toHuman"
    , Prelude.debugBenchStart.usr & function "sp_benchStart"
    , Prelude.debugBenchStop.usr & function "sp_benchStop"
    , Prelude.compare.usr & function "basics_compare"
    #
    , corelib "Text" "fromNumber"
    & function "text_fromNumber"
    , corelib "Text" "toLower" & function "text_toLower"
    , corelib "Text" "toUpper" & function "text_toUpper"
    , corelib "Text" "toNumber" & function "text_toNumber"
    , corelib "Text" "split" & function "text_split"
    , corelib "Text" "length" & function "text_length"
    , corelib "Text" "slice" & function "text_slice"
    , corelib "Text" "startsWith" & function "text_startsWith"
    , corelib "Text" "startsWithRegex" & function "text_startsWithRegex"
    , corelib "Text" "replaceRegex" & function "text_replaceRegex"
    , corelib "Text" "trimLeft" & function "text_trimLeft"
    , corelib "Text" "dropLeft" & function "text_dropLeft"
    , corelib "Text" "forEach" & function "text_forEach"
    #
    , corelib "Hash" "fromList"
    & function "hash_fromList"
    , corelib "Hash" "insert" & function "hash_insert"
    , corelib "Hash" "remove" & function "hash_remove"
    , corelib "Hash" "get" & function "hash_get"
    , corelib "Hash" "for" & function "hash_for"
    , corelib "Hash" "each" & function "hash_each"
    , corelib "Hash" "pop" & function "hash_pop"
    #
    , corelib "Array" "each"
    & function "array_each"
    , corelib "Array" "push" & function "array_push"
    , corelib "Array" "pop" & function "array_pop"
    , corelib "Array" "get" & function "array_get"
    , corelib "Array" "set" & function "array_set"
    , corelib "Array" "sortBy" & function "array_sortBy"
    , corelib "Array" "fromList" & function "array_fromList"
    , corelib "Array" "toList" & function "array_toList"
    #
    , corelib "List" "sortBy"
    & function "list_sortBy"
    #
    , corelib "Self" "load"
    & loadOverride
    , corelib "Self" "introspect" & introspectOverride
    , corelib "Self" "internalRepresentation" & function "JSON.stringify"
    ]
    >> Dict.fromList


unaryPlus as Override =
    {
    , call =
        fn env, arguments:
        try arguments as

            [ EA.'argumentSpend fullType arg ]:
                # Num.unaryPlus n == n
                translateExpressionToExpression env arg

            _:
                todo "compiler bug: wrong number of arguments for unop"
    , value = fn env: todo "unaryPlus has no raw value"
    }
    >> 'override


unaryMinus as Override =
    {
    , call =
        fn env, arguments:
        try arguments as

            [ EA.'argumentSpend fullType arg ]:
                # Num.unaryMinus n == -n
                JA.'unop "-" (translateExpressionToExpression env arg)

            _:
                todo "compiler bug: wrong number of arguments for unop"
    , value = fn env: todo "unaryMinus has no raw value"
    }
    >> 'override


binop as fn Text: Override =
    fn jsOp:
    {
    , call =
        fn env, arguments:
        try arguments as
            [ right, left ]: JA.'binop jsOp (translateArg { nativeBinop = 'true } env right) (translateArg { nativeBinop = 'true } env left)
            _: todo << "compiler bug: wrong number of arguments for binop" .. toHuman { arguments, jsOp }
    , value = fn env: todo << "binop " .. jsOp .. " has no raw value"
    }
    >> 'override


constructor as fn Text: Override =
    fn jsValue:
    {
    , call = fn env, args: makeCall env (JA.'var jsValue) args
    , value = fn env: JA.'var jsValue
    }
    >> 'override


function as fn Text: Override =
    fn jaName:
    {
    , call = fn env, args: makeCall env (JA.'var jaName) args
    , value = fn env: JA.'var jaName
    }
    >> 'override


#
# Dynamic loading
#
introspectOverride as Override =
    call =
        fn env, eaArgs:
        try eaArgs as

            [ EA.'argumentSpend { with  raw } e ]:
                expression as JA.Expr =
                    e
                    >> Self.internalRepresentation
                    >> JA.'literal

                type as JA.Expr =
                    raw
                    >> Self.internalRepresentation
                    >> JA.'literal

                nonFn as JA.Expr =
                    # TODO!!!
                    JA.'array []

                value as JA.Expr =
                    translateExpressionToExpression env e

                [
                , "expression" & expression
                , "raw" & type
                , "nonFn" & nonFn
                , "value" & value
                ]
                >> Dict.fromList
                >> JA.'record

            _:
                todo "introspectOverride BUG?!"

    {
    , call
    , value = fn env: todo "TODO: monomorphization is not yet implemented so `introspect` can only be called directly"
    }
    >> 'override


loadOverride as Override =
    call =
        fn env, eaArgs:
        jaArgs =
            List.map (translateArg { nativeBinop = 'false } env __) eaArgs

        requestedTypeHumanized as JA.Expr =
            try eaArgs as

                [ compiledProgram, EA.'argumentSpend { with  raw = TA.'typeFn [ TA.'parSp { with  raw = compiledType } ] _ } _ ]:
                    !hash =
                        Hash.fromList []

                    compiledType
                    >> TA.normalizeType @hash __
                    # TODO: once we have proper encoders, we can use those
                    >> toHuman
                    >> literalString

                _:
                    todo "loadOverride BUG?!"

        JA.'call (JA.'var "self_load") [ requestedTypeHumanized, jaArgs... ]

    {
    , call
    , value = fn env: todo "TODO: load as value... I guess we need monomorphization?"
    }
    >> 'override


#
# Translation
#
translateConstructorName as fn Text: Text =
    fn x:
    head =
        Text.slice 1 2 x

    rest =
        Text.slice 2 9999 x

    Text.toUpper head .. rest


translateConstructorUsr as fn USR: Text =
    fn 'USR umr raw:
    raw
    >> translateConstructorName
    >> 'USR umr __
    >> translateUsr


maybeOverrideUsr as fn Env, USR: JA.Expr =
    fn env, usr:
    try Dict.get usr env.overrides as
        'just ('override { call, value }): value env
        'nothing: JA.'var (translateUsr usr)


maybeOverrideUsrForConstructor as fn Env, USR: JA.Expr =
    fn env, usr:
    try Dict.get usr env.overrides as

        'just ('override { call, value }):
            value env

        'nothing:
            usr
            >> translateConstructorUsr
            >> JA.'var


accessAttrs as fn [ Text ], JA.Expr: JA.Expr =
    fn attrPath, e:
    List.for e attrPath JA.'accessWithDot


translateArg as fn { nativeBinop as Bool }, Env, EA.Argument: JA.Expr =
    fn stuff, env, eaExpression:
    try eaExpression as
        EA.'argumentSpend fullType e: translateExpressionToExpression env e
        EA.'argumentRecycle rawType attrPath name: accessAttrs attrPath (translateName name >> JA.'var)


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
    >> JA.'literal
    >> JA.'accessWithBrackets __ j


literalString as fn Text: JA.Expr =
    fn str:
    escaped =
        str

#            >> Text.replace "\n" "\\n" __
#            >> Text.replace "\"" "\\\"" __

    "\"" .. escaped .. "\"" >> JA.'literal


#
#
#
var TranslatedExpression =
    , 'block [ JA.Statement ]
    , 'inline JA.Expr


translateExpressionToExpression as fn Env, EA.Expression: JA.Expr =
    fn env, expr:
    try translateExpression env expr as
        'inline e: e
        'block block: JA.'call (JA.'blockLambda [] block) []


makeCall as fn Env, JA.Expr, [ EA.Argument ]: JA.Expr =
    fn env, jaRef, args:
    call =
        args
        >> List.map (translateArg { nativeBinop = 'false } env __) __
        >> JA.'call jaRef __

    asRecycled as fn EA.Argument: Maybe JA.Expr =
        fn arg:
        try arg as

            EA.'argumentSpend _ _:
                'nothing

            EA.'argumentRecycle rawType attrPath name:
                name
                >> translateName
                >> JA.'var
                >> accessAttrs attrPath __
                >> 'just

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
                >> JA.'literal

            JA.'binop "=" arg (JA.'accessWithBrackets bracketIndex recycleTempVariable)

        #
        # (t = ref(arg1, re1, arg3, re2, ....), re1 = t[1], re2 = t[2], t[0]);
        #
        [
        # t = ref(arg1, re0, arg3, re1, ....)
        , [ JA.'binop "=" recycleTempVariable call ]
        # re1 = t[1], re2 = t[2]
        , recycledArgs
        >> List.indexedMap zzz __
        # t[2]
        , [ JA.'accessWithBrackets (JA.'literal "0") recycleTempVariable ]
        ]
        >> List.concat
        >> JA.'comma


translateExpression as fn Env, EA.Expression: TranslatedExpression =
    fn env, eaExpression:
    try eaExpression as

        EA.'variable ('refLocal name):
            name
            >> translateName
            >> JA.'var
            >> 'inline

        EA.'variable ('refPlaceholder n):
            n
            >> Text.fromNumber
            >> translateName
            >> JA.'var
            >> 'inline

        EA.'variable ('refGlobal usr):
            maybeOverrideUsr env usr >> 'inline

        EA.'call ref args:
            maybeNativeOverride =
                try ref as
                    EA.'variable ('refGlobal usr): Dict.get usr env.overrides
                    _: 'nothing

            try maybeNativeOverride as
                'just ('override { call, value = _ }): call env args >> 'inline
                'nothing: makeCall env (translateExpressionToExpression env ref) args >> 'inline

        EA.'fn eaArgs body:
            argsWithNames as [ Bool & Text ] =
                zzz =
                    fn index, re & maybeName:
                    try maybeName as
                        'just name: re & translateName name
                        'nothing: re & "_" .. Text.fromNumber index

                List.indexedMap zzz eaArgs

            recycledPars as [ JA.Expr ] =
                argsWithNames
                >> List.filter Tuple.first __
                >> List.map (fn _ & name: JA.'var name) __

            statementsRaw as [ JA.Statement ] =
                try translateExpression env body as
                    'inline expr: [ JA.'return expr ]
                    'block block: block

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
                            JA.'return e: JA.'return (JA.'array (e :: recycledPars))
                            _: stat

                    List.map addRecycled statementsRaw

            'inline << JA.'blockLambda (List.map Tuple.second argsWithNames) statementsFinal

        EA.'letIn { inExpression, letExpression, maybeName, type }:
            inStatements =
                try translateExpression env inExpression as
                    'block stats: stats
                    'inline jaExpression: [ JA.'return jaExpression ]

            try maybeName as

                'nothing:
                    try translateExpression env letExpression as
                        'inline expr: 'block << JA.'eval expr :: inStatements
                        'block stats: 'block << List.concat [ stats, inStatements ]

                'just name:
                    letStatement =
                        letExpression
                        >> translateExpressionToExpression env __
                        >> JA.'define (type.uni == 'uni) (translateName name) __

                    'block << letStatement :: inStatements

        EA.'literalText string:
            literalString string >> 'inline

        EA.'literalNumber num:
            Text.fromNumber num
            >> JA.'literal
            >> 'inline

        EA.'conditional test true false:
            JA.'conditional (translateExpressionToExpression env test) (translateExpressionToExpression env true) (translateExpressionToExpression env false) >> 'inline

        EA.'and eaTests:
            jaTests =
                List.map (translateExpressionToExpression env __) eaTests

            try List.reverse jaTests as

                []:
                    JA.'literal "true" >> 'inline

                head :: tail:
                    head
                    >> List.for __ tail (fn test, expr: JA.'binop "&&" test expr)
                    >> 'inline

        EA.'shallowEqual a b:
            JA.'binop "===" (translateExpressionToExpression env a) (translateExpressionToExpression env b) >> 'inline

        EA.'literalArray items:
            items
            >> List.map (translateExpressionToExpression env __) __
            >> JA.'array
            >> 'inline

        EA.'arrayAccess index array:
            array
            >> translateExpressionToExpression env __
            >> accessArrayIndex index __
            >> 'inline

        EA.'constructor usr:
            maybeOverrideUsrForConstructor env usr >> 'inline

        EA.'constructorAccess argIndex value:
            accessArrayIndex (argIndex + 1) (translateExpressionToExpression env value) >> 'inline

        EA.'isConstructor usr eaValue:
            jaValue =
                translateExpressionToExpression env eaValue

            if usr == CoreTypes.noneConsUsr then
                JA.'var "true" >> 'inline
            else if usr == CoreTypes.trueUsr then
                jaValue >> 'inline
            else if usr == CoreTypes.falseUsr then
                JA.'unop "!" jaValue >> 'inline
            else
                'USR _ name =
                    usr

                name
                >> translateConstructorName
                >> literalString
                >> JA.'binop "===" (accessArrayIndex 0 jaValue) __
                >> 'inline

        EA.'literalRecord maybeExtend attrNamesAndValues:
            obj =
                Dict.empty
                >> List.for __ attrNamesAndValues fn name & value, d:
                    Dict.insert name (translateExpressionToExpression env value) d
                >> JA.'record

            try maybeExtend as

                'nothing:
                    'inline obj

                'just extend:
                    JA.'call
                        (JA.'var "Object.assign")
                        [
                        , JA.'record Dict.empty
                        , translateExpressionToExpression env extend
                        , obj
                        ]
                    >> 'inline

        EA.'recordAccess attrName value:
            JA.'accessWithDot attrName (translateExpressionToExpression env value) >> 'inline

        EA.'missingPattern location value:
            [
            , JA.'literal "'Missing pattern in try..as'"
            , JA.'literal ("'" .. location .. "'")
            , JA.'call (JA.'literal "sp_toHuman") [ translateExpressionToExpression env value ]
            ]
            >> JA.'call (JA.'literal "sp_throw") __
            >> 'inline


translateConstructor as fn USR & TA.FullType: JA.Statement =
    fn usr & full:
    taType =
        full.raw

    'USR umr apoName =
        usr

    slug =
        translateConstructorName apoName

    # `(($1, $2, $3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString slug

    definitionBody =
        try taType as

            TA.'typeFn pars out:
                argNames as [ Text ] =
                    pars >> List.indexedMap (fn index, name: constructorArgumentName (index + 1)) __

                arrayHead :: List.map JA.'var argNames
                >> JA.'array
                >> JA.'simpleLambda argNames __

            _:
                JA.'array [ arrayHead ]

    usrAsText =
        translateUsr ('USR umr slug)

    JA.'define 'false usrAsText definitionBody


translateDef as fn Env, EA.GlobalDefinition: Maybe JA.Statement =
    fn env, def:
    try Dict.get def.usr env.overrides as
        'just _: 'nothing
        'nothing: JA.'define 'false (translateUsr def.usr) (translateExpressionToExpression env def.expr) >> 'just


translateSource as fn Meta.Source: Text =
    fn src:
    try src as
        Meta.'core: "core"
        Meta.'posix: "posix"
        Meta.'browser: "browser"
        Meta.'sourceDirId id: id


translateUsr as fn USR: Text =
    fn 'USR ('UMR source modulePath) name:
    "$" .. translateSource source .. "$" .. Text.replace "/" "$" modulePath .. "$" .. name


translateName as fn Name: Text =
    fn n:
    "$" .. n


TranslateAllPars =
    {
    , constructors as [ USR & TA.FullType ]
    , eaDefs as [ EA.GlobalDefinition ]
    , platformOverrides as [ USR & Text ]
    }


translateAll as fn TranslateAllPars: [ JA.Statement ] =
    fn pars:
    { constructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [ JA.Statement ] =
        List.map (translateConstructor __) constructors

    env as Env =
        {
        , overrides =
            List.for (coreOverrides 'none) platformOverrides fn usr & runtimeName, d:
                Dict.insert usr (function runtimeName) d
        }

    jaStatements as [ JA.Statement ] =
        List.filterMap (translateDef env __) eaDefs

    List.concat [ jaConstructors, jaStatements ]
