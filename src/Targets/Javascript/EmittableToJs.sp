Env =
    {
    , overrides as Dict EA.TranslatedUsr Override
    }


recycleTempVariable =
    JA.'var "__re__"


var Override =
    , 'override
          {
          , call as fn Env, [ EA.Argument ]: JA.Expr
          , value as fn Env: JA.Expr
          }


coreOverrides as fn None: Dict EA.TranslatedUsr Override =
    fn 'none:
    #
    corelib as fn Text, Text: USR =
        fn module, name:
        'USR (CoreDefs.makeUmr module) name

    [
    , CoreDefs.unaryPlus.usr & unaryPlus
    , CoreDefs.unaryMinus.usr & unaryMinus
    #
    , CoreDefs.add.usr
    & binop "+"
    , CoreDefs.multiply.usr & binop "*"
    , CoreDefs.subtract.usr & binop "-"
    , CoreDefs.mutableAssign.usr & binop "="
    , CoreDefs.mutableAdd.usr & binop "+="
    , CoreDefs.mutableSubtract.usr & binop "-="
    , CoreDefs.textConcat.usr & binop "+"
    , CoreDefs.greaterThan.usr & binop ">"
    , CoreDefs.lesserThan.usr & binop "<"
    , CoreDefs.greaterOrEqualThan.usr & binop ">="
    , CoreDefs.lesserOrEqualThan.usr & binop "<="
    , CoreDefs.or_.usr & binop "||"
    , CoreDefs.and_.usr & binop "&&"
    #
    , CoreDefs.trueUsr
    & constructor "true"
    , CoreDefs.falseUsr & constructor "false"
    , CoreDefs.noneConsUsr & constructor "null"
    #
    , CoreDefs.divide.usr
    & function "sp_divide"
    , CoreDefs.listCons.usr & function "sp_cons"
    , CoreDefs.equal.usr & function "sp_equal"
    , CoreDefs.notEqual.usr & function "sp_not_equal"
    , corelib "Basics" "modBy" & function "basics_modBy"
    , corelib "Basics" "round" & function "Math.round"
    , corelib "Basics" "cloneImm" & function "basics_cloneImm"
    , corelib "Basics" "cloneUni" & function "basics_cloneUni"
    , corelib "Basics" "compare" & function "basics_compare"
    #
    , corelib "Debug" "log"
    & function "sp_log"
    , corelib "Debug" "todo" & function "sp_todo"
    , corelib "Debug" "toHuman" & function "sp_toHuman"
    , corelib "Debug" "benchStart" & function "sp_benchStart"
    , corelib "Debug" "benchStop" & function "sp_benchStop"
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
    , corelib "Self" "internalRepresentation" & function "JSON.stringify"
    ]
    >> List.for Dict.empty __ (fn d, usr & override: Dict.insert d (Compiler/MakeEmittable.translateUsr usr) override)


unaryPlus as Override =
    {
    , call =
        fn env, arguments:
        try arguments as

            [ EA.'argumentSpend fullType arg ]:
                # Num.unaryPlus n == n
                translateExpressionToExpression env 'true arg

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
                JA.'unop "-" (translateExpressionToExpression env 'true arg)

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
loadOverride as Override =
    call =
        fn env, eaArgs:
        jaArgs =
            List.map eaArgs (translateArg { nativeBinop = 'false } env __)

        requestedTypeHumanized as JA.Expr =
            try eaArgs as

                [
                , loadPars
                , EA.'argumentSpend { with  raw = EA.'typeFn [ EA.'parSp { with  raw = compiledType } ] _ } _
                ]:
                    !hash =
                        Hash.fromList []

                    compiledType
                    #>> TA.normalizeType @hash __
                    # TODO: This is a horrid workaround, we should use a dedicated function or a decoder, or even better, unify the types
                    # Actually no, fuck unification, we can't have tyvars!
                    >> toHuman
                    >> Text.replace "\"" "" __
                    >> Text.replace "\n" "" __
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
maybeOverrideUsr as fn Env, EA.TranslatedUsr: JA.Expr =
    fn env, usr:
    try Dict.get usr env.overrides as
        'just ('override { call, value }): value env
        'nothing: JA.'var (_usrToText usr)


maybeOverrideUsrForConstructor as fn Env, EA.TranslatedUsr: JA.Expr =
    fn env, usr:
    try Dict.get usr env.overrides as

        'just ('override { call, value }):
            value env

        'nothing:
            usr
            >> _usrToText
            >> JA.'var


accessAttrs as fn [ Text ], JA.Expr: JA.Expr =
    fn attrPath, e:
    List.for e attrPath (fn a, b: JA.'accessWithDot b a)


translateName as fn Name: Text =
    __
    >> Compiler/MakeEmittable.translateName
    >> "$" .. __


translateUsrToText as fn EA.TranslatedUsr: Text =
    _usrToText


_usrToText as fn EA.TranslatedUsr: Text =
    Text.join "$" __


translateArg as fn { nativeBinop as Bool }, Env, EA.Argument: JA.Expr =
    fn stuff, env, eaExpression:
    try eaExpression as
        EA.'argumentSpend fullType e: translateExpressionToExpression env 'true e
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


wrapInAutoLambda as fn [ JA.Statement ]: JA.Expr =
    fn statements:
    JA.'call (JA.'blockLambda [] statements) []


#
#
#
var TranslatedExpression =
    , 'block [ JA.Statement ]
    , 'inline JA.Expr


translateExpressionToExpression as fn Env, Bool, EA.Expression: JA.Expr =
    fn env, mustReturnValue, expr:
    try translateExpression env mustReturnValue expr as
        'inline e: e
        'block block: wrapInAutoLambda block


makeCall as fn Env, JA.Expr, [ EA.Argument ]: JA.Expr =
    fn env, jaRef, args:
    call =
        args
        >> List.map __ (translateArg { nativeBinop = 'false } env __)
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
        List.filterMap args asRecycled

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
        , [ JA.'binop "=" recycleTempVariable call ]
        # re1 = t[1], re2 = t[2]
        , List.mapWithIndex recycledArgs fn index, arg:
            bracketIndex =
                index + 1
                >> Text.fromNumber
                >> JA.'literal

            JA.'binop "=" arg (JA.'accessWithBrackets bracketIndex recycleTempVariable)
        # t[2]
        , [ JA.'accessWithBrackets (JA.'literal "0") recycleTempVariable ]
        ]
        >> List.concat
        >> JA.'comma


translateExpression as fn Env, Bool, EA.Expression: TranslatedExpression =
    fn env, mustReturnValue, eaExpression:
    perhapsReturn =
        # This is necessary because stray return statements could make the function return when it shouldn't.
        # (This was happening when translating imperative try..as)
        if mustReturnValue then
            JA.'return
        else
            JA.'eval

    try eaExpression as

        EA.'localVariable name:
            name
            >> translateName
            >> JA.'var
            >> 'inline

        EA.'placeholderVariable n:
            n
            >> Text.fromNumber
            >> translateName
            >> JA.'var
            >> 'inline

        EA.'globalVariable usr:
            maybeOverrideUsr env usr >> 'inline

        EA.'call ref args:
            maybeNativeOverride =
                try ref as
                    EA.'globalVariable usr: Dict.get usr env.overrides
                    _: 'nothing

            try maybeNativeOverride as
                'just ('override { call, value = _ }): call env args >> 'inline
                'nothing: makeCall env (translateExpressionToExpression env 'true ref) args >> 'inline

        EA.'fn eaArgs body:
            argsWithNames as [ Bool & Text ] =
                List.mapWithIndex eaArgs fn index, re & maybeName:
                    try maybeName as
                        'just name: re & translateName name
                        'nothing: re & "_" .. Text.fromNumber index

            recycledPars as [ JA.Expr ] =
                argsWithNames
                >> List.filter __ Tuple.first
                >> List.map __ (fn _ & name: JA.'var name)

            statementsRaw as [ JA.Statement ] =
                try translateExpression env 'true body as
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

                    List.map statementsRaw addRecycled

            'inline << JA.'blockLambda (List.map argsWithNames Tuple.second) statementsFinal

        EA.'letIn { inExpression, letExpression, maybeName, type }:
            inStatements =
                try translateExpression env 'true inExpression as
                    'block stats: stats
                    'inline jaExpression: [ perhapsReturn jaExpression ]

            try maybeName as

                'nothing:
                    try translateExpression env 'false letExpression as
                        'inline expr: 'block << JA.'eval expr :: inStatements
                        'block stats: 'block << List.concat [ stats, inStatements ]

                'just name:
                    letStatement =
                        letExpression
                        >> translateExpressionToExpression env 'true __
                        >> JA.'define (type.uni == 'uni) (translateName name) __

                    'block << letStatement :: inStatements

        EA.'literalText string:
            literalString string >> 'inline

        EA.'literalNumber num:
            Text.fromNumber num
            >> JA.'literal
            >> 'inline

        EA.'conditional test true false:
            JA.'conditional (translateExpressionToExpression env 'true test) (translateExpressionToExpression env 'true true) (translateExpressionToExpression env 'true false) >> 'inline

        EA.'and eaTests:
            jaTests =
                List.map eaTests (translateExpressionToExpression env 'true __)

            try List.reverse jaTests as

                []:
                    JA.'literal "true" >> 'inline

                head :: tail:
                    head
                    >> List.for __ tail (fn expr, test: JA.'binop "&&" test expr)
                    >> 'inline

        EA.'isLiteralText text b:
            JA.'binop "===" (translateExpressionToExpression env 'true (EA.'literalText text)) (translateExpressionToExpression env 'true b) >> 'inline

        EA.'isLiteralNumber number b:
            JA.'binop "===" (translateExpressionToExpression env 'true (EA.'literalNumber number)) (translateExpressionToExpression env 'true b) >> 'inline

        EA.'constructor usr:
            maybeOverrideUsrForConstructor env usr >> 'inline

        EA.'constructorAccess argIndex value:
            accessArrayIndex (argIndex + 1) (translateExpressionToExpression env 'true value) >> 'inline

        EA.'isConstructor tUsr eaValue:
            jaValue =
                translateExpressionToExpression env 'true eaValue

            if tUsr == Compiler/MakeEmittable.translateUsr CoreDefs.noneConsUsr then
                JA.'var "true" >> 'inline
            else if tUsr == Compiler/MakeEmittable.translateUsr CoreDefs.trueUsr then
                jaValue >> 'inline
            else if tUsr == Compiler/MakeEmittable.translateUsr CoreDefs.falseUsr then
                JA.'unop "!" jaValue >> 'inline
            else
                tUsr
                >> List.last
                >> Maybe.withDefault __ "TODO List.last error"
                >> translateName
                >> literalString
                >> JA.'binop "===" (accessArrayIndex 0 jaValue) __
                >> 'inline

        EA.'literalRecord maybeExtend attrNamesAndValues:
            obj =
                Dict.empty
                >> List.for __ attrNamesAndValues fn d, name & value:
                    Dict.insert d name (translateExpressionToExpression env 'true value)
                >> JA.'record

            try maybeExtend as

                'nothing:
                    'inline obj

                'just extend:
                    JA.'call
                        (JA.'var "Object.assign")
                        [
                        , JA.'record Dict.empty
                        , translateExpressionToExpression env 'true extend
                        , obj
                        ]
                    >> 'inline

        EA.'recordAccess attrName value:
            JA.'accessWithDot attrName (translateExpressionToExpression env 'true value) >> 'inline

        EA.'missingPattern location value:
            [
            , JA.'literal "'Missing pattern in try..as'"
            , JA.'literal ("'" .. location .. "'")
            , JA.'call (JA.'literal "sp_toHuman") [ translateExpressionToExpression env 'true value ]
            ]
            >> JA.'call (JA.'literal "sp_throw") __
            >> 'inline

        EA.'introspect self:
            self
            >> Self.internalRepresentation
            >> JA.'literal
            >> 'inline


translateConstructorDef as fn EA.TranslatedUsr & EA.RawType: JA.Statement =
    fn tusr & taType:
    #
    nameWithApostrophe as Text =
        tusr
        >> List.last
        >> Maybe.withDefault __ "this should not happen"

    # `(($1, $2, $3) => [ "theConstructorName", $1, $2, $3, ... ])`
    arrayHead =
        literalString (translateName nameWithApostrophe)

    definitionBody =
        try taType as

            EA.'typeFn pars out:
                argNames as [ Text ] =
                    List.mapWithIndex pars (fn index, name: constructorArgumentName (index + 1))

                arrayHead :: List.map argNames JA.'var
                >> JA.'array
                >> JA.'simpleLambda argNames __

            _:
                JA.'array [ arrayHead ]

    JA.'define 'false (translateUsrToText tusr) definitionBody


translateDef as fn Env, EA.GlobalDefinition: Maybe JA.Statement =
    fn env, def:
    try Dict.get def.usr env.overrides as
        'just _: 'nothing
        'nothing: JA.'define 'false (_usrToText def.usr) (translateExpressionToExpression env 'true def.expr) >> 'just


TranslateAllPars =
    {
    , constructors as [ EA.TranslatedUsr & EA.RawType ]
    , eaDefs as [ EA.GlobalDefinition ]
    , platformOverrides as [ EA.TranslatedUsr & Text ]
    }


translateAll as fn TranslateAllPars: [ JA.Statement ] =
    fn pars:
    { constructors, eaDefs, platformOverrides } =
        pars

    jaConstructors as [ JA.Statement ] =
        List.map constructors (translateConstructorDef __)

    env as Env =
        {
        , overrides =
            List.for (coreOverrides 'none) platformOverrides fn d, tUsr & runtimeName:
                Dict.insert d tUsr (function runtimeName)
        }

    jaStatements as [ JA.Statement ] =
        List.filterMap eaDefs (translateDef env __)

    List.concat [ jaConstructors, jaStatements ]
