[#
    `Env` is immutable and depends only on the parent scopes.
    Which means, each scope will have a different one.

    `State` is mutable and is reset per each module.
    We don't have state for now.

#]
Env =
    {
    # This is the innermost record we're updating, which is what the shorthands will apply to:
    #
    #     new = { old with x = .x + 1 }
    #
    , maybeShorthandTarget as Maybe CA.Expression
    , nextGeneratedVariableName as Int
    , nonFn as Dict Name Pos
    #
    # TODO This is used to tell the user that definitions must be in order
    #
    #, futureNonRootValues as Dict Text Pos
    #
    #
    , ro as ReadOnly
    #
    # Non-root values don't need to be expanded with the module name.
    #
    , values as Dict Name { isRoot as Bool, pos as Pos }
    }


ReadOnly =
    {
    , errorModule as Error.Module
    , imports as Meta.Imports
    , resolveToUsr as fn Pos, Maybe Name, Name: Res USR
    , umr as UMR
    }


initEnv as fn ReadOnly: Env =
    fn ro:
    {
    , maybeShorthandTarget = 'nothing
    , nextGeneratedVariableName = 0
    , nonFn = Dict.empty
    , ro
    , values = Dict.empty
    }


#
# Errors
#

erroro as fn ReadOnly, Pos, [ Text ]: Res a =
    fn ro, pos, msg:
    Error.res ro.errorModule pos msg


error as fn Env, Pos, [ Text ]: Res a =
    fn env, pos, msg:
    Error.res env.ro.errorModule pos msg


#
# Dependencies
#

typeDeps as fn Dict USR DependencyType, CA.RawType: Dict USR DependencyType =
    fn acc, type:
    try type as

        CA.'typeNamed _ usr args:
            acc >> Dict.insert usr 'typeDependency __ >> List.for __ args typeDeps

        CA.'typeAnnotationVariable _ _:
            acc

        CA.'typeRecord _ attrs:
            Dict.for acc attrs (fn k, v, a: typeDeps a v)

        CA.'typeError _:
            acc

        CA.'typeFn _ params to:
            acc
            >> typeDeps __ to.raw
            >> List.for __ params fn z, par:
                try par as
                    CA.'parRe raw: typeDeps z raw
                    CA.'parSp full: typeDeps z full.raw


patternDeps as fn CA.Deps, CA.Pattern: CA.Deps =
    fn deps, pattern:
    try pattern as

        # TODO count a constructor dependency only when /instancing/ the constructor, not when matching it!
        CA.'patternConstructor _ usr ps:
            deps
            >> Dict.insert usr 'constructorDependency __
            >> List.for __ ps patternDeps

        CA.'patternRecord _ completeness ps:
            Dict.for deps ps (fn k, v, a: patternDeps a v)

        CA.'patternAny _ _ ('just annotation):
            typeDeps deps annotation.raw

        CA.'patternAny _ _ 'nothing:
            deps

        CA.'patternLiteralNumber _ _:
            deps

        CA.'patternLiteralText _ _:
            deps


expressionDeps as fn CA.Deps, CA.Expression: CA.Deps =
    fn deps, expression:
    try expression as

        CA.'literalNumber _ _:
            deps

        CA.'literalText _ _:
            deps

        CA.'variable _ ('refGlobal usr):
            Dict.insert usr 'valueDependency deps

        CA.'variable _ _:
            deps

        CA.'constructor _ usr:
            Dict.insert usr 'constructorDependency deps

        CA.'fn _ pars body:
            deps
            >> List.for __ pars parameterDeps
            >> expressionDeps __ body

        CA.'record _ 'nothing exprByName:
            Dict.for deps exprByName (fn name, v, a: expressionDeps a v)

        CA.'record _ ('just expr) exprByName:
            deps
            >> expressionDeps __ expr
            >> Dict.for __ exprByName (fn name, v, a: expressionDeps a v)

        CA.'record _ _ exprByName:
            Dict.for deps exprByName (fn name, v, a: expressionDeps a v)

        CA.'recordAccess _ _ e:
            expressionDeps deps e

        CA.'call _ e0 args:
            deps
            >> expressionDeps __ e0
            >> List.for __ args argumentDeps

        CA.'if _ args:
            deps
            >> expressionDeps __ args.condition
            >> expressionDeps __ args.true
            >> expressionDeps __ args.false

        CA.'try _ { patternsAndExpressions, value }:
            addDeps =
                fn d, u & p & b:
                d >> patternDeps __ p >> expressionDeps __ b

            deps
            >> expressionDeps __ value
            >> List.for __ patternsAndExpressions addDeps

        CA.'letIn valueDef e:
            deps
            >> patternDeps __ valueDef.pattern
            >> expressionDeps __ valueDef.body
            >> expressionDeps __ e

        CA.'introspect _ introspect usr:
            dependencyType as DependencyType =
                try introspect as
                    Token.'value: 'valueDependency
                    Token.'type: 'typeDependency
                    Token.'typeOpen: 'typeDependency

            # TODO should somehow use the fact that the type is open or not,
            # forcing the deps to have its innards if it is open?

            Dict.insert usr dependencyType deps


argumentDeps as fn CA.Deps, CA.Argument: CA.Deps =
    fn deps, arg:
    try arg as
        CA.'argumentExpression e: expressionDeps deps e
        CA.'argumentRecycle _ _ _: deps


parameterDeps as fn CA.Deps, CA.Parameter: CA.Deps =
    fn deps, par:
    try par as
        CA.'parameterPattern _ pa: patternDeps deps pa
        _: deps


#
# Definition
#

translateLocalDefinition as fn Env, FA.ValueDef: Res (Env & CA.LocalDef) =
    fn env, fa:
    nonFn =
        fa.nonFn
        >> List.map __ (fn pos & name: name & pos)
        >> Dict.fromList

    fa.pattern
    >> translateFullPattern { env with nonFn } __
    >> onOk fn uni & pattern:
    # TODO: todo: check that the typeclasses are consistent with those declared in parentEnv

    # TODO: check that nonFn contains only names actually used in the annotation?

    env
    >> insertPatternNames 'false pattern __
    >> onOk fn localEnv:
    fa.body
    >> translateExpression localEnv __
    >> onOk fn body:
    localEnv & { body, pattern, uni } >> 'ok


translateRootDefinition as fn Env, FA.ValueDef: Res (Env & CA.ValueDef) =
    fn env, fa:
    nonFn =
        fa.nonFn
        >> List.map __ (fn pos & name: name & pos)
        >> Dict.fromList

    fa.pattern
    >> translateFullPattern { env with nonFn } __
    >> onOk fn uni & pattern:
    # TODO: check that nonFn contains only names actually used in the annotation?

    if uni /= 'imm then
        error env (CA.patternPos pattern) [ "Unique values can be declared only inside functions." ]
    else
        'ok 'none
    >> onOk fn 'none:
    try pattern as
        CA.'patternAny pos ('just name) maybeAnnotation: 'ok (pos & name & maybeAnnotation)
        _: error env (CA.patternPos pattern) [ "Root-level patterns are not (yet?) supported." ]
    >> onOk fn namePos & name & maybeAnnotation:
    env
    >> insertPatternNames 'true pattern __
    >> onOk fn localEnv:
    try fa.body as

        FA.'expression _ _ FA.'native:
            'nothing & Dict.empty >> 'ok

        _:
            translateExpression localEnv fa.body
            >> onOk fn body:
            #
            'just body & expressionDeps Dict.empty body >> 'ok
    >> onOk fn maybeBody & bodyDeps:
    directDeps =
        patternDeps bodyDeps pattern

    localEnv & { directDeps, maybeAnnotation, maybeBody, name, namePos } >> 'ok


#
# Pattern
#
translateAttributeName as fn ReadOnly, FA.Expression: Res (Pos & Name & Maybe FA.Expression) =
    fn ro, FA.'expression _ pos expr_:
    try expr_ as

        FA.'lowercase { attrPath, maybeModule, maybeType, name }:
            if maybeModule /= 'nothing then
                erroro ro pos [ "Attribute names must be single words" ]
            else if attrPath /= [] then
                erroro ro pos [ "Attribute names can't contain dots" ]
            else
                pos & name & maybeType >> 'ok

        _:
            erroro ro pos [ "I need a lowercase attribute name here" ]


translatePatternConstructor as fn Env, Pos, Maybe Name, Name, [ CA.Pattern ]: Res CA.Pattern =
    fn env, pos, maybeModule, name, args:
    env.ro.resolveToUsr pos maybeModule name
    >> onOk fn usr:
    CA.'patternConstructor pos usr args >> 'ok


# TODO too many functions args, use named arguments
translatePatternAny as fn Env, Pos, Maybe FA.Expression, Maybe Name, Name, [ Name ]: Res CA.Pattern =
    fn env, pos, maybeType, maybeModule, name, attrPath:
        if attrPath /= [] then
            error env pos [ "pattern names can't have type attributes" ]
        else
            translateMaybeAnnotation env maybeType
            >> onOk fn maybeAnnotation:
            maybeName =
                if name == "_" then 'nothing else 'just name

            'ok << CA.'patternAny pos maybeName maybeAnnotation


translateMaybeAnnotation as fn Env, Maybe FA.Expression: Res (Maybe CA.Annotation) =
    fn env, maybeFaType:
    try maybeFaType as

        'nothing:
            'ok 'nothing

        'just faType:
            translateRawType env.ro faType
            >> onOk fn raw:
            tyvars =
                CA.typeTyvars raw >> Dict.map (fn tyvarName, pos: { nonFn = Dict.get tyvarName env.nonFn }) __

            { raw, tyvars, univars = CA.typeUnivars raw }
            >> 'just
            >> 'ok


insertPatternRecordAttribute as fn Dict Name CA.Pattern, Env, FA.RecordAttribute: Res (Dict Name CA.Pattern) =
    fn caAttrs, env, attr:
    # { x }
    # { x = pattern }
    # { x as Type }

    translateAttributeName env.ro attr.name
    >> onOk fn pos & caName & maybeFaType:
    if Dict.member caName caAttrs then
        error env pos [ "duplicate attribute name in pattern: " .. caName ]
    else
        try attr.maybeExpr & maybeFaType as

            'just _ & 'just (FA.'expression _ typePos _):
                error env typePos [ "if you want to annotate the attribute, use { x = y as TheType }" ]

            'nothing & 'just faType:
                error env pos [ "TODO annotating record attributes needs more thinking" ]

#                translateRawType env.ro faType
#                >> onOk fn caType:
#
#                caAttrs
#                >> Dict.insert caName (CA.PatternAny pos { maybeName = Just caName, maybeAnnotation = Just caType }) __
#                >> Ok

            'just faPattern
            & 'nothing:
                faPattern
                >> translateRawPattern env __
                >> onOk fn caPattern:
                caAttrs
                >> Dict.insert caName caPattern __
                >> 'ok

            'nothing & 'nothing:
                caAttrs
                >> Dict.insert caName (CA.'patternAny pos ('just caName) 'nothing) __
                >> 'ok


translatePatternRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [ { maybeExpr as Maybe FA.Expression, name as FA.Expression } ]: Res CA.Pattern =
    fn env, pos, maybeMaybeExt, attrs:
    try maybeMaybeExt as

        'just ('just (FA.'expression _ p expr_)):
            error env p [ "Can't extend patterns" ]

        'just 'nothing:
            # { with attr1 = ... }
            'ok CA.'partial

        'nothing:
            # { attr1 = ... }
            'ok CA.'complete
    >> onOk fn completeness:
    Dict.empty
    >> List.forRes __ attrs (insertPatternRecordAttribute __ env __)
    >> Result.map (fn x: CA.'patternRecord pos completeness x) __


translateTuple as fn ReadOnly, (fn FA.Expression: Res ca), FA.BinopChain: Res (Dict Name ca) =
    fn ro, translate, chain:
    faExpressions as [ FA.Expression ] =
        FA.binopChainExpressions chain

    faExpressions
    >> List.mapRes __ translate
    >> onOk fn items:
    pos as Pos =
        List.for Pos.'g faExpressions (fn z, FA.'expression _ p _: Pos.range p z)

    try items as

        [ ca1, ca2 ]:
            Dict.empty
            >> Dict.insert "first" ca1 __
            >> Dict.insert "second" ca2 __
            >> 'ok

        [ ca1, ca2, ca3 ]:
            Dict.empty
            >> Dict.insert "first" ca1 __
            >> Dict.insert "second" ca2 __
            >> Dict.insert "third" ca3 __
            >> 'ok

        _:
            erroro ro pos [ "tuples can be only of size 2 or 3, use a record instead" ]


translateFullPattern as fn Env, FA.Expression: Res (Uniqueness & CA.Pattern) =
    fn env, expr:
    expr
    >> translatePoly env.ro __
    >> onOk fn uni & e:
    translateRawPattern env e
    >> onOk fn caPa:
    uni & caPa >> 'ok


translateRawPattern as fn Env, FA.Expression: Res CA.Pattern =
    fn env, FA.'expression _ pos expr_:
    try expr_ as

        FA.'constructor { maybeModule, name }:
            translatePatternConstructor env pos maybeModule name []

        FA.'lowercase { attrPath, maybeModule, maybeType, name }:
            translatePatternAny env pos maybeType maybeModule name attrPath

        FA.'call (FA.'expression _ p ref) faArgs:
            try ref as

                FA.'constructor { maybeModule, name }:
                    faArgs
                    >> List.mapRes __ (translateRawPattern env __)
                    >> onOk fn caPars:
                    translatePatternConstructor env pos maybeModule name caPars

                _:
                    error env p [ "I need 'constructor name here" ]

        FA.'list _ faItems:
            reversedFaItems =
                List.reverse faItems

            pushItem as fn CA.Pattern, CA.Pattern: CA.Pattern =
                fn last, pattern:
                CA.'patternConstructor (CA.patternPos pattern) CoreDefs.consUsr [ pattern, last ]

            try reversedFaItems as

                []:
                    CA.'patternConstructor pos CoreDefs.nilUsr [] >> 'ok

                [ lastHasDots & FA.'expression _ p lastFaExpr, reversedFaRest... ]:
                    if List.any reversedFaRest Tuple.first then
                        error env p [ "only the last item in a list can have ... triple dots" ]
                    else if not lastHasDots then
                        reversedFaItems
                        >> List.mapRes __ (fn hasDots & expr: translateRawPattern env expr)
                        >> onOk fn reversedCaItems:
                        List.for (CA.'patternConstructor p CoreDefs.nilUsr []) reversedCaItems pushItem >> 'ok
                    else
                        reversedFaRest
                        >> List.mapRes __ (fn hasDots & expr: translateRawPattern env expr)
                        >> onOk fn reversedCaRest:
                        try lastFaExpr as

                            FA.'lowercase { attrPath, maybeModule, maybeType, name }:
                                translatePatternAny env pos maybeType maybeModule name attrPath
                                >> onOk fn caInit:
                                List.for caInit reversedCaRest pushItem >> 'ok

                            _:
                                error env p [ "sorry, I don't understand the dots here..." ]

        FA.'record { with  attrs, maybeExtension }:
            translatePatternRecord env pos maybeExtension attrs

        FA.'binopChain precedence chain:
            if precedence == Op.precedence_tuple then
                chain
                >> translateTuple env.ro (translateRawPattern env __) __
                >> onOk fn recordAttrs:
                CA.'patternRecord pos CA.'complete recordAttrs >> 'ok
            else if precedence == Op.precedence_cons then
                chain
                >> FA.binopChainExpressions
                >> List.mapRes __ (translateRawPattern env __)
                >> onOk fn caPas:
                try List.reverse caPas as

                    last :: rest:
                        last
                        >> List.for __ rest (fn list, item: CA.'patternConstructor pos CoreDefs.consUsr [ item, list ])
                        >> 'ok

                    []:
                        error env pos [ "should not happen: empty cons pattern" ]
            else
                error env pos [ "This binop can't be used in pattern matching" ]

        FA.'literalText singleOrTriple l:
            l
            >> escapeLiteralText singleOrTriple __
            >> CA.'patternLiteralText pos __
            >> 'ok

        FA.'literalNumber isPercent l:
            translateNumber env.ro isPercent CA.'patternLiteralNumber pos l

        # Stuff that's not valid for patterns

        FA.'uppercase _:
            error env pos [ "WUT" ]

        FA.'statements stats:
            error env pos [ "WAT" ]

        FA.'fn _ args body:
            error env pos [ "Can't pattern match on functions. =(" ]

        FA.'unopCall unop expr:
            error env pos [ "This op can't be used in pattern matching" ]

        FA.'if _:
            error env pos [ "if..then can't be used in pattern matching" ]

        FA.'try _:
            error env pos [ "try..as can't be used in pattern matching" ]


escapeLiteralText as fn Token.SingleOrTriple, Text: Text =
    fn singleOrTriple, l:
        try singleOrTriple as

            Token.'singleQuote:
                l

            Token.'tripleQuote:
                l
                >> Text.replace "\"" "\\\"" __
                >> Text.replace "\n" "\\n" __


#
# Statement
#
translateStatements as fn Env, [ FA.Statement ]: Res CA.Expression =
    fn env, stats:
    try stats as

        []:
            CoreDefs.noneConsUsr
            >> CA.'constructor Pos.'g __
            >> 'ok

        [ FA.'evaluation faExpression ]:
            translateExpression env faExpression

        FA.'commentStatement _ :: tail:
            translateStatements env tail

        FA.'evaluation faExpr :: tail:
            faExpr
            >> translateExpression env __
            >> onOk fn caExpr:
            caDef as CA.LocalDef =
                {
                , body = caExpr
                , pattern = CA.'patternAny Pos.'g 'nothing 'nothing
                , uni = 'imm
                }

            tail
            >> translateStatements env __
            >> onOk fn acc:
            CA.'letIn caDef acc >> 'ok

        FA.'valueDef fa :: tail:
            fa
            >> translateLocalDefinition env __
            >> onOk fn newEnv & caDef:
            tail
            >> translateStatements newEnv __
            >> onOk fn acc:
            CA.'letIn caDef acc >> 'ok

        FA.'aliasDef fa :: tail:
            error env fa.name.first [ "Aliases can be declared only in the root scope" ]

        FA.'unionDef fa :: tail:
            error env fa.name.first [ "Types can be declared only in the root scope" ]


##
#- Expression
#
translateExpression as fn Env, FA.Expression: Res CA.Expression =
    fn env, FA.'expression _ pos expr_:
    try expr_ as

        FA.'literalNumber isPercent str:
            translateNumber env.ro isPercent CA.'literalNumber pos str

        FA.'literalText singleOrTriple l:
            l
            >> escapeLiteralText singleOrTriple __
            >> CA.'literalText pos __
            >> 'ok

        FA.'statements stats:
            translateStatements env stats

        FA.'lowercase pas:
            translateLowercase env pos pas

        FA.'uppercase _:
            error env pos [ "Can't reference a type or module here...?" ]

        FA.'constructor { maybeModule, name }:
            env.ro.resolveToUsr pos maybeModule name
            >> onOk fn usr:
            CA.'constructor pos usr >> 'ok

        FA.'fn _ faParams faBody:
            faParams
            >> List.mapRes __ (translateParameter env __)
            >> onOk fn caParams:
            env
            >> List.forRes __ caParams fn envX, par:
                try par as
                    CA.'parameterPattern uni pa: insertPatternNames 'false pa envX
                    CA.'parameterRecycle p name: CA.'patternAny p ('just name) 'nothing >> insertPatternNames 'false __ envX
                    CA.'parameterPlaceholder n: { envX with values = Dict.insert (Text.fromNumber n) { isRoot = 'false, pos } .values } >> 'ok
            >> onOk fn localEnv:
            faBody
            >> translateExpression localEnv __
            >> onOk fn caBody:
            CA.'fn pos caParams caBody >> 'ok

        [# TODO
        , FA.Call (FA.Expression p (FA.Fn faPars faBody)) faArgs:

            - check that args and pairs are the same length

            - for any arg
                if non-trivial
                    and
                    contains a recyclable or is used in faBody more than once (watch out for shadowing!!!)
                then
                    set a let-in
                else
                    use as-is

            - inline the args inside faBody
        #]

        FA.'resolvedArgumentPlaceholder n:
            CA.'variable pos ('refPlaceholder n) >> 'ok

        FA.'call faRef faArgs:
            placeholdersCount & reversedArgs =
                List.for (0 & []) faArgs fn cnt & rev, exp:
                    if isPlaceholder exp then
                        FA.'expression c p _ =
                            exp

                        cnt + 1 & [ FA.'expression c p (FA.'resolvedArgumentPlaceholder cnt), rev... ]
                    else
                        cnt & [ exp, rev... ]

            if placeholdersCount > 0 then
                FA.'call faRef (List.reverse reversedArgs) >> makePartiallyAppliedFunction env pos placeholdersCount __
            else
                faRef
                >> translateExpression env __
                >> onOk fn caRef:
                faArgs
                >> List.mapRes __ (translateArgument env __)
                >> onOk fn caArgs:
                try maybeInlinePlaceholders caRef caArgs as
                    'just call: 'ok call
                    _: CA.'call pos caRef caArgs >> 'ok

        FA.'if { with  condition, false, true }:
            translateExpression env condition
            >> onOk fn c:
            translateExpression env true
            >> onOk fn t:
            translateExpression env false
            >> onOk fn f:
            {
            , condition = c
            , false = f
            , true = t
            }
            >> CA.'if pos __
            >> 'ok

        FA.'unopCall opId faOperand:
            try opId as

                Op.'unopUnique:
                    error env pos [ "can't use ! here because REASONS" ]

                Op.'unopRecycle:
                    error env pos [ "can recycle only in function calls!" ]

                Op.'unopPlus:
                    translateExpression env faOperand

                Op.'unopMinus:
                    faOperand
                    >> translateExpression env __
                    >> onOk fn caOperand:
                    CA.'call pos (CA.'variable pos ('refGlobal CoreDefs.unaryMinus.usr)) [ CA.'argumentExpression caOperand ] >> 'ok

        FA.'binopChain group chain:
            translateBinopChain env pos group chain

        FA.'record { with  attrs, maybeExtension }:
            translateRecord env pos maybeExtension attrs

        FA.'recordShorthand { attrPath, name }:
            translateRecordShorthand env pos attrPath name

        FA.'list _ faDotsAndItems:
            rev =
                List.reverse faDotsAndItems

            try rev as

                []:
                    CA.'constructor pos CoreDefs.nilUsr >> 'ok

                hasDots & head :: rest:
                    if List.any rest Tuple.first then
                        error env pos [ "can use dots only on the last element (for now?)" ]
                    else
                        init & revItems =
                            if hasDots then
                                head & rest
                            else
                                FA.'expression [] pos (FA.'list 'false []) & rev

                        translateExpression env init
                        >> onOk fn caInit:
                        caInit
                        >> List.forRes __ revItems fn acc, _ & faItem:
                            translateExpression env faItem
                            >> onOk fn caItem:
                            CA.'call pos (CA.'constructor pos CoreDefs.consUsr) [ CA.'argumentExpression caItem, CA.'argumentExpression acc ] >> 'ok

        FA.'try { patterns, value }:
            if isPlaceholder value then
                FA.'try { patterns, value = FA.'expression [] pos (FA.'resolvedArgumentPlaceholder 0) } >> makePartiallyAppliedFunction env pos 1 __
            else
                translatePatternAndStatements as fn FA.Expression & FA.Expression: Res (Uniqueness & CA.Pattern & CA.Expression) =
                    fn faPattern & faExpression:
                    faPattern
                    >> translateFullPattern env __
                    >> onOk fn uni & caPattern:
                    env
                    >> insertPatternNames 'false caPattern __
                    >> onOk fn localEnv:
                    faExpression
                    >> translateExpression localEnv __
                    >> onOk fn block:
                    uni & caPattern & block >> 'ok

                translateExpression env value
                >> onOk fn caValue:
                patterns
                >> List.mapRes __ translatePatternAndStatements
                >> onOk fn patternsAndExpressions:
                CA.'try pos { patternsAndExpressions, value = caValue } >> 'ok

        FA.'native:
            error env pos [ "`this_is_sp_native` can be used only for root level value defs" ]

        FA.'introspect introspect maybeModule name:
            env.ro.resolveToUsr pos maybeModule name
            >> onOk fn usr:
            CA.'introspect pos introspect usr >> 'ok

        _:
            error env pos [ "something's wrong here...", toHuman expr_ ]


makePartiallyAppliedFunction as fn Env, Pos, Int, FA.Expr_: Res CA.Expression =
    fn env, pos, placeholdersCount, body:
    ex =
        FA.'expression [] pos __

    List.range 0 (placeholdersCount - 1)
    >> List.map __ (fn x: x >> FA.'resolvedArgumentPlaceholder >> ex)
    >> FA.'fn FA.'inline __ (ex body)
    >> ex
    >> translateExpression env __


maybeInlinePlaceholders as fn CA.Expression, [ CA.Argument ]: Maybe CA.Expression =
    fn ref, outerArgs:
    #
    #      (fn placeholder1, placeholder2: someFunc placeholder1 x placeholder2 y z) p q
    #      |
    #      v
    #      someFunc p x q y z
    #
    try ref as

        CA.'fn _ params (CA.'call pos nestedRef innerArgs):
            replaceArg as fn [ CA.Argument ] & [ CA.Argument ], CA.Argument: [ CA.Argument ] & [ CA.Argument ] =
                fn remainingOuterArgs & argsAccum, innerArg:
                try innerArg & remainingOuterArgs as

                    CA.'argumentExpression (CA.'variable _ ('refPlaceholder _)) & [ next, restOuterArgs... ]:
                        #
                        restOuterArgs & [ next, argsAccum... ]

                    arg & _:
                        remainingOuterArgs & [ arg, argsAccum... ]

            remainingOuterArgs & reversedNewArgs =
                List.for (outerArgs & []) innerArgs replaceArg

            if remainingOuterArgs == [] then
                CA.'call pos nestedRef (List.reverse reversedNewArgs) >> 'just
            else
                'nothing

        _:
            'nothing


insertPatternNames as fn Bool, CA.Pattern, Env: Res Env =
    fn isRoot, pattern, env:
    List.forRes env.values (CA.patternNames pattern) fn vs, paName:
        try Dict.get paName.name vs as

            'just duplicateName:
                error
                    env
                    paName.pos
                    [
                    , "A variable named `" .. paName.name .. "` has already been defined."
                    # TODO display earlier location
                    , "You need to find a less ambiguous name."
                    ]

            'nothing:
                shadowsAGlobal =
                    try Dict.get paName.name env.ro.imports.globalNameToLocation as

                        # No global with this name, all good
                        'nothing:
                            'false

                        # There IS a global with that name!!
                        # If here we are defining exactly that global, all good!
                        # Otherwise we have shadowing and we want to error on that.
                        'just location:
                            if not isRoot then
                                # We are defining a local, so definitely not the global. Shadowing!
                                'true
                            else
                                try env.ro.resolveToUsr Pos.'g 'nothing paName.name as

                                    'err _:
                                        # There is a problem figuring out where the global is from.
                                        # Because we're already reading this module, we can assume that the global is not from here
                                        # So, shadowing.
                                        'true

                                    'ok ('USR umr name):
                                        umr /= env.ro.umr

                if not shadowsAGlobal then
                    Dict.insert paName.name { isRoot, pos = paName.pos } vs >> 'ok
                else
                    error
                        env
                        paName.pos
                        [
                        , "There is already a global variable named `" .. paName.name .. "`."
                        , "You need to find a different name, or modify imports.sp"
                        ]
    >> onOk fn values:
    'ok { env with values }


translateLowercase as fn Env, Pos, { attrPath as [ Name ], maybeModule as Maybe Name, maybeType as Maybe FA.Expression, name as Name }: Res CA.Expression =
    fn env, pos, { attrPath, maybeModule, maybeType, name }:
    if maybeType /= 'nothing then
        error env pos [ "no annotations on var reference" ]
    else
        isLocal =
            maybeModule == 'nothing
            and try Dict.get name env.values as
                'nothing: 'false
                'just paName: not paName.isRoot

        if isLocal then
            'refLocal name >> 'ok
        else
            env.ro.resolveToUsr pos maybeModule name >> Result.map 'refGlobal __
        >> onOk fn ref:
        CA.'variable pos ref
        >> List.for __ attrPath (fn a, b: CA.'recordAccess pos b a)
        >> 'ok


translateRecordShorthand as fn Env, Pos, [ Name ], Name: Res CA.Expression =
    fn env, pos, attrPath, name:
        try env.maybeShorthandTarget as

            'nothing:
                error
                    env
                    pos
                    [
                    , "Record update shorthands must be used inside a record update such as"
                    , "    { aRecord with anAttribute = doSomethingWith ." .. Text.join "." attrPath .. " }"
                    , "but we are not inside a record update!"
                    ]

            'just shorthandTarget:
                shorthandTarget
                >> List.for __ (name :: attrPath) (fn expr, attrName: CA.'recordAccess pos attrName expr)
                >> 'ok


translateParameter as fn Env, FA.Expression: Res CA.Parameter =
    fn env, fa:
    FA.'expression _ pos faExpr =
        fa

    try faExpr as

        FA.'unopCall Op.'unopRecycle (FA.'expression _ p faOperand):
            try faOperand as

                FA.'lowercase { attrPath, maybeModule, maybeType = 'nothing, name }:
                    if maybeModule /= 'nothing or attrPath /= [] then
                        error env pos [ "I need a lowercase local variable name here... =|" ]
                    else
                        CA.'parameterRecycle pos name >> 'ok

                _:
                    error env p [ "@ should be followed by a variable name to recycle!" ]

        FA.'resolvedArgumentPlaceholder n:
            CA.'parameterPlaceholder n >> 'ok

        _:
            translateFullPattern env fa
            >> onOk fn uni & ca:
            CA.'parameterPattern uni ca >> 'ok


translateNumber as fn ReadOnly, Bool, (fn Pos, Number: a), Pos, Text: Res a =
    fn ro, isPercent, constructor, pos, numberAsText:
    try Text.toNumber (Text.replace "_" "" numberAsText) as

        'nothing:
            erroro
                ro
                pos
                [
                , "invalid number: `" .. numberAsText .. "`"
                , "TODO link to documentation on valid number formats"
                ]

        'just n:
            'ok << constructor pos (if isPercent then n / 100 else n)


translateRecord as fn Env, Pos, Maybe (Maybe FA.Expression), [ FA.RecordAttribute ]: Res CA.Expression =
    fn env, pos, maybeMaybeExtension, attrs:
    zzz as Res (Maybe CA.Expression) =
        try maybeMaybeExtension as
            'just ('just ext): translateExpression env ext >> Result.map 'just __
            'just 'nothing: error env pos [ "I need to know what record you are updating" ]
            'nothing: 'ok 'nothing

    zzz
    >> onOk fn maybeCaExt:
    try maybeCaExt as

        'nothing:
            Dict.empty
            >> List.forRes __ attrs (translateAndInsertRecordAttribute __ { env with maybeShorthandTarget = 'nothing } __)
            >> onOk fn caAttrs:
            CA.'record pos 'nothing caAttrs >> 'ok

        'just caExt:
            varName =
                Text.fromNumber env.nextGeneratedVariableName

            var =
                CA.'variable Pos.'g ('refLocal varName)

            newEnv =
                { env with
                , maybeShorthandTarget = 'just var
                , nextGeneratedVariableName = .nextGeneratedVariableName + 1
                }

            Dict.empty
            >> List.forRes __ attrs (translateAndInsertRecordAttribute __ newEnv __)
            >> onOk fn caAttrs:
            def as CA.LocalDef =
                {
                , body = caExt
                , pattern = CA.'patternAny Pos.'g ('just varName) 'nothing
                # TODO ----> This desugaring needs to be done by TypeCheck, because MakeCanonical can't infer its uniqueness
                , uni =
                    'imm
                }

            caAttrs
            >> CA.'record pos ('just var) __
            >> CA.'letIn def __
            >> 'ok


translateAndInsertRecordAttribute as fn Dict Text CA.Expression, Env, FA.RecordAttribute: Res (Dict Text CA.Expression) =
    fn caAttrsAccum, env, attr:
    translateAttributeName env.ro attr.name
    >> onOk fn pos & caName & maybeFaType:
    if Dict.member caName caAttrsAccum then
        error env pos [ "duplicate attribute: " .. caName ]
    else
        attr.maybeExpr
        >> Maybe.withDefault attr.name __
        >> translateExpression env __
        >> onOk fn caExpr:
        caAttrsAccum
        >> Dict.insert caName caExpr __
        >> 'ok


translateArgument as fn Env, FA.Expression: Res CA.Argument =
    fn env, faExpression:
    FA.'expression _ pos expr =
        faExpression

    try expr as

        FA.'unopCall Op.'unopRecycle (FA.'expression _ _ faOperand):
            try faOperand as

                FA.'lowercase { attrPath, maybeModule, maybeType, name }:
                    if maybeType /= 'nothing then
                        error env pos [ "Sorry, at least for now annotations are not supported here" ]
                    else if maybeModule /= 'nothing then
                        error env pos [ "Only values declared inside a function scope can be mutated!" ]
                    else
                        CA.'argumentRecycle pos name attrPath >> 'ok

                _:
                    error env pos [ "I can recycle only variables!" ]

        FA.'argumentPlaceholder:
            error env pos [ "compiler error: this should have been eliminated already" ]

        FA.'resolvedArgumentPlaceholder n:
            CA.'argumentExpression (CA.'variable pos ('refPlaceholder n)) >> 'ok

        _:
            faExpression
            >> translateExpression env __
            >> onOk fn caExpr:
            CA.'argumentExpression caExpr >> 'ok


isPlaceholder as fn FA.Expression: Bool =
    fn FA.'expression _ _ expr:
    try expr as
        FA.'argumentPlaceholder: 'true
        _: 'false


translateBinopChain as fn Env, Pos, Int, FA.BinopChain: Res CA.Expression =
    fn env, pos, group, opChain:
    toExpression as fn FA.Expr_: FA.Expression =
        FA.'expression [] pos __

    cnt0 & head =
        if isPlaceholder opChain.first then
            1 & toExpression (FA.'resolvedArgumentPlaceholder 0)
        else
            0 & opChain.first

    (placeholdersCount as Int) & (reversedChainTail as [ FA.Binop & FA.Expression ]) =
        List.for (cnt0 & []) opChain.second fn cnt & rev, op & exp:
            if isPlaceholder exp then
                FA.'expression c p _ =
                    exp

                cnt + 1 & [ op & FA.'expression c p (FA.'resolvedArgumentPlaceholder cnt), rev... ]
            else
                cnt & [ op & exp, rev... ]

    if placeholdersCount > 0 then
        FA.'binopChain group (head & List.reverse reversedChainTail) >> makePartiallyAppliedFunction env pos placeholdersCount __
    else if group == Op.precedence_pipe then
        resolvePipe env pos opChain >> onOk (translateExpression env __)
    else if group == Op.precedence_tuple then
        translateTupleExpression env pos opChain
    else if group == Op.precedence_comparison then
        translateComparison env pos opChain
    else if group == Op.precedence_logical then
        translateLogical env pos opChain
    else if group == Op.precedence_mutop then
        translateMutop env pos opChain
    else if group == Op.precedence_addittive then
        translateLeftAssociativeBinopChain env pos opChain
    else if group == Op.precedence_multiplicative then
        translateLeftAssociativeBinopChain env pos opChain
    else
        translateRightAssociativeBinopChain env pos opChain


resolvePipe as fn Env, Pos, FA.BinopChain: Res FA.Expression =
    fn env, pos, opChain:
    if FA.binopChainAllBinops (fn sep: sep.usr == CoreDefs.sendRight.usr) opChain then
        #
        #   head >> b >> c >> d    --->   d (c (b head))
        #
        head & chainTail =
            opChain

        List.for head chainTail fn acc, sep & faExp:
            FA.'expression _ p _ =
                faExp

            FA.'expression [] p (FA.'call faExp [ acc ])
        >> 'ok
    else if FA.binopChainAllBinops (fn sep: sep.usr == CoreDefs.sendLeft.usr) opChain then
        #
        #   head << b << c << d    --->   head (b (c d))
        #
        last & body =
            FA.binopChainReverse opChain

        List.for last body fn acc, sep & faExp:
            FA.'expression _ p _ =
                faExp

            FA.'expression [] p (FA.'call faExp [ acc ])
        >> 'ok
    else
        error env pos [ "Mixing `>>` and `<<` is ambiguous. Use parens!" ]


translateTupleExpression as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, one & chainTail:
    try chainTail as

        []:
            translateExpression env one

        [ _ & two ]:
            translateExpression env one
            >> onOk fn first:
            translateExpression env two
            >> onOk fn second:
            Dict.empty
            >> Dict.insert "first" first __
            >> Dict.insert "second" second __
            >> CA.'record pos 'nothing __
            >> 'ok

        [ _ & two, _ & three ]:
            translateExpression env one
            >> onOk fn first:
            translateExpression env two
            >> onOk fn second:
            translateExpression env three
            >> onOk fn third:
            Dict.empty
            >> Dict.insert "first" first __
            >> Dict.insert "second" second __
            >> Dict.insert "third" third __
            >> CA.'record pos 'nothing __
            >> 'ok

        _:
            error env pos [ "Tuples can't have more than 3 items, use a record instead." ]


translateComparison as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, opChain:
    try opChain.second as

        []:
            translateExpression env opChain.first

        [ sep & second ]:
            translateRightAssociativeBinopChain env pos opChain

        [ firstSep & second, moar... ]:
            if FA.binopChainAllBinops (sameDirectionAs firstSep __) opChain then
                # TODO expand `a < b < c` to `a < b and b < c` without calculating b twice
                error env pos [ "TODO: not (yet) implemented: compops expansion" ]
            else
                # TODO actually list the seps
                error env pos [ "can't mix comparison ops with different direction" ]


translateLogical as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, opChain:
    allSame =
        FA.binopChainAllBinops (fn sep: sep.usr == CoreDefs.and_.usr) opChain or FA.binopChainAllBinops (fn sep: sep.usr == CoreDefs.or_.usr) opChain

    if allSame then
        translateRightAssociativeBinopChain env pos opChain
    else
        error env pos [ "Mixing `and` and `or` is ambiguous. Use parens!" ]


translateMutop as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, left & chainTail:
    try chainTail as

        []:
            translateExpression env left

        [ op & right ]:
            caRef =
                CA.'variable op.pos ('refGlobal op.usr)

            [ left, right ]
            >> List.mapRes __ (translateArgument env __)
            >> onOk fn caArgs:
            CA.'call pos caRef caArgs >> 'ok

        _:
            error env pos [ "mutops can't be chained" ]


sameDirectionAs as fn FA.Binop, FA.Binop: Bool =
    fn a, b:
    if a.symbol == b.symbol then
        'true
    else
        try a.symbol as
            ">": b.symbol == ">="
            ">=": b.symbol == ">"
            "<": b.symbol == "<="
            "<=": b.symbol == "<"
            _: 'false


translateRightAssociativeBinopChain as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, faLeft & faOpsAndRight:
    try faOpsAndRight as

        []:
            translateExpression env faLeft

        [ op & faRight, faTail... ]:
            translateArgument env faLeft
            >> onOk fn caLeft:
            translateRightAssociativeBinopChain env pos (faRight & faTail)
            >> onOk fn caRight:
            caRef =
                CA.'variable op.pos ('refGlobal op.usr)

            CA.'call pos caRef [ caLeft, CA.'argumentExpression caRight ] >> 'ok


translateLeftAssociativeBinopChain as fn Env, Pos, FA.BinopChain: Res CA.Expression =
    fn env, pos, left & opsAndRight:
    translateExpression env left
    >> onOk fn caLeft:
    translateBinopChainRec env pos caLeft opsAndRight


translateBinopChainRec as fn Env, Pos, CA.Expression, [ FA.Binop & FA.Expression ]: Res CA.Expression =
    fn env, pos, leftAccum, opsAndRight:
    try opsAndRight as

        []:
            'ok leftAccum

        [ op & faRight, tail... ]:
            translateArgument env faRight
            >> onOk fn caRight:
            CA.'call
                pos
                (CA.'variable op.pos ('refGlobal op.usr))
                [
                , CA.'argumentExpression leftAccum
                , caRight
                ]
            >> translateBinopChainRec env pos __ tail


#
# Type
#

translateAndInsertRecordAttributeType as fn Dict Name CA.RawType, ReadOnly, FA.RecordAttribute: Res (Dict Name CA.RawType) =
    fn caAttrs, ro, faAttr:
    translateAttributeName ro faAttr.name
    >> onOk fn pos & name & maybeFaType:
    if Dict.member name caAttrs then
        erroro ro pos [ "Duplicate attribute name: " .. name ]
    else
        try maybeFaType as

            'nothing:
                erroro ro pos [ "I need to see the type of this attribute, `" .. name .. " as TheType`" ]

            'just faType:
                faType
                >> translateRawType ro __
                >> onOk fn caType:
                if faAttr.maybeExpr /= 'nothing then
                    erroro ro pos [ "I need a type here; `=` is for assignign values" ]
                else
                    caAttrs
                    >> Dict.insert name caType __
                    >> 'ok


translateTypeFunctionParameter as fn ReadOnly, FA.Expression: Res CA.ParType =
    fn ro, expression:
    FA.'expression _ _ expr_ =
        expression

    try expr_ as

        FA.'unopCall Op.'unopRecycle faOperand:
            faOperand
            >> translateRawType ro __
            >> Result.map CA.'parRe __

        _:
            expression
            >> translateFullType ro __
            >> Result.map CA.'parSp __


translatePoly as fn ReadOnly, FA.Expression: Res (Uniqueness & FA.Expression) =
    fn ro, expr:
    FA.'expression _ pos expr_ =
        expr

    try expr_ as

        FA.'unopCall Op.'unopUnique e:
            'uni & e >> 'ok

        FA.'poly numberAsString e:
            try Text.toNumber numberAsString as
                'nothing: erroro ro pos [ "I need an integer number here" ]
                'just n: 'depends n & e >> 'ok

        _:
            'imm & expr >> 'ok


translateFullType as fn ReadOnly, FA.Expression: Res CA.FullType =
    fn ro, expr:
    expr
    >> translatePoly ro __
    >> onOk fn uni & e:
    translateRawType ro e
    >> onOk fn raw:
    { raw, uni } >> 'ok


translateRawType as fn ReadOnly, FA.Expression: Res CA.RawType =
    fn ro, FA.'expression _ pos expr_:
    try expr_ as

        FA.'uppercase { maybeModule, name }:
            ro.resolveToUsr pos maybeModule name
            >> onOk fn usr:
            CA.'typeNamed pos usr [] >> 'ok

        FA.'lowercase { attrPath, maybeModule, maybeType, name }:
            if maybeType /= 'nothing then
                erroro ro pos [ "Can't really specify the type of a type." ]
            else if maybeModule /= 'nothing then
                erroro ro pos [ "no modules for tyvars!" ]
            else if attrPath /= [] then
                erroro ro pos [ "no attributes for tyvars!" ]
            else
                CA.'typeAnnotationVariable pos name >> 'ok

        FA.'call (FA.'expression _ refPos ref) faArgs:
            try ref as

                FA.'uppercase { maybeModule, name }:
                    faArgs
                    >> List.mapRes __ (translateRawType ro __)
                    >> onOk fn caArgs:
                    ro.resolveToUsr pos maybeModule name
                    >> onOk fn usr:
                    CA.'typeNamed pos usr caArgs >> 'ok

                _:
                    erroro ro refPos [ "I need an Uppercase type name here" ]

        FA.'list _ dotsAndItems:
            try dotsAndItems as

                []:
                    erroro ro pos [ "You need to specify the type of the List items" ]

                [ hasDots & faItem ]:
                    if hasDots then
                        erroro ro pos [ "No need to use dots here" ]
                    else
                        translateRawType ro faItem
                        >> onOk fn caItem:
                        CoreDefs.listType caItem >> 'ok

                _:
                    erroro ro pos [ "List items must all have the same type, so you can specify only one type" ]

        FA.'record { with  attrs, maybeExtension }:
            if maybeExtension /= 'nothing then
                erroro ro pos [ "Experimentally, extensible type annotations are disabled" ]
            else
                Dict.empty
                >> List.forRes __ attrs (translateAndInsertRecordAttributeType __ ro __)
                >> onOk fn caAttrs:
                caAttrs
                >> CA.'typeRecord pos __
                >> 'ok

        FA.'fn _ faParams faReturn:
            faParams
            >> List.mapRes __ (translateTypeFunctionParameter ro __)
            >> onOk fn caParams:
            faReturn
            >> translateFullType ro __
            >> onOk fn caReturn:
            CA.'typeFn pos caParams caReturn >> 'ok

        FA.'binopChain precedence chain:
            if precedence == Op.precedence_tuple then
                chain
                >> translateTuple ro (translateRawType ro __) __
                >> onOk fn recordAttrs:
                CA.'typeRecord pos recordAttrs >> 'ok
            else
                erroro ro pos [ "This operator can't be used in type definitions", toHuman expr_ ]

        _:
            # TODO: do all other constructors explicitly
            erroro ro pos [ "Not sure what's up with this type =|", toHuman expr_ ]


#
# Union constructor
#

translateConstructor as fn CA.RawType, USR, Dict Name Pos, FA.Expression, Dict Name CA.ConstructorDef & Env: Res (Dict Name CA.ConstructorDef & Env) =
    fn varType, varUsr, varPars, FA.'expression _ pos expr_, constructors & env:
    try expr_ as

        FA.'constructor { maybeModule = 'nothing, name }:
            name & [] >> 'ok

        FA.'call (FA.'expression _ _ (FA.'constructor { maybeModule = 'nothing, name })) pars:
            name & pars >> 'ok

        _:
            error
                env
                pos
                [
                , "I need a 'constructor name here!"
                ]
    >> onOk fn name & faPars:
    if Dict.member name constructors then
        # TODO "union $whatever has two constructors with the same name!"
        error env pos [ "constructor " .. name .. " is duplicate" ]
    else
        'ok 'none
    >> onOk fn 'none:
    faPars
    >> List.mapRes __ (translateRawType env.ro __)
    >> onOk fn ins:
    tyvars as Dict Name Pos =
        List.for Dict.empty ins (fn dict, in: Dict.join (CA.typeTyvars in) dict)

    undeclaredTyvars =
        Dict.diff tyvars varPars

    if undeclaredTyvars == Dict.empty then
        'ok 'none
    else
        toError as fn Name & Pos: Error =
            fn n & p:
            Error.'simple env.ro.errorModule p [ "Undeclared type variable: " .. n ]

        undeclaredTyvars
        >> Dict.toList
        >> List.map __ toError
        >> Error.'nested
        >> 'err
    >> onOk fn 'none:
    env
    >> insertPatternNames 'true (CA.'patternAny pos ('just name) 'nothing) __
    >> onOk fn newEnv:
    directDeps as CA.Deps =
        Dict.ofOne varUsr 'typeDependency >> List.for __ ins typeDeps

    'USR umr _ =
        varUsr

    c as CA.ConstructorDef =
        {
        , constructorUsr = 'USR umr name
        , directDeps
        , ins
        , name
        , out = varType
        , pos
        , variantTypeUsr = varUsr
        }

    Dict.insert name c constructors & newEnv >> 'ok


#
# Module
#

insertRootStatement as fn CA.Module & Env, FA.Statement: Res (CA.Module & Env) =
    fn caModule & env, faStatement:
    try faStatement as

        FA.'evaluation (FA.'expression _ pos _):
            error env pos [ "Root Evaluations don't really do much =|" ]

        FA.'valueDef d:
            d
            >> translateRootDefinition env __
            >> onOk fn newEnv & def:
                # TODO check against name duplication?
                { caModule with valueDefs = Dict.insert def.name def .valueDefs } & newEnv >> 'ok

        FA.'aliasDef fa:
            pos & name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.variantTypeDefs then
                error env fa.name.first [ name .. " declared twice!" ]
            else
                # TODO check that args are not duplicate

                fa.type
                >> translateRawType env.ro __
                >> onOk fn type:
                aliasDef as CA.AliasDef =
                    {
                    , directDeps = typeDeps Dict.empty type
                    , pars = List.map fa.args (fn p & n: n & p)
                    , type
                    , usr = 'USR env.ro.umr name
                    }

                { caModule with aliasDefs = Dict.insert name aliasDef .aliasDefs } & env >> 'ok

        FA.'unionDef fa:
            pos & name =
                fa.name

            if Dict.member name caModule.aliasDefs or Dict.member name caModule.variantTypeDefs then
                error env pos [ name .. " declared twice!" ]
            else
                # TODO check that args are not duplicate
                caPars as [ Name & Pos ] =
                    List.map fa.args (fn p & n: n & p)

                usr =
                    'USR env.ro.umr name

                type =
                    caPars
                    >> List.map __ (fn n & p: CA.'typeAnnotationVariable p n)
                    >> CA.'typeNamed pos usr __

                Dict.empty & env
                >> List.forRes __ fa.constructors (fn a, b: translateConstructor type usr (Dict.fromList caPars) b a)
                >> onOk fn constructors & newEnv:
                varDef as CA.VariantTypeDef =
                    {
                    , constructors
                    , pars = caPars
                    , usr
                    }

                newModule =
                    { caModule with
                    , constructorDefs = Dict.for .constructorDefs constructors Dict.insert
                    , variantTypeDefs = Dict.insert name varDef .variantTypeDefs
                    }

                newModule & newEnv >> 'ok


translateModule as fn ReadOnly, FA.Module: Res CA.Module =
    fn ro, faModule:
    #Debug.benchStart 'none

    module =
        CA.initModule ro.errorModule.fsPath ro.umr ro.errorModule.content

    # Add all definitions
    module & initEnv ro
    >> List.forRes __ faModule (insertRootStatement __ __)
    >> Result.map Tuple.first __


#>> btw Debug.benchStop "translateModule" __

textToCanonicalModule as fn Bool, ReadOnly: Res CA.Module =
    fn stripLocations, ro:
    {
    , errorModule = ro.errorModule
    , keepComments = 'false
    , stripLocations
    }
    >> Compiler/Parser.textToFormattableModule
    >> Result.onOk fn faModule:
    translateModule ro faModule
