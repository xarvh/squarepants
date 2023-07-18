[#

    Takeaways from:
    https://www.youtube.com/watch?v=x3evzO8O9e8
    https://drive.google.com/file/d/1NRkP0hz-0Yo49Rto70b2nUwxjPiGD9Ci/view


    distinguish between annotated tyvars for polymorphic functions and unification tyvars

    unification vars should exist only in an intermediate representation, not in CA.AST and not in EA.AST

    CA -> Elaborated with holes -> Fully typed AST
    Fully typed AST can contain type errors for run-time instead of compile-time failure


    solve trivial stuff while collecting constraints




    binders: assignments, function declaration parameters, try..as?

    function call arguments


    typeclasses pass a record of the type-appropriate functions


    Other stuff in my to-read list:
      https://okmij.org/ftp/Computation/typeclass.html
      https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.127.8206&rep=rep1&type=pdf
      https://jeremymikkola.com/posts/2019_01_01_type_inference_intro.html
      https://gist.github.com/chrisdone/0075a16b32bfd4f62b7b
      https://smunix.github.io/dev.stephendiehl.com/fun/006_hindley_milner.html

#]



typeToHuman as fn Env, TA.RawType: Text =
    fn env, raw:

    raw
    >> Human/Type.doRawType {} __
    >> TT.toText "" __



bug as fn Text: a =
    fn msg:
    todo ("Compiler bug: " .. msg)


alias State =
    {
    , errors as Array Error
    , lastUnificationVarId as Int

    , tyvarsById as Hash TA.TyvarId TA.Tyvar
    , univarsById as Hash UnivarId TA.Univar

    , tyvarSubs as Hash TA.TyvarId TA.RawType
    , univarSubs as Hash UnivarId Uniqueness
    }


alias UnivarEquality =
    {
    , context as Context
    , pos as Pos
    , why as Text
    , id as UnivarId
    , uni as Uniqueness
    }


initState as fn !Int: !State =
    fn !lastUnificationVarId:
    {
    , errors = Array.fromList []
    , lastUnificationVarId
    , tyvarsById = Hash.fromList []
    , univarsById = Hash.fromList []
    , tyvarSubs = Hash.fromList []
    , univarSubs = Hash.fromList []
    }


alias Instance =
    {
    , definedAt as Pos
    , type as TA.FullType
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    , freeUnivars as Dict UnivarId TA.Univar
    }


alias ExpandedAlias =
    {
    , pars as [TA.TyvarId]
    , type as TA.RawType
    }


alias Env =
    {
    , errorModule as Error.Module
    , context as Context
    , constructors as ByUsr Instance
    , variables as Dict Ref Instance

    , expandedAliases as ByUsr ExpandedAlias

    , annotatedTyvarsByName as Dict Name TA.TyvarId
    , annotatedUnivarsByOriginalId as Dict UnivarId UnivarId
    }


initEnv as Env =
    {
    , errorModule = { fsPath = "<internal>", content = "" }
    , context = Context_Global
    , constructors = Dict.empty
    , variables = Dict.empty
    , expandedAliases = Dict.empty
    , annotatedTyvarsByName = Dict.empty
    , annotatedUnivarsByOriginalId = Dict.empty
    }



# TODO: once we have proper error messages, there won't be much point in using these instead than error functions directly
union Error_ =
    , ErrorUnresolvableUniqueness UnivarEquality Uniqueness
    , ErrorShouldBeUnique
    , ErrorCircularValue
    , ErrorVariableNotFound Ref
    , ErrorConstructorNotInType Name
    , ErrorNotCompatibleWithRecord
    , ErrorNotCompatibleWithUnion
    , ErrorRecordDoesNotHaveAttribute Name [# TODO other attrs to give context? #]
    , ErrorRecordHasAttributesNotInAnnotation # TODO which attrs?
    , ErrorRecordIsMissingAttibutesInAnnotation # TODO which attrs?
    , ErrorTryingToAccessAttributeOfNonRecord Name TA.RawType
    , ErrorIncompatibleTypes CA.Expression TA.FullType
    , ErrorIncompatiblePattern CA.Pattern TA.FullType
    , ErrorCallingANonFunction TA.RawType
    , ErrorWrongNumberOfArguments { given as Int, expected as Int, reference as CA.Expression }
    , ErrorWrongNumberOfParameters
    , ErrorWrongNumberOfConstructorArguments
    , ErrorNotEnoughArguments
    , ErrorIncompatibleRecycling
    , ErrorUniquenessDoesNotMatch { fix as Uniqueness, mut as Uniqueness }
    , ErrorUniquenessDoesNotMatchArgument
    , ErrorUniquenessDoesNotMatchParameter Uniqueness TA.FullType
    , ErrorRecyclingDoesNotMatch
    , ErrorUndefinedTypeVariable Name
    , ErrorWrongNumberOfTypeArguments USR [TA.TyvarId] [TA.RawType]
    , ErrorNamedTypeNotFound USR
    , ErrorCircularAlias [USR]
    , ErrorTypeAllowsFunctions TA.RawType TA.Tyvar UMR
    , ErrorUniInTypeArg
    , ErrorUniInRecordAttribute Name
    , ErrorUniqueGlobal


union Context =
    , Context_Global # this is never actually used =|
    , Context_Module UMR
    , Context_Argument Name Context
    , Context_LetInBody [Name]
    , Context_FnPar Int Context
    , Context_FnBody Pos Context
    , Context_TryBranch
    , Context_IfCondition
    , Context_IfFalse
    , Context_IfTrue
    , Context_AttributeName Name Context
    , Context_ConstructorArgument Name Int


union Why =
    , Why_Annotation
    , Why_LetIn
    , Why_Record
    , Why_RecordExt
    , Why_RecordAccess
    , Why_IfCondition
    , Why_IfBranches
    , Why_TryPattern
    , Why_TryExpression
    , Why_ReturnType
    , Why_Argument Int
    , Why_CalledAsFunction
    , Why_Todo
    , Why_Attribute Why
    , Why_FunctionInput Int Why
    , Why_FunctionOutput Why
    , Why_TypeArgument USR Int Why


# TODO, make it obvious that the two types are not interchangeable, because of uniqueness
# (but how do I describe it? "given" and "required"? I forgot already what they mean.
# Also also: maybe context should follow each individual type, so we actually know what they refer to!?
alias Equality = {
    , context as Context
    , pos as Pos
    , why as Why
    , type1 as TA.RawType
    , type2 as TA.RawType
    , expandedRecursives as Set USR
    }


#
# Core types
#
coreTypeBool as TA.RawType =
    TA.TypeUnion Nothing CoreTypes.boolCons

coreTypeNumber as TA.RawType =
    TA.TypeOpaque CoreTypes.numberUsr []

coreTypeText as TA.RawType =
    TA.TypeOpaque CoreTypes.textUsr []


fullTypeError as TA.FullType =
    { uni = Uni, raw = TA.TypeError }


patternError as fn Pos: TA.Pattern =
    fn pos:
    TA.PatternAny pos { maybeName = Nothing, type = fullTypeError }


#
#
#
newTyvarId as fn @State: TA.TyvarId =
    fn @state:
    @state.lastUnificationVarId += 1
    cloneUni @state.lastUnificationVarId


newRawType as fn @State: TA.RawType =
    fn @state:
    TA.TypeVar (newTyvarId @state)


addEquality as fn Env, Pos, Why, TA.RawType, TA.RawType, @State: None =
    fn env, pos, why, type1, type2, @state:

    solveEquality env { context = env.context, pos, why, type1, type2, expandedRecursives = Set.empty } @state


addError as fn Env, Pos, Error_, @State: None =
    fn env, pos, error, @state:

    [
    , Debug.toHuman error
    , Debug.toHuman env.context
    ]
    >> Error.Simple env.errorModule pos __
    >> Array.push @state.errors __


addErrorIf as fn Bool, Env, Pos, Error_, @State: None =
    fn test, env, pos, error, @state:

    if test then addError env pos error @state else None


getConstructorByUsr as fn USR, Env: Maybe Instance =
    fn usr, env:

    Dict.get usr env.constructors


getVariableByRef as fn Ref, Env: Maybe Instance =
    fn ref, env:

    Dict.get ref env.variables


#
#
# Generalize
#
#
generalize as fn Env, Pos, Ref, Instance, @State: TA.FullType =
    fn env, pos, ref, instance, @state:

    # TODO this function is not inline because the parser gets confused
    replaceUnivar =
        fn originalUnivarId, _, r:
        newUnivarId = newTyvarId @state
        #Hash.insert @state.univarsById newUnivarId { originalId = originalUnivarId, constraints = []}
        replaceUnivarRec originalUnivarId (Depends newUnivarId) r

    raw =
        instance.type.raw
        >> Dict.for __ instance.freeUnivars replaceUnivar
        >> Dict.for __ instance.freeTyvars fn originalTyvarId, tyvar, a:

            generalizedTyvarId =
                newTyvarId @state

            # The new tyvar has the same typeclasses as the original!
            Hash.insert @state.tyvarsById generalizedTyvarId { tyvar with generalizedAt = pos, generalizedFor = ref }

            applySubstitutionToType originalTyvarId (TA.TypeVar (generalizedTyvarId)) a

    { instance.type with raw }


replaceUnivarRec as fn UnivarId, Uniqueness, TA.RawType: TA.RawType =
    fn old, new, raw:

    doRaw as fn TA.RawType: TA.RawType =
        replaceUnivarRec old new __

    try raw as
        , TA.TypeOpaque usr args:
            TA.TypeOpaque usr (List.map doRaw args)

        , TA.TypeUnion maybeExt consByName:
            TA.TypeUnion maybeExt (Dict.map (fn k, v: List.map doRaw v) consByName)

        , TA.TypeRecord maybeExt attrs:
            TA.TypeRecord maybeExt (Dict.map (fn k, v: doRaw v) attrs)

        , TA.TypeError:
            TA.TypeError

        , TA.TypeVar id:
            TA.TypeVar id

        , TA.TypeFn ins out:

            doUni as fn Uniqueness: Uniqueness =
                fn uni:
                try uni as
                    , Depends id: if id == old then new else uni
                    , _: uni

            mapPar as fn TA.ParType: TA.ParType =
                fn par:
                try par as
                    , TA.ParRe r: TA.ParRe (doRaw r)
                    , TA.ParSp f: TA.ParSp { uni = doUni f.uni, raw = doRaw f.raw }

            TA.TypeFn (List.map mapPar ins) { uni = doUni out.uni, raw = doRaw out.raw }


#
#
# CA to TA translation
#
#
expandTyvarsInType as fn Dict TA.TyvarId TA.RawType, @State, TA.RawType: TA.RawType =
    fn tyvarIdsToType, @state, type:

    rec =
        expandTyvarsInType tyvarIdsToType @state __

    try type as
        , TA.TypeOpaque usr args:
            TA.TypeOpaque usr (List.map rec args)

        , TA.TypeFn ins out:
            TA.TypeFn (TA.mapPars rec ins) { out with raw = rec .raw }

        , TA.TypeRecord maybeId attrs:
            TA.TypeRecord maybeId (Dict.map (fn k, v: rec v) attrs)

        , TA.TypeUnion maybeId consByName:
            TA.TypeUnion maybeId (Dict.map (fn k, v: List.map rec v) consByName)

        , TA.TypeRecursive usr args:
            TA.TypeRecursive usr (List.map rec args)

        , TA.TypeVar id:
            try Dict.get id tyvarIdsToType as
                , Nothing: bug "this is not supposed to happen"
                , Just ty: ty

        , TA.TypeError:
            TA.TypeError


# This is not really a "translate", it just replaces the originalId with the generated one
translateUni as fn Dict UnivarId UnivarId, Uniqueness: Uniqueness =
    fn originalIdToNewId, originalUni:

    try originalUni as
        , Depends originalId:
            try Dict.get originalId originalIdToNewId as
                , Just newId:
                    Depends newId

                , Nothing:
                    originalUni
        , _:
            originalUni


translateFullType as fn Env, Dict Name TA.RawType, Dict UnivarId UnivarId, @State, CA.FullType: TA.FullType =
    fn env, argsByName, originalIdToNewId, @state, caFull:

    {
    , uni = translateUni originalIdToNewId caFull.uni
    , raw = translateRawType env argsByName originalIdToNewId @state caFull.raw
    }


translateRawType as fn Env, Dict Name TA.RawType, Dict UnivarId UnivarId, @State, CA.RawType: TA.RawType =
    fn env, argsByName, originalIdToNewId, @state, caType:

    rec as fn CA.RawType: TA.RawType =
        translateRawType env argsByName originalIdToNewId @state __

    try caType as

        , CA.TypeFn pos caPars caOut:

            zzz = fn caPar:
                    try caPar as
                        , CA.ParRe caRaw: TA.ParRe (rec caRaw)
                        , CA.ParSp caFull: TA.ParSp (translateFullType env argsByName originalIdToNewId @state caFull)

            taArgs as [TA.ParType] =
                List.map zzz caPars

            TA.TypeFn taArgs (translateFullType env argsByName originalIdToNewId @state caOut)


        , CA.TypeRecord pos caAttrs:
            TA.TypeRecord Nothing (Dict.map (fn name, v: rec v) caAttrs)


        , CA.TypeUnion pos consByName:
            TA.TypeUnion Nothing (Dict.map (fn name, v: List.map rec v) consByName)


        , CA.TypeRecursive pos usr args:
            TA.TypeRecursive usr (List.map rec args)


        , CA.TypeAnnotationVariable pos name:
            try Dict.get name argsByName as
                , Nothing:
                    addError env pos (ErrorUndefinedTypeVariable name) @state
                    TA.TypeError

                , Just raw:
                    raw


        , CA.TypeNamed pos usr pars:

            expandedPars as [TA.RawType] =
                List.map rec pars

            try Dict.get usr env.expandedAliases as
                , Nothing:
                    TA.TypeOpaque usr expandedPars

                , Just expandedAlias:
                    if List.length expandedAlias.pars /= List.length expandedPars then
                        addError env pos (ErrorWrongNumberOfTypeArguments usr expandedAlias.pars expandedPars) @state
                        TA.TypeError

                    else
                        tyvarIdsToType as Dict TA.TyvarId TA.RawType =
                            List.map2 Tuple.pair expandedAlias.pars expandedPars
                            >> Dict.fromList

                        expandTyvarsInType tyvarIdsToType @state expandedAlias.type


translateAnnotationType as fn Env, @State, CA.RawType: TA.RawType =
    fn env, @state, ca:

    nameToType =
        Dict.map (fn k, v: TA.TypeVar v) env.annotatedTyvarsByName

    translateRawType env nameToType env.annotatedUnivarsByOriginalId @state ca



union CanBeCast =
    , CanBeCastYes
    # When cast is not possible, it is still possible to resolve the check by imposing a constrain on one of the two values.
    # (This can be done only if the univar is free)
    , CanBeCastNo [UnivarId & Uniqueness]

#
# This answers the question "can I use `given` uniqueness when `required` is required?
#
uniCanBeCastTo as fn { given as Uniqueness, required as Uniqueness }: CanBeCast =
    fn ({ given, required }):

    try given & required as

        # Anything can be cast to Imm
        , _ & Imm:
            CanBeCastYes

        # Uni can be cast to anything
        , Uni & _:
            CanBeCastYes

        , Imm & Uni:
            CanBeCastNo []

        , Depends a & Uni:
            CanBeCastNo [a & Uni]

        , Depends a & Depends b:
            if a == b then
                CanBeCastYes
            else
                CanBeCastNo [a & Depends b, b & Depends a]

        , Imm & Depends b:
            CanBeCastNo [b & Imm]


# TODO rename mut and fix to given and required
checkUni as fn Env, Pos, { fix as Uniqueness, mut as Uniqueness }, @State: None =
    fn env, pos, ({ fix, mut }), @state:

    try uniCanBeCastTo { given = mut, required = fix } as
        , CanBeCastYes:
            None

        , CanBeCastNo []:
            addError env pos (ErrorUniquenessDoesNotMatch { fix, mut }) @state

        , CanBeCastNo ((univarId & uni) :: tail):
            addConstraint env pos univarId uni @state


addConstraint as fn Env, Pos, UnivarId, Uniqueness, @State: None =
    fn env, pos, id, uni, @state:

    eq as UnivarEquality =
        {
        , context = Context_Global # TODO
        , pos
        , why = "-"
        , id
        , uni
        }

    solveUniquenessConstraint env eq @state


[#
    We use this whenever we don't know the resulting Uni

    The function assumes that whatever annotations we have are correct, so it never adds constraints.

    Cases:

        1) { !x, 3?y } => 3?{...}

        2) { x, 3?y } => {...}

        Neither of this cases requires constraints

        The tricky one is:

        3) { 5?x, 3?y } => ???

    In this case... We should create a new univar and see
    what the resolved 5 and 3 are?


#]
inferUni as fn Uniqueness, Uniqueness: Uniqueness =
    fn a, b:

    try a & b as
        , Imm & _: Imm

        , _ & Imm: Imm

        , Depends aId & Depends bId:
            # TODO ----> if at least one is free, actually try to merge the two?
            Imm

        , _ & Depends _: b

        , _: Uni


#
#
# Definitions
#
#
doDefinition as fn (fn Name: Ref), Env, CA.ValueDef, @State: TA.ValueDef & Env =
    fn nameToRef, env, def, @state:

    { with uni } =
        def

#    log "DEf" def
#
#    (freeUnivars as Dict UnivarId TA.Univar) & (annotatedUnivarsByOriginalId as Dict UnivarId UnivarId) =
#        Dict.empty & env.annotatedUnivarsByOriginalId
#        >> Dict.for __ def.univars fn originalId, _, (fus & aus):
#            try Dict.get originalId env.annotatedUnivarsByOriginalId as
#
#                , Just _:
#                    fus & aus
#
#                , Nothing:
#                    univarId =
#                        newTyvarId @state
#
#                    univar as TA.Univar = { originalId }
#
#                    Dict.insert univarId univar fus & Dict.insert originalId univarId aus
#
#    (freeTyvars as Dict TA.TyvarId TA.Tyvar) & (annotatedTyvarsByName as Dict Name TA.TyvarId) =
#        Dict.empty & env.annotatedTyvarsByName
#        >> Dict.for __ def.tyvars fn tyvarName, caTyvar, (ftById & atByName):
#            try Dict.get tyvarName env.annotatedTyvarsByName as
#
#                , Just _:
#                    # TODO ensure that this definition is not adding flags to tyvars defined in the parent
#                    ftById & atByName
#
#                , Nothing:
#                    tyvarId =
#                        newTyvarId @state
#
#                    tyvar as TA.Tyvar =
#                        {
#                        #, annotatedAt = caTyvar.annotatedAt
#                        , allowFunctions = caTyvar.allowFunctions
#                        , originalName = tyvarName
#                        , generalizedAt = Pos.G
#                        , generalizedFor = RefLocal ""
#                        }
#
#                    Dict.insert tyvarId tyvar ftById & Dict.insert tyvarName tyvarId atByName
#
#
#    patternOut =
#        inferPattern { env with annotatedTyvarsByName, annotatedUnivarsByOriginalId } uni def.pattern @state



[#
    TODO inferPattern deve aggiornare annotatedTyvarsByName e annotatedUnivarsByOriginalId in env

    Come calcolo freeTyvars e freeUnivars?
    Per saperle devo risolvere le equazioni, ma devo saperle prima di usarle, perche' potrei doverle generalizzare!?

    ---> posso risolvere le equazioni man mano che le aggiungo?
    (Ovvio che si', ma quali sono le conseguenze?)
#]


    patternOut =
        inferPattern env uni def.pattern @state

    envWithContext =
        { patternOut.env with
        , context = Context_LetInBody (TA.patternNames patternOut.typedPattern >> Dict.keys)
        }

    typedBody & bodyType =
        if def.native then
            TA.LiteralText Pos.N "native" & { uni, raw = patternOut.patternType }

        else
            try patternOut.maybeFullAnnotation as
                , Just annotation:
                    todo "add free tyvars & free univars to env?"
                    raw = translateAnnotationType envWithContext @state annotation.raw
                    full = { uni, raw }
                    checkExpression envWithContext full def.body @state & full
                , Nothing:
                    typed & inferredType = inferExpression envWithContext def.body @state
                    pos = CA.patternPos def.pattern
                    addEquality envWithContext pos Why_LetIn patternOut.patternType inferredType.raw @state
                    checkUni envWithContext pos { fix = uni, mut = inferredType.uni } @state
                    typed & inferredType

    instance as fn Name, ({ pos as Pos, type as TA.FullType }): Instance =
        fn name, ({ pos, type }):
        {
        , definedAt = pos
        , type
        # TODO: remove tyvars and univars that do not appear in the type
        , freeTyvars = todo "freevars"
        , freeUnivars = todo "univars"
        }

    type =
        { bodyType with uni = def.uni }

    {
    , type
    , freeTyvars = todo "freevars"
    , freeUnivars = todo "freeUnivars"
    , pattern = patternOut.typedPattern
    , native = def.native
    , body = typedBody
    , directValueDeps = def.directValueDeps
    , isFullyAnnotated = patternOut.maybeFullAnnotation /= Nothing
    }
    &
    { patternOut.env with
    # Restore annotated tyvars, which were messed up in patternOut.env
    , annotatedTyvarsByName = env.annotatedTyvarsByName
    , annotatedUnivarsByOriginalId = env.annotatedUnivarsByOriginalId
    #
    , variables = .variables
        >> Dict.for __ (TA.patternNames patternOut.typedPattern) fn name, stuff, vars:
            Dict.insert (nameToRef name) (instance name stuff) vars
    }




#
#
# Expressions
#
#
inferExpression as fn Env, CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, caExpression, @state:

    try caExpression as

        , CA.LiteralNumber pos n:
            TA.LiteralNumber pos n & { uni = Uni, raw = coreTypeNumber }


        , CA.LiteralText pos text:
            TA.LiteralText pos text & { uni = Uni, raw = coreTypeText }


        , CA.Variable pos ref:

            ty =
                try getVariableByRef ref env as
                    , Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        fullTypeError

                    , Just var:
#                        log "VARIABLE" { var }
                        t = generalize env pos ref var @state
#                        log ("GEN---> " .. toHuman ref) { var, uni = t.uni, raw = typeToHuman env t.raw }
                        t


            TA.Variable pos ref & ty


        , CA.Constructor pos name args:
            inferUnion env pos name args @state


        , CA.Fn pos caPars body:
            inferFn env pos caPars body @state


        , CA.Call pos reference args:
            doCall env pos Nothing reference args @state


        , CA.Record pos maybeExt attrs:
            inferRecord env pos maybeExt attrs @state


        , CA.RecordAccess pos attrName recordExpression:
            typedExpr & inferredType =
                inferExpression env recordExpression @state

            TA.RecordAccess pos attrName typedExpr & { inferredType with raw = inferRecordAccess env pos attrName .raw @state }


        , CA.LetIn def rest:

            typedDef & defEnv =
                doDefinition RefLocal env def @state

            typedRest & restType =
                inferExpression defEnv rest @state

            TA.LetIn typedDef typedRest restType & restType


        , CA.If pos { condition, true, false }:

            typedCondition & conditionType =
                inferExpression { env with context = Context_IfCondition } condition @state

            addEquality env pos Why_IfCondition coreTypeBool conditionType.raw @state

            typedTrue & trueType =
                inferExpression { env with context = Context_IfTrue } true @state

            typedFalse & falseType =
                inferExpression { env with context = Context_IfFalse } false @state

            addEquality env pos Why_IfBranches trueType.raw falseType.raw @state

            expression =
                TA.If pos {
                    , condition = typedCondition
                    , true = typedTrue
                    , false = typedFalse
                    }

            # TODO What if "depends" tyvars get resolved to the same?
            # Shouldn't we test this only AFTER solving constraints?
            uni as Uniqueness =
                inferUni trueType.uni falseType.uni

            expression & { uni, raw = trueType.raw }


        , CA.Try pos { value, patternsAndExpressions }:
            doTry env pos (newRawType @state) value patternsAndExpressions @state


        , CA.DestroyIn name expression:
            typedExpression & expressionType =
                inferExpression env expression @state

            TA.DestroyIn name typedExpression & expressionType


doTry as fn Env, Pos, TA.RawType, CA.Expression, [Uniqueness & CA.Pattern & CA.Expression], @State: TA.Expression & TA.FullType =
    fn env, pos, expectedRaw, value, caPatternsAndExpressions, @state:

    typedValue & valueType =
        inferExpression env value @state

    uni & patternsAndExpressions =
        Uni & [] >> List.forReversed __ caPatternsAndExpressions fn (u & pa & exp), (uniX & acc):

            patternOut as PatternOut =
                inferPattern env u pa @state

            addEquality env pos Why_TryPattern patternOut.patternType valueType.raw @state
            checkUni env pos { fix = u, mut = valueType.uni } @state

            newEnv =
                { patternOut.env with
                , context = Context_TryBranch
                }

            typedExpression & expressionType =
                inferExpression newEnv exp @state

            addEquality newEnv (CA.expressionPos exp) Why_TryExpression expectedRaw expressionType.raw @state

            uf = inferUni uniX expressionType.uni
            l = (patternOut.typedPattern & typedExpression) :: acc

            uf & l

    TA.Try pos { value = typedValue, valueType, patternsAndExpressions } & { uni, raw = expectedRaw }


inferParam as fn Env, Int, CA.Parameter, @State: TA.Parameter & TA.ParType & Env =
    fn env, parIndex, par, @state:

    # TODO parIndex is not used

    try par as
        , CA.ParameterRecycle pos name:
            # TODO check name already in env? Is it MakeCanonical resp?
            raw =
                TA.TypeVar (newTyvarId @state)

            instance as Instance =
                {
                , definedAt = pos
                , type = { raw, uni = Uni }
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                }

            newEnv as Env =
                { env with variables = Dict.insert (RefLocal name) instance .variables }

            TA.ParameterRecycle pos raw name & TA.ParRe raw & newEnv


        , CA.ParameterPattern uni pa:
            out =
                inferPattern env uni pa @state

            full =
                { raw = out.patternType, uni }

            TA.ParameterPattern full out.typedPattern & TA.ParSp full & out.env


        , CA.ParameterPlaceholder name num:
            univarId =
                newTyvarId @state

            raw =
                newRawType @state

            type =
                { raw, uni = Depends univarId }

            instance as Instance =
                {
                , definedAt = Pos.G
                , type
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                }

            newEnv as Env =
                { env with variables = Dict.insert (RefLocal name) instance .variables } 

            pa as TA.Pattern =
                TA.PatternAny Pos.G { maybeName = Just name, type }

            TA.ParameterPattern type pa & TA.ParSp type & newEnv


inferFn as fn Env, Pos, [CA.Parameter], CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, pos, caPars, body, @state:

    [# TODO
        Urgh. This is gross.
        Do I really want to enable mixing imm and mut this way?
        Is there really an advantage?
    #]
    !typedPars = Array.fromList []
    !parTypes = Array.fromList []

    newEnv as Env =
        List.indexedFor env caPars fn index, par, envX:
            typedPar & parType & envX1 =
                inferParam envX index par @state

            log "PAR" { typedPar , parType  }

            Array.push @typedPars typedPar
            Array.push @parTypes parType
            envX1

    typedBody & bodyType =
        inferExpression { newEnv with context = Context_FnBody pos env.context } body @state

    type as TA.RawType =
        TA.TypeFn (Array.toList @parTypes) bodyType

    exp =
        TA.Fn pos (Array.toList @typedPars) typedBody bodyType

    exp & { uni = Uni, raw = type }



inferRecordAccess as fn Env, Pos, Name, TA.RawType, @State: TA.RawType =
    fn env, pos, attrName, inferredType, @state:

    try inferredType as
        , TA.TypeRecord Nothing attrTypes:
            try Dict.get attrName attrTypes as
                , Just type:
                    type

                , Nothing:
                    addError env pos (ErrorRecordDoesNotHaveAttribute attrName) @state
                    TA.TypeError

        , TA.TypeRecord (Just tyvarId) extensionAttrTypes:
            try Dict.get attrName extensionAttrTypes as
                , Just type:
                    type

                , Nothing:
                    newExtId = newTyvarId @state

                    newAttrType =
                        newRawType @state

                    type =
                        TA.TypeRecord (Just newExtId) (Dict.insert attrName newAttrType extensionAttrTypes)

                    addEquality env pos Why_RecordAccess (TA.TypeVar tyvarId) type @state

                    newAttrType


        , TA.TypeVar id:
            newExtId = newTyvarId @state

            # Attrs have always the same default uni as the record
            newAttrType = TA.TypeVar (newTyvarId @state)

            type as TA.RawType =
                TA.TypeRecord (Just newExtId) (Dict.ofOne attrName newAttrType)

            addEquality env pos Why_RecordAccess inferredType type @state

            newAttrType

        , _:
            addError env pos (ErrorTryingToAccessAttributeOfNonRecord attrName inferredType) @state
            TA.TypeError


inferUnion as fn Env, Pos, Name, [CA.Expression], @State: TA.Expression & TA.FullType =
    fn env, pos, consName, caArgs, @state:

    taArgs as [TA.Expression & TA.FullType] =
        List.map (inferExpression { env with context = Context_Argument consName .context } __ @state) caArgs

    typedArgs as [TA.Expression] =
        List.map Tuple.first taArgs

    argTypes as [TA.RawType] =
        List.map (fn e & f: f.raw) taArgs

    id =
        newTyvarId @state

    TA.Constructor pos consName typedArgs & { uni = Uni, raw = TA.TypeUnion (Just id) (Dict.ofOne consName argTypes) }



inferRecord as fn Env, Pos, Maybe CA.Expression, Dict Name CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, pos, maybeExt, caAttrs, @state:

    taAttrs as Dict Name (TA.Expression & TA.FullType) =
        Dict.map (fn name, value: inferExpression { env with context = Context_Argument name .context } value @state) caAttrs

    typedAttrs as Dict Name TA.Expression =
        Dict.map (fn k, v: Tuple.first v) taAttrs

    attrTypes as Dict Name TA.RawType =
        Dict.map (fn k, (_ & t): t.raw) taAttrs

    uni as Uniqueness =
        Uni >> Dict.for __ taAttrs fn k, (_ & full), u:
            inferUni full.uni u

    try maybeExt as
        , Nothing:
            TA.Record pos Nothing typedAttrs & { uni, raw = TA.TypeRecord Nothing attrTypes }

        , Just caExt:

            typedExt & extType =
                inferExpression env caExt @state

            finalType as TA.RawType =
                try extType.raw as
                    , TA.TypeRecord Nothing fixedTypes:
                        Dict.each attrTypes fn name, valueType:
                            try Dict.get name fixedTypes as
                                , Nothing: addError env pos (ErrorRecordDoesNotHaveAttribute name) @state
                                , Just ty: addEquality env pos Why_Record ty valueType @state

                        extType.raw


                    , TA.TypeRecord (Just tyvarId) extensionAttrTypes:

                        expressionOnly & both & extensionOnly =
                            Dict.onlyBothOnly attrTypes extensionAttrTypes

                        Dict.each both fn name, (inAttr & extAttr):
                            addEquality env pos Why_Record inAttr extAttr @state

                        # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
                        newExtId =
                            newTyvarId @state

                        TA.TypeRecord (Just newExtId) (Dict.join attrTypes extensionOnly)


                    , TA.TypeVar id:
                        ty =
                            TA.TypeRecord (Just << newTyvarId @state) attrTypes

                        addEquality env pos Why_RecordExt extType.raw ty @state

                        ty


                    , _:
                        addError env pos ErrorNotCompatibleWithRecord @state
                        TA.TypeError

            TA.Record pos (Just typedExt) typedAttrs & { uni = inferUni uni extType.uni, raw = finalType }



#
# Check
#


checkParameter as fn Env, TA.ParType, CA.Parameter, @State: TA.Parameter & Env =
    fn env, expectedParType, par, @state:

    try par as

        , CA.ParameterPattern originalUni pa:

            fullType & (typedPa & env1) =
                try expectedParType as
                    , TA.ParRe _:
                        addError env (CA.patternPos pa) ErrorRecyclingDoesNotMatch @state
                        o = inferPattern env Uni pa @state
                        { uni = Uni, raw = o.patternType } & (o.typedPattern & o.env)

                    , TA.ParSp full:
                        uni = translateUni env.annotatedUnivarsByOriginalId originalUni
                        addErrorIf (uni /= full.uni) env (CA.patternPos pa) (ErrorUniquenessDoesNotMatchParameter uni full) @state
                        full & checkPattern env full pa @state

            TA.ParameterPattern fullType typedPa & env1


        , CA.ParameterPlaceholder name num:
            try expectedParType as
                , TA.ParRe _: todo "TA.ParRe"
                , TA.ParSp type:

                    pa as TA.Pattern =
                        TA.PatternAny Pos.G { maybeName = Just name, type }

                    variable as Instance =
                        {
                        , definedAt = Pos.G
                        , type
                        , freeTyvars = Dict.empty
                        , freeUnivars = Dict.empty
                        }

                    TA.ParameterPattern type pa & { env with variables = Dict.insert (RefLocal name) variable .variables }


        , CA.ParameterRecycle pos name:
            expectedRaw =
                try expectedParType as
                    , TA.ParSp full:
                        addError env pos ErrorRecyclingDoesNotMatch @state
                        TA.TypeError

                    , TA.ParRe raw:
                        raw

            variable as Instance =
                {
                , definedAt = pos
                , type = { raw = expectedRaw, uni = Uni }
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                }

            localEnv as Env =
                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with variables = Dict.insert (RefLocal name) variable .variables }

            TA.ParameterRecycle pos expectedRaw name & localEnv


checkExpression as fn Env, TA.FullType, CA.Expression, @State: TA.Expression =
    fn env, expectedType, caExpression, @state:

    try caExpression & expectedType.raw as

        , CA.LiteralNumber pos n & TA.TypeOpaque typeUsr []:
            addErrorIf (typeUsr /= CoreTypes.numberUsr)
                env pos (ErrorIncompatibleTypes caExpression expectedType) @state

            TA.LiteralNumber pos n


        , CA.LiteralText pos text & TA.TypeOpaque typeUsr []:
            addErrorIf (typeUsr /= CoreTypes.textUsr)
                env pos (ErrorIncompatibleTypes caExpression expectedType) @state

            TA.LiteralText pos text


        , CA.Variable pos ref & _:

            __bleh__ =
                try getVariableByRef ref env as
                    , Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state

                    , Just var:
                        full = generalize env pos ref var @state
                        checkUni env pos { fix = expectedType.uni, mut = full.uni } @state
                        addEquality env pos Why_Annotation full.raw expectedType.raw @state

            TA.Variable pos ref


        , CA.Constructor pos name actualArgs & TA.TypeUnion Nothing consByName:

            try Dict.get name consByName as
                , Nothing:
                    addError env pos (ErrorConstructorNotInType name) @state
                    TA.Error pos

                , Just expectedArgTypes:

                    if List.length expectedArgTypes /= List.length actualArgs then
                        addError env pos (ErrorWrongNumberOfConstructorArguments) @state
                        TA.Error pos
                    else
                        fullExpectedArgTypes =
                            List.map (fn raw: { raw, uni = expectedType.uni }) expectedArgTypes

                        typedArgs =
                            List.map2 (checkExpression env __ __ @state) fullExpectedArgTypes actualArgs

                        TA.Constructor pos name typedArgs


        , CA.Fn pos pars body & TA.TypeFn parTypes out:

            if List.length pars /= List.length parTypes then
                addError env pos ErrorWrongNumberOfParameters @state
                TA.Error pos
            else
                !typedPars = Array.fromList []
                !parIndex = 0

                localEnv as Env =
                    env
                    >> List.for __ (List.map2 Tuple.pair pars parTypes) fn (par & parType), envX:

                        typedPar & envX1 =
                            checkParameter { envX with context = Context_FnPar (cloneUni @parIndex) .context } parType par @state

                        Array.push @typedPars typedPar
                        @parIndex += 1

                        # TODO uglyyy
                        { envX1 with context = envX.context }

                typedBody =
                    checkExpression localEnv out body @state

                TA.Fn pos (Array.toList @typedPars) typedBody out


        , CA.Call pos reference args & _:
            doCall env pos (Just expectedType) reference args @state
            >> Tuple.first


        , CA.Record pos (Just ext) valueByName & TA.TypeRecord Nothing typeByName:

            # ext must have type expectedType
            # TODO: add context
            typedExt =
                checkExpression env expectedType ext @state

            zzz =
                fn attrName, attrExpr:
                    try Dict.get attrName typeByName as
                        , Nothing:
                            addError env pos ErrorRecordHasAttributesNotInAnnotation @state
                            # This is not super clean, but since it's an error condition, it's probably fine
                            Tuple.first (inferExpression env attrExpr @state)
                        , Just attrType:
                            fullAttrType = { uni = expectedType.uni, raw = attrType }
                            checkExpression { env with context = Context_AttributeName attrName .context } fullAttrType attrExpr @state


            # all valueByName attrs must be in typeByName
            typedValueByName =
                Dict.map zzz valueByName

            TA.Record pos (Just typedExt) typedValueByName


        , CA.Record pos Nothing valueByName & TA.TypeRecord Nothing typeByName:

            aOnly & both & bOnly =
                Dict.onlyBothOnly valueByName typeByName

            if aOnly /= Dict.empty then
                addError env pos ErrorRecordHasAttributesNotInAnnotation @state

            else if bOnly /= Dict.empty then
                addError env pos ErrorRecordIsMissingAttibutesInAnnotation @state
            else
                None

            typedAttrs =
                # TODO add attribute name to env!?
                both >> Dict.map (fn name, (value & type): checkExpression env { uni = expectedType.uni, raw = type } value @state) __

            TA.Record pos Nothing typedAttrs


        , CA.RecordAccess pos attrName exp & _:

            typedExpression & expressionType =
                inferExpression env exp @state

            newId =
                newTyvarId @state

            requiredType =
                expectedType.raw
                >> Dict.ofOne attrName __
                >> TA.TypeRecord (Just newId) __

            addEquality env pos Why_RecordAccess expressionType.raw requiredType @state
            checkUni env pos { fix = expectedType.uni, mut = expressionType.uni } @state

            TA.RecordAccess pos attrName typedExpression


        , CA.LetIn def rest & _:

            typedDef & defEnv =
                doDefinition RefLocal env def @state

            typedRest =
                checkExpression defEnv expectedType rest @state

            TA.LetIn typedDef typedRest expectedType


        , CA.If pos { condition, true, false } & _:

            typedCondition & conditionType =
                inferExpression { env with context = Context_IfCondition } condition @state

            addEquality env pos Why_IfCondition coreTypeBool conditionType.raw @state

            typedTrue =
                checkExpression { env with context = Context_IfTrue } expectedType true @state

            typedFalse =
                checkExpression { env with context = Context_IfFalse } expectedType false @state

            TA.If pos {
                , condition = typedCondition
                , true = typedTrue
                , false = typedFalse
                }


        , CA.Try pos { value, patternsAndExpressions } & _:
            typedExp & fullType =
                doTry env pos expectedType.raw value patternsAndExpressions @state

            checkUni env pos { fix = expectedType.uni, mut = fullType.uni } @state

            typedExp


        , CA.DestroyIn name exp & _:
            TA.DestroyIn name  __ << checkExpression env expectedType exp @state


        , _ & TA.TypeError:
            TA.Error (CA.expressionPos caExpression)


        , _:
            pos = CA.expressionPos caExpression
            addError env pos (ErrorIncompatibleTypes caExpression expectedType) @state
            TA.Error pos


doCall as fn Env, Pos, Maybe TA.FullType, CA.Expression, [CA.Argument], @State: TA.Expression & TA.FullType =
    fn env, pos, maybeExpectedType, reference, givenArgs, @state:

    # `reference givenArg1 givenArg2 ...` must be of `expectedType`

    typedReference & inferredReferenceType =
        inferExpression env reference @state

    typedArguments as [TA.Argument] =
        givenArgs >> List.map (fn arg: inferArgument env arg @state) __

    toTypeArg as fn TA.Argument: TA.ParType =
        fn arg:
        try arg as
            , TA.ArgumentExpression full _: TA.ParSp full
            , TA.ArgumentRecycle _ raw _ _: TA.ParRe raw

    expectedReturnType =
        try inferredReferenceType.raw as

            , TA.TypeFn parTypes outType:
                given = List.length typedArguments
                expected = List.length parTypes
                if expected /= given then
                    addError env pos (ErrorWrongNumberOfArguments { reference, given, expected }) @state
                    fullTypeError
                else
                    List.indexedEach2 typedArguments parTypes fn index, givenArg, parType:
                        try givenArg & parType as
                            , TA.ArgumentRecycle p givenRaw attrPath name & TA.ParRe inferredRaw:
                                try getVariableByRef (RefLocal name) env as
                                    , Nothing:
                                        addError env p (ErrorVariableNotFound (RefLocal name)) @state
                                    , Just instance:
                                        addErrorIf (instance.type.uni /= Uni) env p ErrorShouldBeUnique @state
                                        addEquality env pos (Why_Argument index) givenRaw inferredRaw @state

                            , TA.ArgumentExpression givenFull expr & TA.ParSp inferredFull:
                                checkUni env pos { fix = inferredFull.uni, mut = givenFull.uni } @state
                                # TODO The order [inferredFull.raw, givenFull.raw] is important if the raw contains a function!!!
                                # This is **SUPER** brittle (there are probably bugs caused by the inversion of the two comparison terms at some point...)
                                addEquality env pos (Why_Argument index) inferredFull.raw givenFull.raw @state

                            , _:
                                addError env pos (ErrorUniquenessDoesNotMatchArgument) @state

                    try maybeExpectedType as
                        , Nothing: outType
                        , Just e:
                            checkUni env pos { fix = e.uni, mut = outType.uni } @state
                            addEquality env pos Why_Annotation outType.raw e.raw @state
                            e

            , TA.TypeVar id:
                returnType =
                    try maybeExpectedType as
                        , Just e:
                            e

                        , Nothing:
                            # TODO: `Imm` here is completely arbitrary
                            { uni = Imm, raw = newRawType @state }

                refTy = TA.TypeFn (List.map toTypeArg typedArguments) returnType
                addEquality env pos Why_CalledAsFunction refTy inferredReferenceType.raw @state
                returnType

            , TA.TypeError:
                fullTypeError

            , z:
                addError env pos (ErrorCallingANonFunction z) @state
                fullTypeError

    TA.Call pos typedReference typedArguments & expectedReturnType


inferArgument as fn Env, CA.Argument, @State: TA.Argument =
    fn env, arg, @state:

    try arg as
        , CA.ArgumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            TA.ArgumentExpression expType typedExp

        , CA.ArgumentRecycle pos name attrPath:
            ref =
                RefLocal name

            raw =
                try getVariableByRef ref env as

                    , Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        TA.TypeError

                    , Just var:
                        var.type.raw >> List.for __ attrPath fn attrName, tyAcc:
                            inferRecordAccess env pos attrName tyAcc @state

            TA.ArgumentRecycle pos raw attrPath name


#
#
# Patterns
#
#
alias PatternOut =
    {
    , patternType as TA.RawType
    , typedPattern as TA.Pattern
    , maybeFullAnnotation as Maybe CA.Annotation

    # TODO if it is given a context where the pattern is, then this env will contain that context and it's kind of bad?
    # Maybe the function should be given the context as its own param?
    , env as Env
    }


inferPattern as fn Env, Uniqueness, CA.Pattern, @State: PatternOut =
    fn env, uni, pattern, @state:

    try pattern as
        , CA.PatternAny pos maybeName maybeAnnotation:
            inferPatternAny env pos uni maybeName maybeAnnotation @state


        , CA.PatternLiteralText pos text:
            {
            , typedPattern = TA.PatternLiteralText pos text
            , patternType = coreTypeText
            , env
            , maybeFullAnnotation = Nothing
            }


        , CA.PatternLiteralNumber pos n:
            {
            , typedPattern = TA.PatternLiteralNumber pos n
            , patternType = coreTypeNumber
            , env
            , maybeFullAnnotation = Nothing
            }


        , CA.PatternConstructor pos name arguments:

            argumentOuts & newEnv =
                [] & env >> List.forReversed __ arguments fn arg, (argOuts & envX):

                    out =
                        inferPattern envX uni arg @state

                    (out :: argOuts) & out.env

            typedArguments =
                List.map (fn out: out.typedPattern) argumentOuts

            argumentTypes =
                List.map (fn out: out.patternType) argumentOuts

            finalType =
                TA.TypeUnion (Just (newTyvarId @state)) (Dict.ofOne name argumentTypes)

            {
            , typedPattern = TA.PatternConstructor pos name typedArguments
            , patternType = finalType
            , env = newEnv
            , maybeFullAnnotation = Nothing #TODO
            }


        , CA.PatternRecord pos completeness pas:

            outs & newEnv =
                Dict.empty & env >> Dict.for __ pas fn name, pa, (dict & envX):

                    out = inferPattern envX uni pa @state

                    Dict.insert name out dict & out.env

            patternExt as Maybe TA.TyvarId =
                try completeness as
                    , CA.Complete: Nothing
                    , CA.Partial: Just (newTyvarId @state)

            raw =
                TA.TypeRecord patternExt (outs >> Dict.map (fn name, out: out.patternType) __)

            {
            , typedPattern = TA.PatternRecord pos (outs >> Dict.map (fn k, o: o.typedPattern & o.patternType) __)
            , patternType = raw
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }



inferPatternAny as fn Env, Pos, Uniqueness, Maybe Name, Maybe CA.Annotation, @State: PatternOut =
    fn env, pos, uni, maybeName, maybeAnnotation, @state:

    raw as TA.RawType =
        try maybeAnnotation as
            , Nothing:
                newRawType @state

            , Just annotation:
                # TODO should we test annotation uni vs the pattern uni (which is not available in this fn?)
                todo "add annotation vars/unis to env"
                translateAnnotationType env @state annotation.raw

    type as TA.FullType =
        { raw, uni }

    envWithVariable as Env =
        try maybeName as
            , Nothing:
                env

            , Just name:
                variable as Instance =
                    {
                    , definedAt = pos
                    , type
                    , freeTyvars = Dict.empty
                    , freeUnivars = Dict.empty
                    }

                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with
                , variables = Dict.insert (RefLocal name) variable .variables
                }

    typedPattern =
        TA.PatternAny pos { maybeName, type }

    {
    , typedPattern
    , patternType = raw
    , env = envWithVariable
    , maybeFullAnnotation = maybeAnnotation
    }


checkPattern as fn Env, TA.FullType, CA.Pattern, @State: TA.Pattern & Env =
    fn env, expectedType, pattern, @state:

    try pattern & expectedType.raw as
        , CA.PatternAny pos maybeName maybeAnnotation & _:

            newEnv as Env =
                try maybeName as
                    , Nothing:
                        env

                    , Just name:
                        variable as Instance =
                            {
                            , definedAt = pos
                            , type = expectedType
                            , freeTyvars = Dict.empty
                            , freeUnivars = Dict.empty
                            }

                        # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                        { env with
                        , variables = Dict.insert (RefLocal name) variable .variables
                        }

            TA.PatternAny pos { maybeName, type = expectedType } & newEnv


        , CA.PatternLiteralText pos text & TA.TypeOpaque typeUsr []:
            addErrorIf (typeUsr /= CoreTypes.textUsr) env pos (ErrorIncompatiblePattern pattern expectedType) @state

            TA.PatternLiteralText pos text & env


        , CA.PatternLiteralNumber pos text & TA.TypeOpaque typeUsr []:
            addErrorIf (typeUsr /= CoreTypes.numberUsr) env pos (ErrorIncompatiblePattern pattern expectedType) @state

            TA.PatternLiteralNumber pos text & env


        , CA.PatternConstructor pos name arguments & _:
            checkPatternConstructor env pos expectedType name arguments @state


        , CA.PatternRecord pos completeness pas & _:
            checkPatternRecord env pos expectedType completeness pas @state



checkPatternRecord as fn Env, Pos, TA.FullType, CA.PatternCompleteness, Dict Name CA.Pattern, @State: TA.Pattern & Env =
    fn env, pos, expectedType, completeness, pas, @state:

    { with uni } =
        expectedType

    try expectedType.raw as
        , TA.TypeRecord Nothing attrs:

            paOnly & both & typeOnly =
                Dict.onlyBothOnly pas attrs

            addErrorIf (paOnly /= Dict.empty) env pos
                (ErrorRecordHasAttributesNotInAnnotation)
                @state

            addErrorIf (typeOnly /= Dict.empty and completeness == CA.Complete) env pos
                # TODO "add `with` if you don't want to use all attrs"
                (ErrorRecordIsMissingAttibutesInAnnotation)
                @state

            taPas & envF =
                Dict.empty & env
                >> Dict.for __ both fn name, (pa & raw), (acc & envX):
                    taPa & envX0 =
                        checkPattern { envX with context = Context_AttributeName name env.context } { uni, raw } pa @state

                    Dict.insert name (taPa & raw) acc & { envX0 with context = env.context }

            TA.PatternRecord pos taPas & envF


        , TA.TypeRecord (Just tyvarId) a:
            bug "can't annotate extensible types"

        , _:
            addError env pos ErrorNotCompatibleWithRecord @state

            envF =
                env >> Dict.for __ pas fn name, pa, envX:
                    out = inferPattern envX expectedType.uni pa @state
                    out.env

            patternError pos & envF



checkPatternConstructor as fn Env, Pos, TA.FullType, Name, [CA.Pattern], @State: TA.Pattern & Env =
    fn env, pos, expectedType, name, arguments, @state:

    insertArgsOnError as fn Env: Env =
        List.for __ arguments fn arg, envX:
            out = inferPattern envX expectedType.uni arg @state
            out.env


    try expectedType.raw as

        , TA.TypeUnion maybeExt constructorsByName:
            try Dict.get name constructorsByName as

                , Nothing:
                    addError env pos (ErrorConstructorNotInType name) @state
                    patternError pos & insertArgsOnError env

                , Just expectedArgTypes:

                    if List.length arguments /= List.length expectedArgTypes then
                        addError env pos ErrorWrongNumberOfConstructorArguments @state
                        patternError pos & insertArgsOnError env

                    else
                        (newEnv as Env) & (typedArgs as [TA.Pattern]) =
                            List.forReversed2 (env & []) arguments expectedArgTypes fn actualArgument, expectedArgType, envX & args:

                                fullType as TA.FullType =
                                    {
                                    , uni = expectedType.uni
                                    , raw = expectedArgType
                                    }

                                taArg & envX1 =
                                    checkPattern envX fullType actualArgument @state

                                envX1 & [taArg, ...args]


                        # TODO check that args uni are the same or castable as expectedType.uni

                        TA.PatternConstructor pos name typedArgs & newEnv


        , _:
            addError env pos ErrorNotCompatibleWithUnion @state
            patternError pos & insertArgsOnError env



#
#
# Module
#
#
insertAnnotatedAndNonAnnotated as fn CA.Pattern, CA.ValueDef, [CA.ValueDef] & [CA.ValueDef]: [CA.ValueDef] & [CA.ValueDef] =
    fn pa, def, (ann & nonAnn):

    isFullyAnnotated =
        pa
        >> CA.patternNames
        >> Dict.values
        >> List.all (fn stuff: stuff.maybeAnnotation /= Nothing) __

    if isFullyAnnotated then
        (def :: ann) & nonAnn
    else
        ann & (def :: nonAnn)



doRootDefinition as fn @Int, @Array Error, CA.Module, Env, CA.ValueDef: TA.ValueDef & Env =
    fn @lastUnificationVarId, @errors, module, env, def:

    !state as State =
        initState (cloneUni @lastUnificationVarId)

    nameToRef as fn Name: Ref =
        fn name:
        RefGlobal (USR module.umr name)

    Debug.benchStart None

    typedDef & envF =
        doDefinition nameToRef env def @state

    Debug.benchStop "type inference"

#    Debug.benchStart None
#
#    erState0 as ERState =
#        {
#        , equalities = Array.toList @state.equalities
#        , univarEqualities = Array.toList @state.univarEqualities
#        , errors = []
#        , substitutions = Dict.empty
#        , lastUnificationVarId = cloneUni @state.lastUnificationVarId
#        }
#
#    erEnv as EREnv = {
#        , expandedAliases = env.expandedAliases
#        }
#
#    # Solve equality constraints (ie, tyvars)
#    erStateF =
#        erState0
#        >> solveEqualities erEnv __
#        >> fn st: List.for st st.equalities fn eq, s: addErError env eq "unresolved" s
#
#    Debug.benchStop "equalities resolution"

#    List.each (Dict.toList erStateF.substitutions) (id & t):
#        log ("SUB for " .. Text.fromNumber id) t

#    Debug.benchStart None
#
#    # Solve uniqueness constraints (ie, univars)
#    univarSubs as Dict UnivarId Uniqueness =
#        solveUniquenessConstraints env erStateF.univarEqualities @state Dict.empty
#
#    Debug.benchStop "uniqueness resolution"

#    Hash.each @state.tyvarsById fn tyvarId, tyvar:
#        if tyvar.allowFunctions then
#            None
#        else
#            try Dict.get tyvarId erStateF.substitutions as
#                , Nothing:
#                    None
#
#                , Just taType:
#                    testId as fn TA.TyvarId: Bool =
#                        fn id: True
#
#                    # TODO -----> addErrorIf (TA.typeAllowsFunctions testId taType) env tyvar.generalizedAt (ErrorTypeAllowsFunctions taType tyvar caModule.umr) @state
#                    None

    Debug.benchStart None

    subsAsFns as TA.SubsAsFns =
        {
        , ty = fn tyvarId: Hash.get @state.tyvarSubs tyvarId
        , uni = fn univarId: Hash.get @state.univarSubs univarId
        }

    resolvedValueDef as TA.ValueDef =
        TA.resolveValueDef subsAsFns typedDef

    Debug.benchStop "def resolution"

    # Update lastUnificationVarId!!
    # TODO we can make this safer once we have a 'reassign' op?
    @lastUnificationVarId := cloneUni @state.lastUnificationVarId

#    # Add errors
#    Array.each @state.errors fn err:
#        Array.push @errors (makeInferenceAndCheckError env err)
#
#    List.each erStateF.errors fn err:
#        Array.push @errors (makeResolutionError env module err)

    resolvedValueDef & envF



doModule as fn @Int, Env, CA.Module: Res TA.Module =
    fn @lastUnificationVarId, globalEnv, caModule:

    env as Env =
        { globalEnv with errorModule = { fsPath = caModule.fsPath, content = caModule.asText } }

    annotated & nonAnnotated =
        Dict.for ([] & []) caModule.valueDefs insertAnnotatedAndNonAnnotated

    if List.length nonAnnotated > 1 then
        bug "Right now the compiler supports only one root-level non-annotated value per module. =("
    else
        None

    [# TODO I can restore this once I remove the patterns from CanonicalAST

    nonAnnotatedBy??? as Dict CA.Pattern CA.ValueDef =
        Dict.empty >> List.for nonAnnotated caDef: Dict.insert caDef.pattern caDef

    circulars & orderedNonAnnotated =
        RefHierarchy.reorder (d: d.directValueDeps) nonAnnotatedById

    List.each circulars c:
        # TODO test this error. Is it "circular" or "recursive"?
        addError env (Pos.M "TODO get module path") (ErrorCircularValue c) @state

    allOrdered =
        List.concat [ orderedNonAnnotated, annotated ]
    #]

    allOrdered =
        List.concat [ nonAnnotated, annotated ]

    !errors as Array Error =
        Array.fromList []

    (valueDefs as Dict CA.Pattern TA.ValueDef) & (envF as Env) =
        Dict.empty & env
        >> List.for __ allOrdered fn def, (accum & env0):
              typedDef & env1 =
                  doRootDefinition @lastUnificationVarId @errors caModule env0 def

              Dict.insert def.pattern typedDef accum & env1

    #
    # packaging & closing
    #
    typedModule as TA.Module =
        {
        , fsPath = caModule.fsPath
        , umr = caModule.umr
        , asText = caModule.asText
        , valueDefs
        }

    errs as [Error] =
        Array.toList @errors

    if errs == [] then
        Ok typedModule

    else
        errs
        >> Error.Nested
        >> Err


#
#
# Populate global Env
#
#
addCoreValueToCoreEnv as fn @State, Prelude.CoreValue, Env: Env =
    fn @state, { usr, raw, nonFn }, env:

    USR umr name =
        usr

    zzz =
        fn tyvarName, pos: { nonFn = if Dict.member tyvarName nonFn then Just Pos.N else Nothing }

    tyvars =
        raw
        >> CA.typeTyvars
        >> Dict.map zzz __

    univars =
        CA.typeUnivars raw

    addValueToGlobalEnv @state umr
        {
        , uni = Imm
        , pattern = CA.PatternAny Pos.N (Just name) (Just { raw, tyvars, univars })
        , native = True
        , body = CA.LiteralText Pos.N name
        , directTypeDeps = Dict.empty
        , directValueDeps = Dict.empty
        }
        env


addValueToGlobalEnv as fn @State, UMR, CA.ValueDef, Env: Env =
    fn @state, umr, def, env:

    #
    # Tyvars
    #
    nameToIdAndClasses as Dict Name (TA.TyvarId & TA.Tyvar) =
        zzzz =
            fn name, ({ allowFunctions }):
            newTyvarId @state & {
                , originalName = name
                #, annotatedAt
                , generalizedAt = Pos.G
                , generalizedFor = RefLocal ""
                , allowFunctions
                }

        Dict.map zzzz (todo "def.tyvars")

    nameToType as Dict Name TA.RawType =
        Dict.map (fn k, (id & classes): TA.TypeVar id) nameToIdAndClasses

    tyvarIdToClasses as Dict TA.TyvarId TA.Tyvar =
        nameToIdAndClasses
        >> Dict.values
        >> Dict.fromList

    #
    # Univars
    #
    originalIdToNewIdAndUnivar as Dict UnivarId (UnivarId & TA.Univar) =
        todo "def.univars"
        >> Dict.map (fn originalId, None: newTyvarId @state & { originalId }) __

    originalIdToUniqueness as Dict UnivarId UnivarId =
        originalIdToNewIdAndUnivar
        >> Dict.map (fn originalId, (newId & univar): newId) __

    freeUnivars as Dict UnivarId TA.Univar =
        originalIdToNewIdAndUnivar
        >> Dict.values
        >> Dict.fromList

    #
    # Env
    #
    Dict.for env (CA.patternNames def.pattern) fn valueName, valueStuff, envX:
        try valueStuff.maybeAnnotation as
            , Nothing:
                envX

            , Just annotation:

                raw =
                    translateRawType env nameToType originalIdToUniqueness @state annotation

                instance as Instance =
                    {
                    , definedAt = valueStuff.pos
                    , type = { raw, uni = Imm }
                    , freeTyvars = Dict.intersect tyvarIdToClasses (TA.typeTyvars raw)

                    # TODO should intersect with the univars actually used by the specific variable
                    , freeUnivars
                    }

                ref as Ref =
                    valueName
                    >> USR umr __
                    >> RefGlobal

                { envX with variables = Dict.insert ref instance .variables }


namedParsToIdParsAndDict as fn [At Name]: [TA.TyvarId] & Dict Name TA.RawType =
    fn atPars:

    idPars =
        atPars >> List.indexedMap (fn index, atName: -index) __

    typeByName =
        atPars
        >> List.indexedMap (fn index, (At pos name): name & TA.TypeVar -index) __
        >> Dict.fromList

    idPars & typeByName


expandAndInsertAlias as fn @State, ByUsr CA.AliasDef, USR, ByUsr ExpandedAlias: ByUsr ExpandedAlias =
    fn @state, allAliases, usr, aliasAccum:

    aliasDef =
        try Dict.get usr allAliases as
            , Just def: def
            , Nothing: bug "alias not found"

    pars & typeByName =
        namedParsToIdParsAndDict aliasDef.pars

    originalIdToNewId as Dict UnivarId UnivarId =
        # TODO ----> We should probably do something with these
        Dict.empty

    type as TA.RawType =
        translateRawType { initEnv with expandedAliases = aliasAccum } typeByName originalIdToNewId @state aliasDef.type

    Dict.insert usr { pars, type } aliasAccum



makeTypeRecursive as fn Bool, USR, CA.RawType: Result {} CA.RawType =
    fn allowRecursion, required, requiringType:

    rec as fn CA.RawType: Result {} CA.RawType =
        makeTypeRecursive allowRecursion required __

    recAllow as fn CA.RawType: Result {} CA.RawType =
        makeTypeRecursive True required __

    try requiringType as
        , CA.TypeNamed pos usr args:

            List.mapRes rec args
            >> onOk fn fixedArgs:

            if required /= usr then
                CA.TypeNamed pos usr fixedArgs
                >> Ok

            else if allowRecursion then
                CA.TypeRecursive pos required fixedArgs
                >> Ok

            else
                Err {}

        , CA.TypeUnion pos consByName:
            consByName
            >> Dict.mapRes (fn name, cons: List.mapRes recAllow cons) __
            >> Result.map (CA.TypeUnion pos __) __

        , CA.TypeAnnotationVariable pos name:
            Ok requiringType

        , CA.TypeRecord pos attrs:
            attrs
            >> Dict.mapRes (fn k, v: rec v) __
            >> Result.map (CA.TypeRecord pos __) __

        , CA.TypeFn pos ins out:
            mapPar =
                fn par:
                try par as
                    , CA.ParRe raw: Result.map CA.ParRe (rec raw)
                    , CA.ParSp full: Result.map (fn raw: CA.ParSp { full with raw }) (rec full.raw)

            List.mapRes mapPar ins
            >> onOk fn fixedIns:

            rec out.raw
            >> onOk fn fixedOutRaw:

            CA.TypeFn pos fixedIns { out with raw = fixedOutRaw }
            >> Ok



toFixedType as fn ByUsr CA.AliasDef, USR & USR: Maybe (ByUsr CA.AliasDef) =
    fn aliases, required & requiring:

    requiringDef =
        try Dict.get requiring aliases as
            , Just a: a
            , Nothing: bug "no requiring alias =("

    try makeTypeRecursive False required requiringDef.type as
        , Ok fixedType:
            aliases
            >> Dict.insert requiring { requiringDef with type = fixedType } __
            >> Just

        , Err {}: Nothing



tryToBreakCircular as fn [USR], ByUsr CA.AliasDef: Res (ByUsr CA.AliasDef) =
    fn circular, aliases:

    pairs =
        List.circularPairs circular

    try List.findMap (toFixedType aliases __) pairs as
        , Just fixedAliases:
            Ok fixedAliases

        , Nothing:
            [
            , Debug.toHuman "Cannot fix recursive types"
            , ""
            , Debug.toHuman circular
            , ""
            ]
            >> Error.Raw
            >> Err


getAliasDependencies as fn ByUsr aliasDef, CA.AliasDef: Set USR =
    fn allAliases, aliasDef:

    aliasDef.directTypeDeps
    >> Dict.filter (fn usr, _: Dict.member usr allAliases) __

    # TODO: RefHierarchy should accept and Dict, not just a Set
    # Then we can remove this
    >> Dict.map (fn k, v: None) __


initStateAndGlobalEnv as fn [USR & Instance], [CA.Module]: Res (TA.TyvarId & Env) =
    fn externalValues, allModules:

    # Before we expand the aliases, we need to reorder them
    rawAliases as ByUsr CA.AliasDef =
        CoreTypes.aliases
        >> List.for __ allModules fn mod, zz:
            Dict.for zz mod.aliasDefs fn name, aliasDef, d:
                Dict.insert aliasDef.usr aliasDef d

    circulars & orderedAliases =
        RefHierarchy.reorder (getAliasDependencies rawAliases __) rawAliases

    rawAliases
    >> List.forRes __ circulars tryToBreakCircular
    >> onOk fn fixedAliases:

    !state =
        initState 0

    # Expand all aliases
    expandedAliases as ByUsr ExpandedAlias =
        List.for Dict.empty orderedAliases (expandAndInsertAlias @state fixedAliases __ __)

    doStuff as fn CA.Module, Env: Env =
        fn caModule, env:
        Dict.for env caModule.valueDefs (fn pattern, v, a: addValueToGlobalEnv @state caModule.umr v a)

    env =
        { initEnv with
        , expandedAliases
        }
        >> List.for __ Prelude.allCoreValues (addCoreValueToCoreEnv @state __ __)
        >> List.for __ externalValues (fn usr & instance, e: { e with variables = Dict.insert (RefGlobal usr) instance .variables })
        >> List.for __ allModules doStuff

    try Array.toList @state.errors as
        , []:
            state.lastUnificationVarId & env
            >> Ok

        , list:
            list
            >> Error.Nested
            >> Err



#
#
# Uniqueness constraints resolution
#
#
addSub as fn UnivarId, Uniqueness, @Hash UnivarId Uniqueness: None =
    fn newId, newUni, @subs:

    todo "addSub"
#    replace =
#        fn id, uni:
#        try uni as
#            , Depends blah: if blah == newId then newUni else uni
#            , _: uni
#
#    subs
#    >> Dict.map replace __
#    >> Dict.insert newId newUni __


solveUniquenessConstraint as fn Env, UnivarEquality, @State: None =
    fn env, eq, @state:

    try Hash.get @state.univarSubs eq.id as
        , Nothing:
            addSub eq.id eq.uni @state.univarSubs

        , Just subUni:
            if subUni == eq.uni then
                # Nothing to do =)
                None
            else
                try subUni & eq.uni as
                    , Depends subId & _:
                        addSub subId eq.uni @state.univarSubs

                    , _ & Depends newId:
                        addSub newId subUni @state.univarSubs

                    , _:
                        addError env eq.pos (ErrorUnresolvableUniqueness eq subUni) @state


#
#
# Equalities resolution
#
#
addErError as fn Env, Equality, Text, @State: None =
    fn env, equality, message, @state:

    { with context, pos, why, type1, type2 } =
        equality

    [
    , message
    , Debug.toHuman context
    , Debug.toHuman why
    , "TYPE 1 -----------------------"
    , typeToHuman env type1
    , "TYPE 2 -----------------------"
    , typeToHuman env type2
    ]
    >> Error.Simple env.errorModule pos __
    >> Array.push @state.errors __


addErErrorIf as fn Env, Bool, Equality, Text, @State: None =
    fn env, test, equality, message, @state:

    if test then addErError env equality message @state else None


compareParTypes as fn Env, Equality, Int, TA.ParType, TA.ParType, @State: None =
    fn env, currentEquality, index, p1, p2, @state:

    { with context, pos, why, expandedRecursives } =
        currentEquality

    try p1 & p2 as
        , TA.ParRe raw1 & TA.ParRe raw2:
            { expandedRecursives, context, pos, why = Why_FunctionInput index why, type1 = raw1, type2 = raw2 }
            >> solveEquality env __ @state

        , TA.ParSp full1 & TA.ParSp full2:

            {
            , context
            , pos
            , why = Why_FunctionInput index why
            , type1 = full1.raw
            , type2 = full2.raw
            , expandedRecursives
            }
            >> solveEquality env __ @state

            try uniCanBeCastTo { given = full1.uni, required = full2.uni } as
                , CanBeCastYes: None
                , CanBeCastNo []: addErError env currentEquality ("Function call par " .. Text.fromNumber index .. " with wrong uniqueness") @state
                , CanBeCastNo [id & uni, ...tail]: solveUniquenessConstraint env { pos, context, id, uni, why = "fn arg" } @state

            None

        , _:
            addErError env { currentEquality with why = (Why_FunctionInput index why) } "recycling does not match" @state


#
# This turns an Equality into substitutions and errors, added to @State
#
solveEquality as fn Env, Equality, @State: None =
    fn env, head, @state:

    { expandedRecursives, context, pos, why, type1, type2 } =
        head

    try type1 & type2 as

      , TA.TypeVar tyvarId & t2:
          replaceUnificationVariable env head tyvarId t2 @state


      , t1 & TA.TypeVar tyvarId:
          replaceUnificationVariable env head tyvarId t1 @state


      , TA.TypeOpaque usr1 args1 & TA.TypeOpaque usr2 args2:
          if usr1 /= usr2 then
              addErError env head "types are incompatible2" @state
          else
              List.indexedEach2 args2 args1 fn index, raw1, raw2:
                  { head with why = Why_TypeArgument usr1 index why
                  , type1 = raw1
                  , type2 = raw2
                  }
                  >> solveEquality env __ @state

              None


      , TA.TypeFn pars1 out1 & TA.TypeFn pars2 out2:
          if List.length pars1 /= List.length pars2 then
              addErError env head "functions expect a different number of arguments" @state

          else
              solveEquality env { head with why = Why_FunctionOutput why, type1 = out1.raw, type2 = out2.raw } @state

              # TODO there is not much guarantee which one is given and which one is required
              try uniCanBeCastTo { given = out2.uni, required = out1.uni } as
                  , CanBeCastYes: None
                  , CanBeCastNo []: addErError env head "the function return type have different uniqueness" @state
                  , CanBeCastNo [id & uni, ...tail]: solveUniquenessConstraint env { pos, context, id, uni, why = "fn out" } @state

              List.indexedEach2 pars1 pars2 (compareParTypes env head __ __ __ @state)


      , TA.TypeUnion ext1 cbn1 & TA.TypeUnion ext2 cbn2:
          solveUnionEquation env head ext1 cbn1 ext2 cbn2 @state


      , TA.TypeRecord Nothing attrs1 & TA.TypeRecord Nothing attrs2:
          only1 & both & only2 =
              Dict.onlyBothOnly attrs1 attrs2

          Dict.each both fn attrName, (attrType1 & attrType2):
              solveEquality env { head with why = Why_Attribute why, type1 = attrType1, type2 = attrType2 } @state

          addErErrorIf env (only1 /= Dict.empty or only2 /= Dict.empty) head "record attrs don't match" @state


      , TA.TypeRecord (Just tyvar1) attrs1 & TA.TypeRecord Nothing attrs2:
          solveRecordExt env head False tyvar1 attrs1 attrs2 @state


      , TA.TypeRecord Nothing attrs1 & TA.TypeRecord (Just tyvar2) attrs2:
          # The True is to swap the terms when we insert the equality, so that we don't mix given and required
          solveRecordExt env head True tyvar2 attrs2 attrs1 @state


      , TA.TypeRecord (Just tyvar1) attrs1 & TA.TypeRecord (Just tyvar2) attrs2:

          only1 & both & only2 =
              Dict.onlyBothOnly attrs1 attrs2

          newExtId =
              newTyvarId @state

          newType =
              TA.TypeRecord (Just newExtId) (Dict.join attrs1 only2)

          replaceUnificationVariable env head tyvar1 newType @state
          replaceUnificationVariable env head tyvar2 newType @state

          Dict.each both fn name, (t1 & t2):
              solveEquality env { expandedRecursives, context = Context_AttributeName name context, pos, why, type1 = t1, type2 = t1 } @state


      , TA.TypeRecursive usr1 args1 & TA.TypeRecursive usr2 args2:
          #
          # NOTE: Recursion is nominal, not structural.
          # Fot the time being I hope I can get away with it.
          #
          if usr1 /= usr2 then
              addErError env head "wrong TypeRecursive!?" @state
          else
              # TODO do I actually need to compare args? I don't have the brainpower to think about this now. T_T
              List.indexedEach2 args2 args1 fn index, a, b:
                  solveEquality env { expandedRecursives, context, pos, why = Why_TypeArgument usr1 index why, type1 = a, type2 = b } @state


      , TA.TypeRecursive usr1 args1 & _:
          solveTypeRecursive env head usr1 args1 type2 @state


      , _ & TA.TypeRecursive usr2 args2:
          solveTypeRecursive env head usr2 args2 type1 @state


      , TA.TypeError & _:
          None


      , _ & TA.TypeError:
          None


      , _:
          addErError env head "types are incompatible1" @state


solveTypeRecursive as fn Env, Equality, USR, [TA.RawType], TA.RawType, @State: None =
    fn env, eq, recUsr, recArgs, otherType, @state:

    if Set.member recUsr eq.expandedRecursives then
        addErError env eq "(recursive) types are incompatible" @state
    else

        recAlias as ExpandedAlias =
            try Dict.get recUsr env.expandedAliases as
                , Nothing: bug "no expandedAlias!"
                , Just a: a

        argTypeByTyvarId =
            List.for2 Dict.empty recAlias.pars recArgs Dict.insert

        subsAsFn as TA.SubsAsFns = {
            , ty = Dict.get __ argTypeByTyvarId
            , uni = fn _: Nothing
            }

        recType =
            TA.resolveRaw subsAsFn recAlias.type

        { eq with
        , expandedRecursives = Set.insert recUsr .expandedRecursives
        , type1 = recType
        , type2 = otherType
        }
        >> solveEquality env __ @state


solveUnionEquation as fn Env, Equality, Maybe TA.TyvarId, Dict Name [TA.RawType], Maybe TA.TyvarId, Dict Name [TA.RawType], @State: None =
    fn env, equality, ext1, cbn1, ext2, cbn2, @state:

    { with context, pos, why } =
        equality

    only1 & both & only2 =
        Dict.onlyBothOnly cbn1 cbn2

    # checkExtensibility
    List.each [ ext1 & only2, ext2 & only1 ] fn ext & only:
        addErErrorIf env (only /= Dict.empty and ext == Nothing) equality "TODO error message ext & only" @state

    # compareConstructorArgs
    Dict.each both fn name, (args1 & args2):

        l1 = List.length args1
        l2 = List.length args2

        if l1 /= l2 then
            addErError env equality ("constructor " .. name .. "wrong args number: " .. Text.fromNumber l1 .. " vs " .. Text.fromNumber l2) @state
        else
            List.indexedEach2 args1 args2 fn index, a1, a2:
                solveEquality env { equality with context = Context_ConstructorArgument name index, type1 = a1, type2 = a2 } @state

    # solveExtensions
    exts =
        List.filterMap identity [ ext1, ext2 ]

    if exts == [] then
        None
    else
        maybeExt =
            if ext1 == Nothing or ext2 == Nothing then
                Nothing
            else
                Just (newTyvarId @state)

        newType =
            TA.TypeUnion maybeExt (Dict.join cbn1 cbn2)

        List.each exts (replaceUnificationVariable env equality __ newType @state)


solveRecordExt as fn Env, Equality, Bool, TA.TyvarId, Dict Name TA.RawType, Dict Name TA.RawType, @State: None =
    fn env, equality, swapEquality, tyvar1, attrs1, attrs2, @state:

    #
    # { tyvar1 with attrs1 } == { attrs2 }
    #
    # =>    tyvar1 == { attrs2 }
    #
    # =>    all of attrs1 must be in attrs2 and match
    #

    { with context, pos, why } =
        equality

    Dict.each attrs1 fn name, type1:
        try Dict.get name attrs2 as
            , Nothing:
                addErError env equality ("missing attribute " .. name) @state

            , Just type2:
                a & b = if swapEquality then type2 & type1 else type1 & type2

                { equality with
                , context = Context_AttributeName name context
                , type1 = a
                , type2 = b
                }
                >> solveEquality env __ @state

    replaceUnificationVariable env equality tyvar1 (TA.TypeRecord Nothing attrs2) @state


replaceUnificationVariable as fn Env, Equality, TA.TyvarId, TA.RawType, @State: None =
    fn env, equality, tyvarId, replacingType, @state:

    isSame =
        try replacingType as
            , TA.TypeVar tyvarId2:
                tyvarId == tyvarId2

            , _:
                False

    if isSame then
        None
    else if occurs tyvarId replacingType then
        addErError env equality "circular!?" @state
    else
        todo "ZZZZZOT"
        [#
        zzz =
            fn eq:
            { eq with
            , type1 = applySubstitutionToType tyvarId replacingType .type1
            , type2 = applySubstitutionToType tyvarId replacingType .type2
            }

        equalities =
            List.map zzz state.equalities

        substitutions =
            state.substitutions
            >> Dict.map (fn _, type: applySubstitutionToType tyvarId replacingType type) __
            >> Dict.insert tyvarId replacingType __

        { state with substitutions, equalities }
        #]


occurs as fn TA.TyvarId, TA.RawType: Bool =
    fn tyvarId, type:

    rec =
        occurs tyvarId __

    try type as
        , TA.TypeFn ins out: List.any (fn t: t >> TA.toRaw >> rec) ins or rec out.raw
        , TA.TypeVar id: id == tyvarId
        , TA.TypeOpaque usr args: List.any rec args
        , TA.TypeUnion _ consByName: Dict.any (fn k, v: List.any rec v) consByName
        , TA.TypeRecord _ attrs: Dict.any (fn k, v: rec v) attrs
        , TA.TypeRecursive usr args: False # It's recursive, so we can assume it was done already?
        , TA.TypeError: False


applySubstitutionToType as fn TA.TyvarId, TA.RawType, TA.RawType: TA.RawType =
    fn tyvarId, replacingType, originalType:

    subsAsFns as TA.SubsAsFns =
        {
        , ty = fn id: if id == tyvarId then Just replacingType else Nothing
        , uni = fn _: Nothing
        }

    TA.resolveRaw subsAsFns originalType

