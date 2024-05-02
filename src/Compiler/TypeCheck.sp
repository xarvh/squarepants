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
    'USR umr _ =
        env.currentRootUsr

    module =
        try Dict.get umr env.modulesByUmr as
            'nothing: CoreDefs.coreModule
            'just m: m

    raw
    >> Human/Type.doRawType module __
    >> Human/Format.formatExpression { isRoot = 'true, originalContent = "" } __
    >> Fmt.render


bug as fn Text: a =
    fn msg:
    todo ("Compiler bug: " .. msg)


State =
    {
    [#
       Every time we use a value in an expression, we must re-instantiate its free variables, because each time they can be used in a different way.

       Because of this each Definition has a `freeTypeVariables` field.

       To figure out `freeTypeVariables` we can't simply take all tyvars we find in the definition's type, because:

       1) a tyvar that is free in a parent definition should not be considered free in children definitions:

         parent as List a =


           child as fn a: a =
             ...

           ...

       2) the placeholder tyvar of an argument should not be considered free while we infer the type of a function, otherwise we keep reinstantitiating it and we never figure out anything.

         f =
           fn x:
           { first } = x
           first

       By keeping track of which tyvars should NOT be considered free, we can figure out the correct `freeTypeVariables` field for each child definition.
    #]
    , boundTyvars as Hash TA.TyvarId None
    , errors as Array Error
    , lastLambdaRefId as Int
    , lastUnificationVarId as Int
    , tyvarSubs as Hash TA.TyvarId TA.RawType
    , tyvarsById as Hash TA.TyvarId TA.Tyvar
    , univarSubs as Hash UnivarId Uniqueness
    , univarsById as Hash UnivarId TA.Univar
    }


initState as fn !Int: !State =
    fn !lastUnificationVarId:
    {
    , boundTyvars = Hash.fromList []
    , errors = Array.fromList []
    , lastLambdaRefId = -1
    , lastUnificationVarId
    , tyvarSubs = Hash.fromList []
    , tyvarsById = Hash.fromList []
    , univarSubs = Hash.fromList []
    , univarsById = Hash.fromList []
    }


Instance =
    {
    , definedAt as Pos
    , freeTyvars as Dict TA.TyvarId TA.Tyvar
    , freeUnivars as Dict UnivarId TA.Univar
    , type as TA.FullType
    }


ExpandedAlias =
    {
    , pars as [ TA.TyvarId ]
    , type as TA.RawType
    }


Env =
    {
    , annotatedTyvarsByName as Dict Name TA.TyvarId
    , annotatedUnivarsByOriginalId as Dict UnivarId UnivarId
    , constructors as ByUsr Instance
    , context as Context
    , currentRootUsr as USR
    # TODO merge these two in a single Dict
    , exactTypes as ByUsr [ Name & Pos ]
    , expandedAliases as ByUsr ExpandedAlias
    , modulesByUmr as Dict UMR CA.Module
    , projectImports as Imports
    , reversedRootValueDefs as [ USR & TA.ValueDef ]
    , variables as Dict Ref Instance
    }


initEnv as fn Imports, Dict UMR CA.Module: Env =
    fn projectImports, modulesByUmr:
    {
    , annotatedTyvarsByName = Dict.empty
    , annotatedUnivarsByOriginalId = Dict.empty
    , constructors = Dict.empty
    , context = 'context_Global
    , currentRootUsr = CoreDefs.usr "error"
    , exactTypes = Dict.empty
    , expandedAliases = Dict.empty
    , modulesByUmr
    , projectImports
    , reversedRootValueDefs = []
    , variables = Dict.empty
    }


# TODO: once we have proper error messages, there won't be much point in using these instead than error functions directly
var Error_ =
    , 'errorTypeNotFound USR
    , 'errorTyvarNotIndependent Name
    , 'errorUnresolvableUniqueness UnivarEquality Uniqueness
    , 'errorShouldBeUnique
    , 'errorCircularValue
    , 'errorVariableNotFound Ref
    , 'errorConstructorNotFound USR
    , 'errorNotCompatibleWithRecord
    , 'errorRecordDoesNotHaveAttribute Name
    , 'errorRecordHasAttributesNotInAnnotation [ Name ]
    , 'errorRecordIsMissingAttibutesInAnnotation [ Name ]
    , 'errorTryingToAccessAttributeOfNonRecord Name TA.RawType
    , 'errorIncompatibleTypes CA.Expression TA.FullType
    , 'errorIncompatiblePattern CA.Pattern TA.FullType
    , 'errorCallingANonFunction TA.RawType
    , 'errorWrongNumberOfArguments { expected as Int, given as Int, reference as CA.Expression }
    , 'errorWrongNumberOfParameters
    , 'errorWrongNumberOfConstructorArguments
    , 'errorNotEnoughArguments
    , 'errorIncompatibleRecycling
    , 'errorUniquenessDoesNotMatch { given as Uniqueness, required as Uniqueness }
    , 'errorUniquenessDoesNotMatchArgument
    , 'errorUniquenessDoesNotMatchParameter Uniqueness TA.FullType
    , 'errorRecyclingDoesNotMatch
    , 'errorUndefinedTypeVariable Name
    , 'errorWrongNumberOfTypeArguments USR [ TA.TyvarId ] [ TA.RawType ]
    , 'errorNamedTypeNotFound USR
    , 'errorCircularAlias [ USR ]
    , 'errorTypeAllowsFunctions TA.RawType TA.Tyvar UMR
    , 'errorUniInTypeArg
    , 'errorUniInRecordAttribute Name
    , 'errorUniqueGlobal
    , 'errorModuleNotFound UMR


var Context =
    , 'context_Global
    , 'context_Argument Name Context
    , 'context_LetInBody [ Name ]
    , 'context_FnPar Int Context
    , 'context_FnBody Pos Context
    , 'context_TryBranch
    , 'context_IfCondition
    , 'context_IfFalse
    , 'context_IfTrue
    , 'context_AttributeName Name Context


var Why =
    , 'why_Annotation
    , 'why_LetIn
    , 'why_Record
    , 'why_RecordExt
    , 'why_RecordAccess
    , 'why_IfCondition
    , 'why_IfBranches
    , 'why_TryPattern
    , 'why_TryExpression
    , 'why_ReturnType
    , 'why_Argument Int
    , 'why_CalledAsFunction
    , 'why_Todo
    , 'why_Attribute Why
    , 'why_FunctionInput Int Why
    , 'why_FunctionOutput Why
    , 'why_TypeArgument USR Int Why


# TODO, make it obvious that the two types are not interchangeable, because of uniqueness
# (but how do I describe it? "given" and "required"? I forgot already what they mean.
# Also also: maybe context should follow each individual type, so we actually know what they refer to!?
Equality =
    {
    , context as Context
    , expandedRecursives as Set USR
    , pos as Pos
    , type1 as TA.RawType
    , type2 as TA.RawType
    , why as Why
    }


UnivarEquality =
    {
    , context as Context
    , id as UnivarId
    , pos as Pos
    , uni as Uniqueness
    , why as Text
    }


#
# Core types
#
coreTypeBool as TA.RawType =
    TA.'typeExact Pos.'n CoreDefs.boolUsr []


coreTypeNumber as TA.RawType =
    TA.'typeExact Pos.'n CoreDefs.numberDef.usr []


coreTypeText as TA.RawType =
    TA.'typeExact Pos.'n CoreDefs.textDef.usr []


fullTypeError as TA.FullType =
    { raw = TA.'typeError, uni = 'uni }


patternError as fn Pos: TA.Pattern =
    fn pos:
    TA.'patternAny pos { maybeName = 'nothing, type = fullTypeError }


#
#
#
newTyvarId as fn @State: TA.TyvarId =
    fn @state:
    @state.lastUnificationVarId += 1

    cloneUni @state.lastUnificationVarId


newRawType as fn @State: TA.RawType =
    fn @state:
    TA.'typeVar Pos.'g (newTyvarId @state)


getErrorModule as fn Env: Error.Module =
    fn env:
    'USR umr _ =
        env.currentRootUsr

    try Dict.get umr env.modulesByUmr as
        'nothing: { content = "", fsPath = "N/A" }
        'just { with  asText = content, fsPath }: { content, fsPath }


addEquality as fn Env, Pos, Why, TA.RawType, TA.RawType, @State: None =
    fn env, pos, why, t1, t2, @state:
    #log "EQ" { why , type1 = applyAllSubs @state t1 , type2 = applyAllSubs @state t2 }

    solveEquality
        env
        {
        , context = env.context
        , expandedRecursives = Set.empty
        , pos
        , type1 = applyAllSubs @state t1
        , type2 = applyAllSubs @state t2
        , why
        }
        @state


addError as fn Env, Pos, Error_, @State: None =
    fn env, pos, error, @state:
    addErrorE env pos error @state.errors


addErrorE as fn Env, Pos, Error_, @Array Error: None =
    fn env, pos, error, @errors:
    [
    , Debug.toHuman error
    , Debug.toHuman env.context
    ]
    >> Error.'simple (getErrorModule env) pos __
    >> Array.push @errors __


addErrorIf as fn Bool, Env, Pos, Error_, @State: None =
    fn test, env, pos, error, @state:
    if test then addError env pos error @state else 'none


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
        newUnivarId =
            newTyvarId @state

        #Hash.insert @state.univarsById newUnivarId { originalId = originalUnivarId, constraints = []}
        replaceUnivarRec originalUnivarId ('depends newUnivarId) r

    raw =
        instance.type.raw
        >> Dict.for __ instance.freeUnivars replaceUnivar
        >> Dict.for __ instance.freeTyvars fn originalTyvarId, tyvar, a:
            generalizedTyvarId =
                newTyvarId @state

            # The new tyvar has the same typeclasses as the original!
            Hash.insert @state.tyvarsById generalizedTyvarId tyvar

            # { tyvar with generalizedAt = pos, generalizedFor = ref }

            applySubstitutionToType originalTyvarId (TA.'typeVar Pos.'g generalizedTyvarId) a

    { instance.type with raw }


replaceUnivarRec as fn UnivarId, Uniqueness, TA.RawType: TA.RawType =
    fn old, new, raw:
    doRaw as fn TA.RawType: TA.RawType =
        replaceUnivarRec old new __

    try raw as

        TA.'typeExact p usr args:
            TA.'typeExact p usr (List.map doRaw args)

        TA.'typeRecord p maybeExt attrs:
            TA.'typeRecord p maybeExt (Dict.map (fn k, v: doRaw v) attrs)

        TA.'typeError:
            TA.'typeError

        TA.'typeVar p id:
            TA.'typeVar p id

        TA.'typeFn p ins out:
            doUni as fn Uniqueness: Uniqueness =
                fn uni:
                try uni as
                    'depends id: if id == old then new else uni
                    _: uni

            mapPar as fn TA.ParType: TA.ParType =
                fn par:
                try par as
                    TA.'parRe r: TA.'parRe (doRaw r)
                    TA.'parSp f: TA.'parSp { raw = doRaw f.raw, uni = doUni f.uni }

            TA.'typeFn p (List.map mapPar ins) { raw = doRaw out.raw, uni = doUni out.uni }


#
#
# CA to TA translation
#
#
expandTyvarsInType as fn Dict TA.TyvarId TA.RawType, TA.RawType: TA.RawType =
    fn tyvarIdsToType, type:
    rec =
        expandTyvarsInType tyvarIdsToType __

    try type as

        TA.'typeExact p usr args:
            TA.'typeExact p usr (List.map rec args)

        TA.'typeFn p ins out:
            TA.'typeFn p (TA.mapPars rec ins) { out with raw = rec .raw }

        TA.'typeRecord p 'nothing attrs:
            TA.'typeRecord p 'nothing (Dict.map (fn k, v: rec v) attrs)

        TA.'typeVar p id:
            try Dict.get id tyvarIdsToType as
                'nothing: bug "this is not supposed to happen"
                'just ty: ty

        TA.'typeRecord p ('just id) attrs:
            TA.'typeRecord p ('just id) (Dict.map (fn k, v: rec v) attrs)

        TA.'typeError:
            TA.'typeError


# This is not really a "translate", it just replaces the originalId with the generated one
translateUni as fn Dict UnivarId UnivarId, Uniqueness: Uniqueness =
    fn originalIdToNewId, originalUni:
    try originalUni as

        'depends originalId:
            try Dict.get originalId originalIdToNewId as
                'just newId: 'depends newId
                'nothing: originalUni

        _:
            originalUni


translateFullType as fn Env, Dict Name TA.RawType, Dict UnivarId UnivarId, @Array Error, CA.FullType: TA.FullType =
    fn env, argsByName, originalIdToNewId, @errors, caFull:
    {
    , raw = translateRawType env argsByName originalIdToNewId @errors caFull.raw
    , uni = translateUni originalIdToNewId caFull.uni
    }


translateRawType as fn Env, Dict Name TA.RawType, Dict UnivarId UnivarId, @Array Error, CA.RawType: TA.RawType =
    fn env, argsByName, originalIdToNewId, @errors, caType:
    rec as fn CA.RawType: TA.RawType =
        translateRawType env argsByName originalIdToNewId @errors __

    try caType as

        CA.'typeFn pos caPars caOut:
            zzz =
                fn caPar:
                    try caPar as
                        CA.'parRe caRaw: TA.'parRe (rec caRaw)
                        CA.'parSp caFull: TA.'parSp (translateFullType env argsByName originalIdToNewId @errors caFull)

            taArgs as [ TA.ParType ] =
                List.map zzz caPars

            TA.'typeFn pos taArgs (translateFullType env argsByName originalIdToNewId @errors caOut)

        CA.'typeRecord pos caAttrs:
            TA.'typeRecord pos 'nothing (Dict.map (fn name, v: rec v) caAttrs)

        CA.'typeAnnotationVariable pos name:
            try Dict.get name argsByName as

                'nothing:
                    addErrorE env pos ('errorUndefinedTypeVariable name) @errors

                    TA.'typeError

                'just raw:
                    raw

        CA.'typeNamed pos usr pars:
            expandedPars as [ TA.RawType ] =
                List.map rec pars

            try Dict.get usr env.expandedAliases as

                'nothing:
                    try Dict.get usr env.exactTypes as

                        'just exact:
                            TA.'typeExact pos usr expandedPars

                        'nothing:
                            addErrorE env pos ('errorTypeNotFound usr) @errors

                            TA.'typeError

                'just expandedAlias:
                    if List.length expandedAlias.pars /= List.length expandedPars then
                        addErrorE env pos ('errorWrongNumberOfTypeArguments usr expandedAlias.pars expandedPars) @errors

                        TA.'typeError
                    else
                        tyvarIdsToType as Dict TA.TyvarId TA.RawType =
                            List.map2 Tuple.pair expandedAlias.pars expandedPars >> Dict.fromList

                        expandTyvarsInType tyvarIdsToType expandedAlias.type


translateAnnotationType as fn Env, @State, CA.RawType: TA.RawType =
    fn env, @state, ca:
    nameToType =
        Dict.map (fn k, v: TA.'typeVar Pos.'g v) env.annotatedTyvarsByName

    translateRawType env nameToType env.annotatedUnivarsByOriginalId @state.errors ca


var CanBeCast =
    , 'canBeCastYes
    , # When cast is not possible, it is still possible to resolve the check by imposing a constrain on one of the two values.
      # (This can be done only if the univar is free)
      'canBeCastNo [ UnivarId & Uniqueness ]


#
# This answers the question "can I use `given` uniqueness when `required` is required?
#
uniCanBeCastTo as fn { given as Uniqueness, required as Uniqueness }: CanBeCast =
    fn { given, required }:
    try given & required as

        # Anything can be cast to Imm
        _
        & 'imm:
            'canBeCastYes

        # Uni can be cast to anything
        'uni
        & _:
            'canBeCastYes

        'imm & 'uni:
            'canBeCastNo []

        'depends a & 'uni:
            'canBeCastNo [ a & 'uni ]

        'depends a & 'depends b:
            if a == b then
                'canBeCastYes
            else
                'canBeCastNo [ a & 'depends b, b & 'depends a ]

        'imm & 'depends b:
            'canBeCastNo [ b & 'imm ]


checkUni as fn Env, Pos, { given as Uniqueness, required as Uniqueness }, @State: None =
    fn env, pos, { given, required }, @state:
    try uniCanBeCastTo { given, required } as
        'canBeCastYes: 'none
        'canBeCastNo []: addError env pos ('errorUniquenessDoesNotMatch { given, required }) @state
        'canBeCastNo (univarId & uni :: tail): addConstraint env pos univarId uni @state


addConstraint as fn Env, Pos, UnivarId, Uniqueness, @State: None =
    fn env, pos, id, uni, @state:
    eq as UnivarEquality =
        {
        , context = 'context_Global
        , id
        # TODO
        , pos
        , uni
        , why = "-"
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

        'imm & _:
            'imm

        _ & 'imm:
            'imm

        'depends aId & 'depends bId:
            # TODO ----> if at least one is free, actually try to merge the two?
            'imm

        _ & 'depends _:
            b

        _:
            'uni


#
#
# Definitions
#
#

CA_ValueDef =
    {
    , directDeps as Dict USR DependencyType
    , maybeBody as Maybe CA.Expression
    , pattern as CA.Pattern
    , uni as Uniqueness
    }


doDefinition as fn fn Name: Ref, Env, CA_ValueDef, @State: TA.ValueDef & Env =
    fn nameToRef, baseEnv, def, @state:
    # TODO explain why we need this...
    !parentBoundTyvars =
        cloneUni @state.boundTyvars

    patternOut =
        inferPattern baseEnv def.uni def.pattern @state

    localEnv =
        { patternOut.env with
        , context = 'context_LetInBody (TA.patternNames patternOut.typedPattern >> Dict.keys)
        }

    (typedBody as Maybe TA.Expression) & (bodyType as TA.FullType) =
        try def.maybeBody as

            'nothing:
                'nothing & { raw = patternOut.patternType, uni = def.uni }

            'just body:
                try patternOut.maybeFullAnnotation as

                    'just annotation:
                        raw =
                            translateAnnotationType localEnv @state annotation.raw

                        full =
                            { raw, uni = def.uni }

                        pars as CheckExpressionPars =
                            {
                            , annotatedPattern = patternOut.typedPattern
                            , annotation = annotation.raw
                            , env = localEnv
                            }

                        checkExpression pars full body @state >> Tuple.mapFirst 'just __

                    'nothing:
                        typed & inferredType =
                            inferExpression localEnv body @state

                        pos =
                            CA.patternPos def.pattern

                        addEquality localEnv pos 'why_LetIn patternOut.patternType inferredType.raw @state

                        checkUni localEnv pos { given = inferredType.uni, required = def.uni } @state

                        'just typed & inferredType

    defType as TA.FullType =
        {
        , raw = applyAllSubs @state bodyType.raw
        , uni = def.uni
        }

    # Univars are not inferred.
    # If they are not annotated, values are assumed to be immutable
    freeUnivars as Dict UnivarId TA.Univar =
        def.pattern
        >> CA.patternNames
        >> List.filterMap (fn entry: entry.maybeAnnotation) __
        >> List.for Dict.empty __ (fn annotation, acc: Dict.join annotation.univars acc)
        >> Dict.for Dict.empty __ fn annotatedId, 'none, acc:
            try Dict.get annotatedId localEnv.annotatedUnivarsByOriginalId as
                'nothing: acc
                'just newId: Dict.insert newId { annotatedId } acc

    # TODO explain what is happening here
    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        # TODO: not all tyvars are bindable.
        # for example, in `fn a: a` a would be free unless it also appears somewhere else?
        allBindableTyvarsIn as fn TA.TyvarId: Set TA.TyvarId =
            fn tyvarId:
            try Hash.get @state.tyvarSubs tyvarId as
                'nothing: Set.empty
                'just raw: Dict.map (fn k, v: 'none) (TA.typeTyvars raw)

        resolvedParentBoundTyvars as Set TA.TyvarId =
            Hash.for_ Set.empty @parentBoundTyvars fn parentTyvar, _, set:
                set
                >> Set.insert parentTyvar __
                >> Set.for __ (allBindableTyvarsIn parentTyvar) Set.insert

        tyvarAnnotatedNameByTyvarId =
            Dict.for Dict.empty localEnv.annotatedTyvarsByName (fn name, tyvarId, acc: Dict.insert tyvarId name acc)

        defType.raw
        >> TA.typeTyvars
        >> Dict.for Dict.empty __ fn id, _, acc:
            if Set.member id resolvedParentBoundTyvars then
                acc
            else
                try Dict.get id tyvarAnnotatedNameByTyvarId as

                    'nothing:
                        { maybeAnnotated = 'nothing }

                    # TODO: actually set allowFunctions!!!
                    'just name:
                        { maybeAnnotated = 'just { allowFunctions = 'true, name } }
                >> Dict.insert id __ acc

    #
    # Add instance
    #
    caNames =
        CA.patternNames def.pattern >> List.indexBy (fn e: e.name) __

    instance as fn Name, { pos as Pos, type as TA.FullType }: Instance =
        fn name, { pos, type = unresolvedType }:
        type =
            { unresolvedType with raw = applyAllSubs @state .raw }

        typeTyvars =
            TA.typeTyvars type.raw

        actualTyvars =
            Dict.filter (fn k, v: Dict.member k typeTyvars) freeTyvars

        tryAsStillBreaksIfUsedImperatively =
            try Dict.get name caNames as
                'just { with  maybeAnnotation = 'just annotation, pos = p }: addErrorIf (Dict.size annotation.tyvars > Dict.size actualTyvars) localEnv p ('errorTyvarNotIndependent name) @state
                _: 'none

        # TODO Also check that all uniqueness vars are independent

        {
        , definedAt = pos
        , freeTyvars = actualTyvars
        , freeUnivars
        , type
        }

    variables =
        patternOut.env.variables
        >> Dict.for __ (TA.patternNames patternOut.typedPattern) fn name, stuff, vars:
            Dict.insert (nameToRef name) (instance name stuff) vars

    {
    , body = typedBody
    , directDeps = def.directDeps
    , freeTyvars
    , freeUnivars
    , isFullyAnnotated = patternOut.maybeFullAnnotation /= 'nothing
    , pattern = patternOut.typedPattern
    , type = defType
    }
    & { baseEnv with variables }


#
#
# Expressions
#
#
inferExpression as fn Env, CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, caExpression, @state:
    try caExpression as

        CA.'literalNumber pos n:
            TA.'literalNumber pos n & { raw = coreTypeNumber, uni = 'uni }

        CA.'literalText pos text:
            TA.'literalText pos text & { raw = coreTypeText, uni = 'uni }

        CA.'variable pos ref:
            ty =
                try getVariableByRef ref env as

                    'nothing:
                        addError env pos ('errorVariableNotFound ref) @state

                        fullTypeError

                    'just var:
                        t =
                            generalize env pos ref var @state

                        #log ("GEN---> " .. toHuman ref) { var, type = t }
                        t

            TA.'variable pos ref & ty

        CA.'constructor pos usr:
            ty =
                try getConstructorByUsr usr env as

                    'nothing:
                        addError env pos ('errorConstructorNotFound usr) @state

                        fullTypeError

                    'just cons:
                        generalize env pos ('refGlobal usr) cons @state

            # TODO setting Uni like this feels a bit hacky... =|
            TA.'constructor pos usr & { ty with uni = 'uni }

        CA.'fn pos caPars body:
            inferFn env pos caPars body @state

        CA.'call pos reference args:
            doCall env pos 'nothing reference args @state

        CA.'record pos maybeExt attrs:
            inferRecord env pos maybeExt attrs @state

        CA.'recordAccess pos attrName recordExpression:
            typedExpr & inferredType =
                inferExpression env recordExpression @state

            TA.'recordAccess pos attrName typedExpr & { inferredType with raw = inferRecordAccess env pos attrName .raw @state }

        CA.'letIn def rest:
            typedDef & defEnv =
                {
                , directDeps = Dict.empty
                , maybeBody = 'just def.body
                , pattern = def.pattern
                , uni = def.uni
                }
                >> doDefinition 'refLocal env __ @state

            typedRest & restType =
                inferExpression defEnv rest @state

            TA.'letIn typedDef typedRest restType & restType

        CA.'if pos { condition, false, true }:
            typedCondition & conditionType =
                inferExpression { env with context = 'context_IfCondition } condition @state

            addEquality env pos 'why_IfCondition coreTypeBool conditionType.raw @state

            typedTrue & trueType =
                inferExpression { env with context = 'context_IfTrue } true @state

            typedFalse & falseType =
                inferExpression { env with context = 'context_IfFalse } false @state

            addEquality env pos 'why_IfBranches trueType.raw falseType.raw @state

            expression =
                TA.'if
                    pos
                    {
                    , condition = typedCondition
                    , false = typedFalse
                    , true = typedTrue
                    }

            # TODO What if "depends" tyvars get resolved to the same?
            # Shouldn't we test this only AFTER solving constraints?
            uni as Uniqueness =
                inferUni trueType.uni falseType.uni

            expression & { raw = trueType.raw, uni }

        CA.'try pos { patternsAndExpressions, value }:
            doTry env pos (newRawType @state) value patternsAndExpressions @state

        CA.'introspect pos introspect usr:
            doIntrospect env pos introspect usr @state


getTypeDef as fn Env, Pos, USR, @State: Maybe ([ Name & Pos ] & Self.Def) =
    fn env, pos, usr, @state:
    'USR umr name =
        usr

    try Dict.get umr env.modulesByUmr as

        'nothing:
            addError env pos ('errorModuleNotFound umr) @state

            'nothing

        'just module:
            try Dict.get name module.aliasDefs as

                'just def:
                    'just << def.pars & Self.'openAliasType def

                'nothing:
                    try Dict.get name module.variantTypeDefs as

                        'nothing:
                            addError env pos ('errorTypeNotFound usr) @state

                            'nothing

                        'just def:
                            'just << def.pars & Self.'openVarType def


getValueDef as fn Env, Pos, USR, @State: Maybe CA.ValueDef =
    fn env, pos, usr, @state:
    'USR umr name =
        usr

    try Dict.get umr env.modulesByUmr as

        'nothing:
            addError env pos ('errorModuleNotFound umr) @state

            'nothing

        'just module:
            try Dict.get name module.valueDefs as

                'just def:
                    'just def

                'nothing:
                    # TODO message should be "module X does not contain...."
                    # Which should be caught by MakeCanonical... Can it actually happen here?
                    addError env pos ('errorVariableNotFound ('refGlobal usr)) @state

                    'nothing


doIntrospect as fn Env, Pos, Token.Introspect, USR, @State: TA.Expression & TA.FullType =
    fn env, pos, introspect, usr, @state:
    selfUsr as USR =
        'USR (CoreDefs.makeUmr "Self") "Self"

    selfType as TA.RawType =
        try Dict.get selfUsr env.expandedAliases as
            'nothing: bug "no self?"
            'just expandedAlias: expandedAlias.type

    expression =
        try introspect as

            Token.'value:
                try getValueDef env pos usr @state as

                    'nothing:
                        TA.'error pos

                    'just def:
                        if def.maybeAnnotation == 'nothing then
                            todo "cannot introspect non-annotated values"
                        else
                            TA.'introspect { def = Self.'value { def with maybeBody = 'nothing }, usr }

            Token.'type:
                try getTypeDef env pos usr @state as

                    'nothing:
                        TA.'error pos

                    'just (pars & _):
                        {
                        , def =
                            Self.'opaqueType
                                {
                                , constructors = Dict.empty
                                , pars
                                , usr
                                }
                        , usr
                        }
                        >> TA.'introspect

            Token.'typeOpen:
                #TODO!!!! - ensure that type is not opaque

                try getTypeDef env pos usr @state as
                    'nothing: TA.'error pos
                    'just (_ & def): TA.'introspect { def, usr }

    expression & { raw = selfType, uni = 'uni }


doTry as fn Env, Pos, TA.RawType, CA.Expression, [ Uniqueness & CA.Pattern & CA.Expression ], @State: TA.Expression & TA.FullType =
    fn env, pos, expectedRaw, value, caPatternsAndExpressions, @state:
    typedValue & valueType =
        inferExpression env value @state

    uni & patternsAndExpressions =
        'uni & []
        >> List.forReversed __ caPatternsAndExpressions fn u & pa & exp, uniX & acc:
            patternOut as PatternOut =
                inferPattern env u pa @state

            addEquality env pos 'why_TryPattern patternOut.patternType valueType.raw @state

            checkUni env pos { given = valueType.uni, required = u } @state

            newEnv =
                { patternOut.env with
                , context = 'context_TryBranch
                }

            typedExpression & expressionType =
                inferExpression newEnv exp @state

            addEquality newEnv (CA.expressionPos exp) 'why_TryExpression expectedRaw expressionType.raw @state

            uf =
                inferUni uniX expressionType.uni

            l =
                patternOut.typedPattern & typedExpression :: acc

            uf & l

    TA.'try pos { patternsAndExpressions, value = typedValue, valueType } & { raw = expectedRaw, uni }


inferParam as fn Env, Int, CA.Parameter, @State: TA.Parameter & TA.ParType & Env =
    fn env, parIndex, par, @state:
    # TODO parIndex is not used

    try par as

        CA.'parameterRecycle pos name:
            # TODO check name already in env? Is it MakeCanonical resp?

            tyvarId =
                newTyvarId @state

            Hash.insert @state.boundTyvars tyvarId 'none

            raw =
                TA.'typeVar Pos.'g tyvarId

            instance as Instance =
                {
                , definedAt = pos
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type = { raw, uni = 'uni }
                }

            newEnv as Env =
                { env with variables = Dict.insert ('refLocal name) instance .variables }

            TA.'parameterRecycle pos raw name & TA.'parRe raw & newEnv

        CA.'parameterPattern uni pa:
            out =
                inferPattern env uni pa @state

            Dict.each (TA.typeTyvars out.patternType) fn tyvarId, _:
                Hash.insert @state.boundTyvars tyvarId 'none

            full =
                { raw = out.patternType, uni }

            TA.'parameterPattern full out.typedPattern & TA.'parSp full & out.env

        CA.'parameterPlaceholder num:
            tyvarId =
                newTyvarId @state

            Hash.insert @state.boundTyvars tyvarId 'none

            raw =
                TA.'typeVar Pos.'g tyvarId

            univarId =
                newTyvarId @state

            type =
                { raw, uni = 'depends univarId }

            instance as Instance =
                {
                , definedAt = Pos.'g
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type
                }

            newEnv as Env =
                { env with variables = Dict.insert ('refPlaceholder num) instance .variables }

            TA.'parameterPlaceholder type num & TA.'parSp type & newEnv


inferFn as fn Env, Pos, [ CA.Parameter ], CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, pos, caPars, body, @state:
    [#

      - Get all tyvars in the param types
      - Inside the function body, all these tyvars are treated as bound
      - Outside the function body, the resolved tyvars are free, unless already bound in the parent scope

      ----> At the end of a Definition we take the type tyvars, see which ones are free, then resolve them.

    #]

    [# TODO
        Urgh. This is gross.
        Do I really want to enable mixing imm and mut this way?
        Is there really an advantage?
    #]
    !typedPars =
        Array.fromList []

    !parTypes =
        Array.fromList []

    newEnv as Env =
        List.indexedFor env caPars fn index, par, envX:
            typedPar & parType & envX1 =
                inferParam envX index par @state

            Array.push @typedPars typedPar

            Array.push @parTypes parType

            envX1

    typedBody & bodyType =
        inferExpression { newEnv with context = 'context_FnBody pos env.context } body @state

    type as TA.RawType =
        TA.'typeFn pos (Array.toList @parTypes) bodyType

    exp =
        TA.'fn pos (Array.toList @typedPars) typedBody bodyType

    exp & { raw = type, uni = 'uni }


inferRecordAccess as fn Env, Pos, Name, TA.RawType, @State: TA.RawType =
    fn env, pos, attrName, inferredType, @state:
    try inferredType as

        TA.'typeRecord _ 'nothing attrTypes:
            try Dict.get attrName attrTypes as

                'just type:
                    type

                'nothing:
                    addError env pos ('errorRecordDoesNotHaveAttribute attrName) @state

                    TA.'typeError

        TA.'typeRecord p ('just tyvarId) extensionAttrTypes:
            try Dict.get attrName extensionAttrTypes as

                'just type:
                    type

                'nothing:
                    newExtId =
                        newTyvarId @state

                    newAttrType =
                        newRawType @state

                    type =
                        TA.'typeRecord p ('just newExtId) (Dict.insert attrName newAttrType extensionAttrTypes)

                    addEquality env pos 'why_RecordAccess (TA.'typeVar p tyvarId) type @state

                    newAttrType

        TA.'typeVar p id:
            newExtId =
                newTyvarId @state

            # Attrs have always the same default uni as the record
            newAttrType =
                TA.'typeVar p (newTyvarId @state)

            type as TA.RawType =
                TA.'typeRecord p ('just newExtId) (Dict.ofOne attrName newAttrType)

            addEquality env pos 'why_RecordAccess inferredType type @state

            newAttrType

        _:
            addError env pos ('errorTryingToAccessAttributeOfNonRecord attrName inferredType) @state

            TA.'typeError


inferRecord as fn Env, Pos, Maybe CA.Expression, Dict Name CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, pos, maybeExt, caAttrs, @state:
    taAttrs as Dict Name (TA.Expression & TA.FullType) =
        Dict.map (fn name, value: inferExpression { env with context = 'context_Argument name .context } value @state) caAttrs

    typedAttrs as Dict Name TA.Expression =
        Dict.map (fn k, v: Tuple.first v) taAttrs

    attrTypes as Dict Name TA.RawType =
        Dict.map (fn k, _ & t: t.raw) taAttrs

    uni as Uniqueness =
        'uni
        >> Dict.for __ taAttrs fn k, _ & full, u:
            inferUni full.uni u

    try maybeExt as

        'nothing:
            TA.'record pos 'nothing typedAttrs & { raw = TA.'typeRecord pos 'nothing attrTypes, uni }

        'just caExt:
            typedExt & extType =
                inferExpression env caExt @state

            finalType as TA.RawType =
                try extType.raw as

                    TA.'typeRecord _ 'nothing fixedTypes:
                        Dict.each attrTypes fn name, valueType:
                            try Dict.get name fixedTypes as
                                'nothing: addError env pos ('errorRecordDoesNotHaveAttribute name) @state
                                'just ty: addEquality env pos 'why_Record ty valueType @state

                        extType.raw

                    TA.'typeRecord p ('just tyvarId) extensionAttrTypes:
                        expressionOnly & both & extensionOnly =
                            Dict.onlyBothOnly attrTypes extensionAttrTypes

                        Dict.each both fn name, inAttr & extAttr:
                            addEquality env pos 'why_Record inAttr extAttr @state

                        # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
                        newExtId =
                            newTyvarId @state

                        TA.'typeRecord p ('just newExtId) (Dict.join attrTypes extensionOnly)

                    TA.'typeVar p id:
                        ty =
                            TA.'typeRecord p ('just << newTyvarId @state) attrTypes

                        addEquality env pos 'why_RecordExt extType.raw ty @state

                        ty

                    _:
                        addError env pos 'errorNotCompatibleWithRecord @state

                        TA.'typeError

            TA.'record pos ('just typedExt) typedAttrs & { raw = finalType, uni = inferUni uni extType.uni }


#
# Check
#

checkParameter as fn Env, TA.ParType, CA.Parameter, @State: TA.Parameter & Env =
    fn env, expectedParType, par, @state:
    try par as

        CA.'parameterPattern originalUni pa:
            fullType & (typedPa & env1) =
                try expectedParType as

                    TA.'parRe _:
                        addError env (CA.patternPos pa) 'errorRecyclingDoesNotMatch @state

                        o =
                            inferPattern env 'uni pa @state

                        { raw = o.patternType, uni = 'uni } & (o.typedPattern & o.env)

                    TA.'parSp full:
                        uni =
                            translateUni env.annotatedUnivarsByOriginalId originalUni

                        addErrorIf (uni /= full.uni) env (CA.patternPos pa) ('errorUniquenessDoesNotMatchParameter uni full) @state

                        full & checkPattern env full pa @state

            TA.'parameterPattern fullType typedPa & env1

        CA.'parameterPlaceholder num:
            try expectedParType as

                TA.'parRe _:
                    todo "TA.ParRe"

                TA.'parSp type:
                    variable as Instance =
                        {
                        , definedAt = Pos.'g
                        , freeTyvars = Dict.empty
                        , freeUnivars = Dict.empty
                        , type
                        }

                    TA.'parameterPlaceholder type num & { env with variables = Dict.insert ('refPlaceholder num) variable .variables }

        CA.'parameterRecycle pos name:
            expectedRaw =
                try expectedParType as

                    TA.'parSp full:
                        addError env pos 'errorRecyclingDoesNotMatch @state

                        TA.'typeError

                    TA.'parRe raw:
                        raw

            variable as Instance =
                {
                , definedAt = pos
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type = { raw = expectedRaw, uni = 'uni }
                }

            localEnv as Env =
                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with variables = Dict.insert ('refLocal name) variable .variables }

            TA.'parameterRecycle pos expectedRaw name & localEnv


mergeFunctionRawTypes as fn TA.RawType, TA.RawType: TA.RawType =
    todo "mergeFunctionRawTypes"


CheckExpressionPars =
    {
    , annotatedPattern as TA.Pattern
    , annotation as CA.RawType
    , env as Env
    }


[#
# TODO make expectedType a Maybe and merge with inferExpression?
#]
checkExpression as fn CheckExpressionPars, TA.FullType, CA.Expression, @State: TA.Expression & TA.FullType =
    fn pars, expectedType, caExpression, @state:
    #
    addErrorLocal as fn Text: None =
        fn typeConstraint:
        module =
            getErrorModule pars.env

        annotatedPatternName =
            try pars.annotatedPattern as
                TA.'patternAny _ { with  maybeName = 'just name }: "`" .. name .. "`"
                _: "the pattern"

        annotatedPatternLocation =
            Error.posToHuman module (CA.typePos pars.annotation)

        [
        #
        # astPiece code location is displayed here
        #
        , typeConstraint
        , ""
        , "However the annotation for " .. annotatedPatternName .. ":"
        , ""
        , annotatedPatternLocation.block
        , "says that it must have type"
        , ""
        , "    " .. typeToHuman pars.env expectedType.raw
        , "I need the annotation and the value to have the same type!"
        ]
        # TODO have suggestions for specific type combinations:
        #   Number <=> Text
        #   fn :a => a
        #   ...
        >> Error.'simple module (CA.expressionPos caExpression) __
        >> Array.push @state.errors __

    assertLocal as fn Bool, Text: None =
        fn test, typeConstraint:
        if test then 'none else addErrorLocal typeConstraint

    try caExpression as

        CA.'literalNumber pos n:
            isOk =
                try expectedType.raw as
                    TA.'typeExact _ typeUsr []: typeUsr == CoreDefs.numberDef.usr
                    _: 'false

            assertLocal isOk "This is a literal number, which means its type is always `Number`."

            TA.'literalNumber pos n & expectedType

        CA.'literalText pos text:
            isOk =
                try expectedType.raw as
                    TA.'typeExact _ typeUsr []: typeUsr == CoreDefs.textDef.usr
                    _: 'false

            assertLocal isOk "This is a literal text, which means its type is always `Text`."

            TA.'literalText pos text & expectedType

        CA.'variable pos ref:
            { with  env } =
                pars

            type =
                try getVariableByRef ref env as

                    'nothing:
                        addError env pos ('errorVariableNotFound ref) @state

                        expectedType

                    'just var:
                        full =
                            generalize env pos ref var @state

                        checkUni env pos { given = full.uni, required = expectedType.uni } @state

                        addEquality env pos 'why_Annotation full.raw expectedType.raw @state

                        full

            TA.'variable pos ref & type

        CA.'constructor pos usr:
            type =
                try expectedType.raw as

                    TA.'typeExact _ _ _:
                        { with  env } =
                            pars

                        try getConstructorByUsr usr env as

                            'nothing:
                                addError env pos ('errorConstructorNotFound usr) @state

                                expectedType

                            'just cons:
                                full as TA.FullType =
                                    generalize env pos ('refGlobal usr) cons @state

                                addEquality env pos 'why_Annotation full.raw expectedType.raw @state

                                full

                    _:
                        addErrorLocal "This is a literal variant, which means its type must always be a variant type."

                        expectedType

            # Constructor literal is always unique, so no need to check uniqueness

            TA.'constructor pos usr & type

        CA.'fn pos fnPars body:
            try expectedType.raw as

                TA.'typeFn typePos _ parTypes out:
                    if List.length fnPars /= List.length parTypes then
                        addError pars.env pos 'errorWrongNumberOfParameters @state

                        TA.'error pos & expectedType
                    else
                        !typedPars =
                            Array.fromList []

                        !parIndex =
                            0

                        localEnv as Env =
                            pars.env
                            >> List.for __ (List.map2 Tuple.pair fnPars parTypes) fn par & parType, envX:
                                typedPar & envX1 =
                                    checkParameter envX parType par @state

                                Array.push @typedPars typedPar

                                @parIndex += 1

                                envX1

                        typedBody & bodyType =
                            checkExpression { pars with env = localEnv } out body @state

                        @state.lastLambdaRefId += 1

                        lambdaRef =
                            pars.env.currentRootUsr & cloneUni @state.lastLambdaRefId

                        finalType =
                            TA.'typeFn typePos (Set.ofOne lambdaRef) (todo "parTypes") bodyType

                        TA.'fn pos lambdaRef (Array.toList @typedPars) typedBody out & { raw = finalType, uni = expectedType.uni }

                _:
                    addErrorLocal "This expression is a function, which means its type is always a `fn` type."

                    TA.'error pos & expectedType

        CA.'call pos reference args:
            doCall pars.env pos ('just expectedType) reference args @state

        CA.'record pos ('just ext) valueByName:
            try expectedType.raw as

                TA.'typeRecord typePos 'nothing annotatedTypeByName:
                    # ext must have type expectedType
                    typedExt & extType =
                        checkExpression pars expectedType ext @state

                    typedValueByName & populatedTypeByName =
                        Dict.for (Dict.empty & Dict.empty) valueByName fn attrName, attrExpr, vs & ts:
                            # all valueByName attrs must be in annotatedTypeByName
                            v & t =
                                try Dict.get attrName annotatedTypeByName as

                                    'nothing:
                                        addError pars.env pos ('errorRecordHasAttributesNotInAnnotation [ attrName ]) @state

                                        # This is not super clean, but since we're throwing an error anyway it's probably fine
                                        inferExpression pars.env attrExpr @state

                                    'just attrType:
                                        checkExpression pars { raw = attrType, uni = expectedType.uni } attrExpr @state

                            Dict.insert attrName v vs & Dict.insert attrName t.raw ts

                    type as TA.RawType =
                        mergeFunctionRawTypes extType.raw (TA.'typeRecord typePos 'nothing populatedTypeByName)

                    finalExpr as TA.Expression =
                        TA.'record pos ('just typedExt) typedValueByName

                    finalType as TA.FullType =
                        { raw = type, uni = expectedType.uni }

                    finalExpr & finalType

                _:
                    addErrorLocal "This is a literal record, which means its type is always a record type."

                    TA.'error pos & expectedType

        CA.'record pos 'nothing valueByName:
            try expectedType.raw as

                TA.'typeRecord typePos 'nothing typeByName:
                    aOnly & both & bOnly =
                        Dict.onlyBothOnly valueByName typeByName

                    if aOnly /= Dict.empty then
                        addError pars.env pos ('errorRecordHasAttributesNotInAnnotation (Dict.keys aOnly)) @state
                    else if bOnly /= Dict.empty then
                        addError pars.env pos ('errorRecordIsMissingAttibutesInAnnotation (Dict.keys bOnly)) @state
                    else
                        'none

                    typedAttrs & attrTypes =
                        Dict.for (Dict.empty & Dict.empty) both fn name, value & type, vs & ts:
                            v & t =
                                checkExpression pars { raw = type, uni = expectedType.uni } value @state

                            Dict.insert name v vs & Dict.insert name t.raw ts

                    TA.'record pos 'nothing typedAttrs & { uni = expectedType.uni, raw = TA.'typeRecord typePos 'nothing attrTypes }

                _:
                    addErrorLocal "This is a literal record, which means its type is always a record type."

                    TA.'error pos & expectedType

        CA.'recordAccess pos attrName exp:
            typedExpression & expressionType =
                inferExpression pars.env exp @state

            newId =
                newTyvarId @state

            requiredType =
                expectedType.raw
                >> Dict.ofOne attrName __
                >> TA.'typeRecord pos ('just newId) __

            addEquality pars.env pos 'why_RecordAccess expressionType.raw requiredType @state

            checkUni pars.env pos { given = expressionType.uni, required = expectedType.uni } @state

            TA.'recordAccess pos attrName typedExpression & todo "recordAccessXXXX"

        CA.'letIn def rest:
            typedDef & defEnv =
                {
                , directDeps = Dict.empty
                , maybeBody = 'just def.body
                , pattern = def.pattern
                , uni = def.uni
                }
                >> doDefinition 'refLocal pars.env __ @state

            typedRest & restType =
                checkExpression { pars with env = defEnv } expectedType rest @state

            TA.'letIn typedDef typedRest expectedType & restType

        CA.'if pos { condition, false, true }:
            typedCondition & conditionType =
                inferExpression pars.env condition @state

            addEquality pars.env pos 'why_IfCondition coreTypeBool conditionType.raw @state

            typedTrue & trueType =
                checkExpression pars expectedType true @state

            typedFalse & falseType =
                checkExpression pars expectedType false @state

            finalExpr =
                TA.'if
                    pos
                    {
                    , condition = typedCondition
                    , false = typedFalse
                    , true = typedTrue
                    }

            finalType =
                todo "mergeFunctionRawTypes trueType falseType"

            finalExpr & finalType

        CA.'try pos { patternsAndExpressions, value }:
            typedExp & fullType =
                doTry pars.env pos expectedType.raw value patternsAndExpressions @state

            checkUni pars.env pos { given = fullType.uni, required = expectedType.uni } @state

            typedExp & fullType

        CA.'introspect pos _ _:
            todo "checkExpression 'introspect"


doCall as fn Env, Pos, Maybe TA.FullType, CA.Expression, [ CA.Argument ], @State: TA.Expression & TA.FullType =
    fn env, pos, maybeExpectedType, reference, givenArgs, @state:
    # `reference givenArg1 givenArg2 ...` must be of `expectedType`

    typedReference & inferredReferenceType =
        inferExpression env reference @state

    typedArguments as [ TA.Argument ] =
        givenArgs >> List.map (fn arg: inferArgument env arg @state) __

    toTypeArg as fn TA.Argument: TA.ParType =
        fn arg:
        try arg as
            TA.'argumentExpression full _: TA.'parSp full
            TA.'argumentRecycle _ raw _ _: TA.'parRe raw

    expectedReturnType =
        try inferredReferenceType.raw as

            TA.'typeFn _ _ parTypes outType:
                given =
                    List.length typedArguments

                expected =
                    List.length parTypes

                if expected /= given then
                    addError env pos ('errorWrongNumberOfArguments { expected, given, reference }) @state

                    fullTypeError
                else
                    List.indexedEach2 typedArguments parTypes fn index, givenArg, parType:
                        try givenArg & parType as

                            TA.'argumentRecycle p givenRaw attrPath name & TA.'parRe inferredRaw:
                                try getVariableByRef ('refLocal name) env as

                                    'nothing:
                                        addError env p ('errorVariableNotFound ('refLocal name)) @state

                                    'just instance:
                                        addErrorIf (instance.type.uni /= 'uni) env p 'errorShouldBeUnique @state

                                        addEquality env pos ('why_Argument index) givenRaw inferredRaw @state

                            TA.'argumentExpression givenFull expr & TA.'parSp inferredFull:
                                checkUni env pos { given = givenFull.uni, required = inferredFull.uni } @state

                                # TODO The order [inferredFull.raw, givenFull.raw] is important if the raw contains a function!!!
                                # This is **SUPER** brittle (there are probably bugs caused by the inversion of the two comparison terms at some point...)
                                addEquality env pos ('why_Argument index) inferredFull.raw givenFull.raw @state

                            _:
                                addError env pos 'errorUniquenessDoesNotMatchArgument @state

                    try maybeExpectedType as

                        'nothing:
                            outType

                        'just e:
                            checkUni env pos { given = outType.uni, required = e.uni } @state

                            addEquality env pos 'why_Annotation outType.raw e.raw @state

                            e

            TA.'typeVar p id:
                returnType =
                    try maybeExpectedType as

                        'just e:
                            e

                        'nothing:
                            # TODO: `Imm` here is completely arbitrary
                            { raw = newRawType @state, uni = 'imm }

                refTy =
                    TA.'typeFn p (List.map toTypeArg typedArguments) returnType

                addEquality env pos 'why_CalledAsFunction refTy inferredReferenceType.raw @state

                returnType

            TA.'typeError:
                fullTypeError

            z:
                addError env pos ('errorCallingANonFunction z) @state

                fullTypeError

    TA.'call pos typedReference typedArguments & expectedReturnType


inferArgument as fn Env, CA.Argument, @State: TA.Argument =
    fn env, arg, @state:
    try arg as

        CA.'argumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            TA.'argumentExpression expType typedExp

        CA.'argumentRecycle pos name attrPath:
            ref =
                'refLocal name

            raw =
                try getVariableByRef ref env as

                    'nothing:
                        addError env pos ('errorVariableNotFound ref) @state

                        TA.'typeError

                    'just var:
                        var.type.raw
                        >> List.for __ attrPath fn attrName, tyAcc:
                            inferRecordAccess env pos attrName tyAcc @state

            TA.'argumentRecycle pos raw attrPath name


#
#
# Patterns
#
#
PatternOut =
    {
    # TODO if it is given a context where the pattern is, then this env will contain that context and it's kind of bad?
    # Maybe the function should be given the context as its own param?
    , env as Env
    , maybeFullAnnotation as Maybe CA.Annotation
    , patternType as TA.RawType
    , typedPattern as TA.Pattern
    }


inferPattern as fn Env, Uniqueness, CA.Pattern, @State: PatternOut =
    fn env, uni, pattern, @state:
    try pattern as

        CA.'patternAny pos maybeName maybeAnn:
            inferPatternAny env pos uni maybeName maybeAnn @state

        CA.'patternLiteralText pos text:
            {
            , env
            , maybeFullAnnotation = 'nothing
            , patternType = coreTypeText
            , typedPattern = TA.'patternLiteralText pos text
            }

        CA.'patternLiteralNumber pos n:
            {
            , env
            , maybeFullAnnotation = 'nothing
            , patternType = coreTypeNumber
            , typedPattern = TA.'patternLiteralNumber pos n
            }

        CA.'patternConstructor pos usr arguments:
            argumentOuts & newEnv =
                [] & env
                >> List.forReversed __ arguments fn arg, argOuts & envX:
                    out =
                        inferPattern envX uni arg @state

                    (out :: argOuts) & out.env

            typedArguments =
                List.map (fn out: out.typedPattern) argumentOuts

            argumentTypes =
                List.map (fn out: out.patternType) argumentOuts

            finalType =
                try getConstructorByUsr usr env as

                    'nothing:
                        addError env pos ('errorConstructorNotFound usr) @state

                        TA.'typeError

                    'just cons:
                        x as TA.FullType =
                            generalize env pos ('refGlobal usr) cons @state

                        parTypes & returnType =
                            try x.raw as
                                TA.'typeFn _ ins out: ins & out.raw
                                _: [] & x.raw

                        addErrorIf (List.length parTypes /= List.length arguments) env pos 'errorWrongNumberOfConstructorArguments @state

                        List.indexedEach2 parTypes argumentTypes fn index, parType, argType:
                            try parType as

                                TA.'parRe raw:
                                    bug "cons can't recycle?!"

                                TA.'parSp full:
                                    # TODO -----> check unis
                                    addEquality env pos ('why_Argument index) full.raw argType @state

                        returnType

            {
            , env = newEnv
            , maybeFullAnnotation = 'nothing
            , patternType = finalType
            , typedPattern = TA.'patternConstructor pos usr typedArguments
            }

        #TODO
        CA.'patternRecord pos completeness pas:
            outs & newEnv =
                Dict.empty & env
                >> Dict.for __ pas fn name, pa, dict & envX:
                    out =
                        inferPattern envX uni pa @state

                    Dict.insert name out dict & out.env

            patternExt as Maybe TA.TyvarId =
                try completeness as
                    CA.'complete: 'nothing
                    CA.'partial: 'just (newTyvarId @state)

            raw =
                TA.'typeRecord pos patternExt (outs >> Dict.map (fn name, out: out.patternType) __)

            {
            , env = newEnv
            , maybeFullAnnotation = 'nothing
            , patternType = raw
            , typedPattern = TA.'patternRecord pos (outs >> Dict.map (fn k, o: o.typedPattern & o.patternType) __)
            }


# TODO
inferPatternAny as fn Env, Pos, Uniqueness, Maybe Name, Maybe CA.Annotation, @State: PatternOut =
    fn baseEnv, pos, uni, maybeName, maybeAnnotation, @state:
    (raw as TA.RawType) & (envWithAnnotations as Env) =
        try maybeAnnotation as

            'nothing:
                newRawType @state & baseEnv

            'just annotation:
                annotatedTyvarsByName =
                    Dict.for baseEnv.annotatedTyvarsByName annotation.tyvars fn name, { nonFn }, acc:
                        if Dict.member name acc then
                            acc
                        else
                            Dict.insert name (newTyvarId @state) acc

                annotatedUnivarsByOriginalId =
                    Dict.for baseEnv.annotatedUnivarsByOriginalId annotation.univars fn id, _, acc:
                        if Dict.member id acc then
                            acc
                        else
                            Dict.insert id (newTyvarId @state) acc

                # TODO should we test annotation uni vs the pattern uni (which is not available in this fn?)
                newEnv =
                    { baseEnv with annotatedTyvarsByName, annotatedUnivarsByOriginalId }

                translateAnnotationType newEnv @state annotation.raw & newEnv

    type as TA.FullType =
        { raw, uni }

    envWithVariable as Env =
        try maybeName as

            'nothing:
                envWithAnnotations

            'just name:
                variable as Instance =
                    {
                    , definedAt = pos
                    # TODO Why are these empty? Did I decide that I'm not generalizing non-root values?
                    , freeTyvars =
                        Dict.empty
                    , freeUnivars = Dict.empty
                    , type
                    }

                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { envWithAnnotations with
                , variables = Dict.insert ('refLocal name) variable .variables
                }

    typedPattern =
        TA.'patternAny pos { maybeName, type }

    {
    , env = envWithVariable
    , maybeFullAnnotation = maybeAnnotation
    , patternType = raw
    , typedPattern
    }


checkPattern as fn Env, TA.FullType, CA.Pattern, @State: TA.Pattern & Env =
    fn env, expectedType, pattern, @state:
    try pattern & expectedType.raw as

        CA.'patternAny pos maybeName maybeAnnotation & _:
            newEnv as Env =
                try maybeName as

                    'nothing:
                        env

                    'just name:
                        variable as Instance =
                            {
                            , definedAt = pos
                            , freeTyvars = Dict.empty
                            , freeUnivars = Dict.empty
                            , type = expectedType
                            }

                        # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                        { env with
                        , variables = Dict.insert ('refLocal name) variable .variables
                        }

            TA.'patternAny pos { maybeName, type = expectedType } & newEnv

        CA.'patternLiteralText pos text & TA.'typeExact _ typeUsr []:
            addErrorIf (typeUsr /= CoreDefs.textDef.usr) env pos ('errorIncompatiblePattern pattern expectedType) @state

            TA.'patternLiteralText pos text & env

        CA.'patternLiteralNumber pos text & TA.'typeExact _ typeUsr []:
            addErrorIf (typeUsr /= CoreDefs.numberDef.usr) env pos ('errorIncompatiblePattern pattern expectedType) @state

            TA.'patternLiteralNumber pos text & env

        CA.'patternConstructor pos usr arguments & _:
            checkPatternConstructor env pos expectedType usr arguments @state

        CA.'patternRecord pos completeness pas & _:
            checkPatternRecord env pos expectedType completeness pas @state


checkPatternRecord as fn Env, Pos, TA.FullType, CA.PatternCompleteness, Dict Name CA.Pattern, @State: TA.Pattern & Env =
    fn env, pos, expectedType, completeness, pas, @state:
    { with  uni } =
        expectedType

    try expectedType.raw as

        TA.'typeRecord _ 'nothing attrs:
            paOnly & both & typeOnly =
                Dict.onlyBothOnly pas attrs

            addErrorIf (paOnly /= Dict.empty) env pos ('errorRecordHasAttributesNotInAnnotation (Dict.keys paOnly)) @state

            addErrorIf
                (typeOnly /= Dict.empty and completeness == CA.'complete)
                env
                pos
                (# TODO "add `with` if you don't want to use all attrs"
                 'errorRecordIsMissingAttibutesInAnnotation (Dict.keys typeOnly)
                )
                @state

            taPas & envF =
                Dict.empty & env
                >> Dict.for __ both fn name, pa & raw, acc & envX:
                    taPa & envX0 =
                        checkPattern { envX with context = 'context_AttributeName name env.context } { raw, uni } pa @state

                    Dict.insert name (taPa & raw) acc & { envX0 with context = env.context }

            TA.'patternRecord pos taPas & envF

        TA.'typeRecord _ ('just tyvarId) a:
            bug "can't annotate extensible types"

        _:
            addError env pos 'errorNotCompatibleWithRecord @state

            envF =
                env
                >> Dict.for __ pas fn name, pa, envX:
                    out =
                        inferPattern envX expectedType.uni pa @state

                    out.env

            patternError pos & envF


checkPatternConstructor as fn Env, Pos, TA.FullType, USR, [ CA.Pattern ], @State: TA.Pattern & Env =
    fn env, pos, expectedType, usr, arguments, @state:
    insertArgsOnError as fn Env: Env =
        List.for __ arguments fn arg, envX:
            out =
                inferPattern envX expectedType.uni arg @state

            out.env

    try getConstructorByUsr usr env as

        'nothing:
            addError env pos ('errorConstructorNotFound usr) @state

            patternError pos & insertArgsOnError env

        'just instance:
            # We know already that this constructor's uniqueness is expectedType.uni,
            # so we can skip its generalization
            fullType_ =
                generalize env pos ('refGlobal usr) { instance with freeUnivars = Dict.empty } @state

            fullType =
                { fullType_ with raw = replaceUnivarRec 1 expectedType.uni .raw }

            (requiredParTypes as [ TA.ParType ]) & (requiredOut as TA.FullType) =
                try fullType.raw as
                    TA.'typeFn _ ax o: ax & o
                    _: [] & fullType

            if List.length arguments /= List.length requiredParTypes then
                addError env pos 'errorWrongNumberOfConstructorArguments @state

                patternError pos & insertArgsOnError env
            else
                checkArg as fn CA.Pattern & TA.ParType, Env & [ TA.Pattern ]: Env & [ TA.Pattern ] =
                    fn arg & parType, envX & args:
                        taArg & envX1 =
                            try parType as
                                TA.'parSp full: checkPattern envX full arg @state
                                TA.'parRe raw: bug "should not happen???"

                        envX1 & (taArg :: args)

                (newEnv as Env) & (typedArgs as [ TA.Pattern ]) =
                    env & [] >> List.forReversed __ (List.map2 Tuple.pair arguments requiredParTypes) checkArg

                addEquality env pos 'why_CalledAsFunction requiredOut.raw expectedType.raw @state

                # TODO check that args uni are the same or castable as expectedType.uni

                TA.'patternConstructor pos usr typedArgs & newEnv


#
#
# Module
#
#
insertAnnotatedAndNonAnnotated as fn Name, CA.ValueDef, [ CA.ValueDef ] & [ CA.ValueDef ]: [ CA.ValueDef ] & [ CA.ValueDef ] =
    fn pa, def, ann & nonAnn:
    if def.maybeAnnotation /= 'nothing then
        (def :: ann) & nonAnn
    else
        ann & (def :: nonAnn)


doRootDefinition as fn @Int, @Array Error, USR, Env, CA.ValueDef: Env =
    fn @lastUnificationVarId, @errors, usr, envRaw, def:
    env0 =
        { envRaw with currentRootUsr = usr }

    !state as State =
        initState (cloneUni @lastUnificationVarId)

    'USR umr _ =
        usr

    nameToRef as fn Name: Ref =
        fn name:
        'refGlobal ('USR umr name)

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

    #Debug.benchStart 'none

    typedDef & env1 =
        {
        , directDeps = def.directDeps
        , maybeBody = def.maybeBody
        , pattern = CA.'patternAny def.namePos ('just def.name) def.maybeAnnotation
        , uni = 'imm
        }
        >> doDefinition nameToRef env0 __ @state

    #Debug.benchStop "type inference"

    #Debug.benchStart 'none

    subsAsFns as TA.SubsAsFns =
        {
        , ty = fn tyvarId: Hash.get @state.tyvarSubs tyvarId
        , uni = fn univarId: Hash.get @state.univarSubs univarId
        }

    resolvedValueDef as TA.ValueDef =
        TA.resolveValueDef subsAsFns typedDef

    #Debug.benchStop "def resolution"

    # Update lastUnificationVarId!!
    # TODO we can make this safer once we have a 'reassign' op?
    @lastUnificationVarId := cloneUni @state.lastUnificationVarId

    # Add errors
    # TODO No need to do this, just set
    #   @state.errors := errors
    # and set @error back at the end
    Array.each @state.errors fn err:
        Array.push @errors err

    { env1 with reversedRootValueDefs = [ usr & resolvedValueDef, .reversedRootValueDefs... ] } >> addInstance @lastUnificationVarId @errors umr def __


addInstance as fn @Int, @Array Error, UMR, CA.ValueDef, Env: Env =
    fn @lastUnificationVarId, @errors, umr, def, env:
    #
    # Tyvars
    #

    #??? Why am I reinitializing the state here!?
    !state as State =
        initState (cloneUni @lastUnificationVarId)

    nameToIdAndClasses as Dict Name (TA.TyvarId & TA.Tyvar) =
        zzzz =
            fn tyvarName, { nonFn }:
            newTyvarId @state & { maybeAnnotated = 'just { allowFunctions = nonFn == 'nothing, name = tyvarName } }

        def.maybeAnnotation
        >> Maybe.map (fn ann: Dict.map zzzz ann.tyvars) __
        >> Maybe.withDefault Dict.empty __

    nameToType as Dict Name TA.RawType =
        Dict.map (fn k, id & classes: TA.'typeVar Pos.'g id) nameToIdAndClasses

    tyvarIdToClasses as Dict TA.TyvarId TA.Tyvar =
        nameToIdAndClasses
        >> Dict.values
        >> Dict.fromList

    #
    # Univars
    #
    originalIdToNewIdAndUnivar as Dict UnivarId (UnivarId & TA.Univar) =
        try def.maybeAnnotation as

            'just ann:
                ann.raw
                >> CA.typeUnivars
                >> Dict.map (fn annotatedId, 'none: newTyvarId @state & { annotatedId }) __

            'nothing:
                Dict.empty

    originalIdToUniqueness as Dict UnivarId UnivarId =
        originalIdToNewIdAndUnivar >> Dict.map (fn annotatedId, newId & univar: newId) __

    freeUnivars as Dict UnivarId TA.Univar =
        originalIdToNewIdAndUnivar
        >> Dict.values
        >> Dict.fromList

    #
    # Env
    #
    envF =
        try def.maybeAnnotation as

            'nothing:
                env

            'just annotation:
                raw =
                    translateRawType env nameToType originalIdToUniqueness @state.errors annotation.raw

                instance as Instance =
                    {
                    , definedAt = def.namePos
                    , freeTyvars = Dict.intersect tyvarIdToClasses (TA.typeTyvars raw)
                    # TODO should intersect with the univars actually used by the specific variable
                    , freeUnivars
                    , type = { raw, uni = 'imm }
                    }

                ref as Ref =
                    def.name
                    >> 'USR umr __
                    >> 'refGlobal

                { env with variables = Dict.insert ref instance .variables }

    # TODO I don't like this, need to find a better way

    # Update lastUnificationVarId!!
    # TODO we can make this safer once we have a 'reassign' op?
    @lastUnificationVarId := cloneUni @state.lastUnificationVarId

    # Add errors
    # TODO No need to do this, just set
    #   @state.errors := errors
    # and set @error back at the end
    Array.each @state.errors fn err:
        Array.push @errors err

    envF


addConstructorToGlobalEnv as fn @Array Error, Name, CA.ConstructorDef, Env: Env =
    fn @errors, name, caConstructor, env:
    'USR umr _ =
        caConstructor.variantTypeUsr

    ins =
        caConstructor.ins >> List.map (fn in: CA.'parSp { raw = in, uni = 'depends 1 }) __

    caRaw =
        if ins == [] then
            caConstructor.out
        else
            CA.'typeFn Pos.'g ins { raw = caConstructor.out, uni = 'depends 1 }

    tyvarNamesAndIds as [ Name & TA.TyvarId ] =
        caRaw
        >> CA.typeTyvars
        >> Dict.keys
        >> List.indexedMap (fn index, n: n & -index) __

    paramsByName as Dict Name TA.RawType =
        List.for Dict.empty tyvarNamesAndIds (fn n & id, d: Dict.insert n (TA.'typeVar Pos.'g id) d)

    raw =
        translateRawType env paramsByName Dict.empty @errors caRaw

    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        List.for Dict.empty tyvarNamesAndIds (fn n & id, d: Dict.insert id { maybeAnnotated = 'just { allowFunctions = 'true, name = n } } d)

    taConstructor as Instance =
        {
        , definedAt = Pos.'g
        , freeTyvars
        , freeUnivars = Dict.ofOne 1 { annotatedId = 1 }
        , type = toImm raw
        }

    { env with constructors = Dict.insert ('USR umr name) taConstructor .constructors }


expandAndInsertAlias as fn @Array Error, Env, CA.AliasDef, ByUsr ExpandedAlias: ByUsr ExpandedAlias =
    fn @errors, env, aliasDef, aliasAccum:
    pars & typeByName =
        namedParsToIdParsAndDict aliasDef.pars

    originalIdToNewId as Dict UnivarId UnivarId =
        # TODO ----> We should probably do something with these
        Dict.empty

    type as TA.RawType =
        translateRawType { env with expandedAliases = aliasAccum } typeByName originalIdToNewId @errors aliasDef.type

    Dict.insert aliasDef.usr { pars, type } aliasAccum


namedParsToIdParsAndDict as fn [ Name & Pos ]: [ TA.TyvarId ] & Dict Name TA.RawType =
    fn atPars:
    idPars =
        atPars >> List.indexedMap (fn index, atName: -index) __

    typeByName =
        atPars
        >> List.indexedMap (fn index, name & pos: name & TA.'typeVar pos -index) __
        >> Dict.fromList

    idPars & typeByName


getAliasDependencies as fn ByUsr aliasDef, CA.AliasDef: CA.Deps =
    fn allAliases, aliasDef:
    aliasDef.directDeps >> Dict.filter (fn usr, _: Dict.member usr allAliases) __


#
#
# Uniqueness constraints resolution
#
#
addSub as fn UnivarId, Uniqueness, @Hash UnivarId Uniqueness: None =
    fn newId, newUni, @subs:
    !newSubs as Hash UnivarId Uniqueness =
        Hash.fromList []

    replace =
        fn uni:
        try uni as
            'depends id: if id == newId then newUni else uni
            _: uni

    Hash.each @subs fn univarId, uniqueness:
        Hash.insert @newSubs univarId (replace uniqueness)

    Hash.insert @newSubs newId newUni

    @subs := newSubs


solveUniquenessConstraint as fn Env, UnivarEquality, @State: None =
    fn env, eq, @state:
    try Hash.get @state.univarSubs eq.id as

        'nothing:
            addSub eq.id eq.uni @state.univarSubs

        'just subUni:
            if subUni == eq.uni then
                # Nothing to do =)
                'none
            else
                try subUni & eq.uni as
                    'depends subId & _: addSub subId eq.uni @state.univarSubs
                    _ & 'depends newId: addSub newId subUni @state.univarSubs
                    _: addError env eq.pos ('errorUnresolvableUniqueness eq subUni) @state


#
#
# Equalities resolution
#
#
addErError as fn Env, Equality, Text, @State: None =
    fn env, equality, message, @state:
    { with  context, pos, type1, type2, why } =
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
    >> Error.'simple (getErrorModule env) pos __
    >> Array.push @state.errors __


addErErrorIf as fn Env, Bool, Equality, Text, @State: None =
    fn env, test, equality, message, @state:
    if test then addErError env equality message @state else 'none


compareParTypes as fn Env, Equality, Int, TA.ParType, TA.ParType, @State: None =
    fn env, currentEquality, index, p1, p2, @state:
    { with  context, expandedRecursives, pos, why } =
        currentEquality

    try p1 & p2 as

        TA.'parRe raw1 & TA.'parRe raw2:
            {
            , context
            , expandedRecursives
            , pos
            , type1 = applyAllSubs @state raw1
            , type2 = applyAllSubs @state raw2
            , why = 'why_FunctionInput index why
            }
            >> solveEquality env __ @state

        TA.'parSp full1 & TA.'parSp full2:
            {
            , context
            , expandedRecursives
            , pos
            , type1 = applyAllSubs @state full1.raw
            , type2 = applyAllSubs @state full2.raw
            , why = 'why_FunctionInput index why
            }
            >> solveEquality env __ @state

            try uniCanBeCastTo { given = full1.uni, required = full2.uni } as
                'canBeCastYes: 'none
                'canBeCastNo []: addErError env currentEquality ("Function call par " .. Text.fromNumber index .. " with wrong uniqueness") @state
                'canBeCastNo [ id & uni, tail... ]: solveUniquenessConstraint env { context, id, pos, uni, why = "fn arg" } @state

            'none

        _:
            addErError env { currentEquality with why = 'why_FunctionInput index why } "recycling does not match" @state


#
# This turns an Equality into substitutions and errors, added to @State
#
solveEquality as fn Env, Equality, @State: None =
    fn env, head, @state:
    { context, expandedRecursives, pos, type1, type2, why } =
        head

    try type1 & type2 as

        TA.'typeVar _ tyvarId & t2:
            replaceUnificationVariable env head tyvarId t2 @state

        t1 & TA.'typeVar _ tyvarId:
            replaceUnificationVariable env head tyvarId t1 @state

        TA.'typeExact _ usr1 args1 & TA.'typeExact _ usr2 args2:
            if usr1 /= usr2 then
                addErError env head "types are incompatible2" @state
            else
                List.indexedEach2 args2 args1 fn index, raw1, raw2:
                    { head with
                    , type1 = raw1
                    , type2 = raw2
                    , why = 'why_TypeArgument usr1 index why
                    }
                    >> solveEquality env __ @state

                'none

        TA.'typeFn _ pars1 out1 & TA.'typeFn _ pars2 out2:
            if List.length pars1 /= List.length pars2 then
                addErError env head "functions expect a different number of arguments" @state
            else
                solveEquality env { head with type1 = out1.raw, type2 = out2.raw, why = 'why_FunctionOutput why } @state

                # TODO there is not much guarantee which one is given and which one is required
                bleh =
                    try uniCanBeCastTo { given = out2.uni, required = out1.uni } as
                        'canBeCastYes: 'none
                        'canBeCastNo []: addErError env head "the function return type have different uniqueness" @state
                        'canBeCastNo [ id & uni, tail... ]: solveUniquenessConstraint env { context, id, pos, uni, why = "fn out" } @state

                List.indexedEach2 pars1 pars2 (compareParTypes env head __ __ __ @state)

        TA.'typeRecord _ 'nothing attrs1 & TA.'typeRecord _ 'nothing attrs2:
            only1 & both & only2 =
                Dict.onlyBothOnly attrs1 attrs2

            Dict.each both fn attrName, attrType1 & attrType2:
                solveEquality env { head with type1 = attrType1, type2 = attrType2, why = 'why_Attribute why } @state

            addErErrorIf env (only1 /= Dict.empty or only2 /= Dict.empty) head "record attrs don't match" @state

        TA.'typeRecord _ ('just tyvar1) attrs1 & TA.'typeRecord _ 'nothing attrs2:
            solveRecordExt env head 'false tyvar1 attrs1 attrs2 @state

        TA.'typeRecord _ 'nothing attrs1 & TA.'typeRecord _ ('just tyvar2) attrs2:
            # The True is to swap the terms when we insert the equality, so that we don't mix given and required
            solveRecordExt env head 'true tyvar2 attrs2 attrs1 @state

        TA.'typeRecord p ('just tyvar1) attrs1 & TA.'typeRecord _ ('just tyvar2) attrs2:
            only1 & both & only2 =
                Dict.onlyBothOnly attrs1 attrs2

            newExtId =
                newTyvarId @state

            newType =
                TA.'typeRecord p ('just newExtId) (Dict.join attrs1 only2)

            replaceUnificationVariable env head tyvar1 newType @state

            replaceUnificationVariable env head tyvar2 newType @state

            Dict.each both fn name, t1 & t2:
                solveEquality env { context = 'context_AttributeName name context, expandedRecursives, pos, type1 = t1, type2 = t1, why } @state

        TA.'typeError & _:
            'none

        _ & TA.'typeError:
            'none

        _:
            addErError env head "types are incompatible1" @state


solveRecordExt as fn Env, Equality, Bool, TA.TyvarId, Dict Name TA.RawType, Dict Name TA.RawType, @State: None =
    fn env, equality, swapEquality, tyvar1, attrs1, attrs2, @state:
    #
    # { tyvar1 with attrs1 } == { attrs2 }
    #
    # =>    tyvar1 == { attrs2 }
    #
    # =>    all of attrs1 must be in attrs2 and match
    #

    { with  context, pos, why } =
        equality

    Dict.each attrs1 fn name, type1:
        try Dict.get name attrs2 as

            'nothing:
                addErError env equality ("missing attribute " .. name) @state

            'just type2:
                a & b =
                    if swapEquality then type2 & type1 else type1 & type2

                { equality with
                , context = 'context_AttributeName name context
                , type1 = a
                , type2 = b
                }
                >> solveEquality env __ @state

    replaceUnificationVariable env equality tyvar1 (TA.'typeRecord pos 'nothing attrs2) @state


replaceUnificationVariable as fn Env, Equality, TA.TyvarId, TA.RawType, @State: None =
    fn env, equality, tyvarId, replacingType, @state:
    isSame =
        try replacingType as
            TA.'typeVar _ tyvarId2: tyvarId == tyvarId2
            _: 'false

    if isSame then
        'none
    else if occurs tyvarId replacingType then
        addErError env equality "circular!?" @state
    else
        !new =
            Hash.fromList []

        Hash.each @state.tyvarSubs fn tId, rawType:
            Hash.insert @new tId (applySubstitutionToType tyvarId replacingType rawType)

        Hash.insert @new tyvarId replacingType

        @state.tyvarSubs := new


occurs as fn TA.TyvarId, TA.RawType: Bool =
    fn tyvarId, type:
    rec =
        occurs tyvarId __

    try type as
        TA.'typeFn _ ins out: List.any (fn t: t >> TA.toRaw >> rec) ins or rec out.raw
        TA.'typeVar _ id: id == tyvarId
        TA.'typeExact _ usr args: List.any rec args
        TA.'typeRecord _ _ attrs: Dict.any (fn k, v: rec v) attrs
        TA.'typeError: 'false


applySubstitutionToType as fn TA.TyvarId, TA.RawType, TA.RawType: TA.RawType =
    fn tyvarId, replacingType, originalType:
    subsAsFns as TA.SubsAsFns =
        {
        , ty = fn id: if id == tyvarId then 'just replacingType else 'nothing
        , uni = fn _: 'nothing
        }

    TA.resolveRaw subsAsFns originalType


applyAllSubs as fn @State, TA.RawType: TA.RawType =
    fn @state, raw:
    subsAsFns as TA.SubsAsFns =
        {
        , ty = fn id: Hash.get @state.tyvarSubs id
        , uni = fn id: Hash.get @state.univarSubs id
        }

    TA.resolveRaw subsAsFns raw
