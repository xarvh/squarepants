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


newAddError as fn @State, Env, Pos, Text: None =
    fn @state, env, pos, message:
    [
    , message
    , "[[context: " .. Debug.toHuman env.context .. "]]"
    ]
    >> Error.'simple (getErrorModule env) pos __
    >> Array.push @state.errors __


newAddErrorIf as fn Bool, @State, Env, Pos, Text: None =
    fn cond, @state, env, pos, message:
    if cond then
        newAddError @state env pos message
    else
        'none


newAddErrorMaybe as fn Maybe a, @State, Env, Pos, (fn a: Text): None =
    fn maybe, @state, env, pos, getMessage:
    try maybe as
        'nothing: 'none
        'just errorPayload: newAddError @state env pos (getMessage errorPayload)


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
        fn r, originalUnivarId, _:
        newUnivarId =
            newTyvarId @state

        #Hash.insert @state.univarsById newUnivarId { originalId = originalUnivarId, constraints = []}
        replaceUnivarRec originalUnivarId ('depends newUnivarId) r

    raw =
        instance.type.raw
        >> Dict.for __ instance.freeUnivars replaceUnivar
        >> Dict.for __ instance.freeTyvars fn a, originalTyvarId, tyvar:
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
            TA.'typeExact p usr (List.map args doRaw)

        TA.'typeRecord p maybeExt attrs:
            TA.'typeRecord p maybeExt (Dict.map attrs doRaw)

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

            TA.'typeFn p (List.map ins mapPar) { raw = doRaw out.raw, uni = doUni out.uni }


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
            TA.'typeExact p usr (List.map args rec)

        TA.'typeFn p ins out:
            TA.'typeFn p (TA.mapPars rec ins) { out with raw = rec .raw }

        TA.'typeRecord p 'nothing attrs:
            TA.'typeRecord p 'nothing (Dict.map attrs rec)

        TA.'typeVar p id:
            try Dict.get id tyvarIdsToType as
                'nothing: bug "this is not supposed to happen"
                'just ty: ty

        TA.'typeRecord p ('just id) attrs:
            TA.'typeRecord p ('just id) (Dict.map attrs rec)

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
            taArgs as [ TA.ParType ] =
                List.map caPars fn caPar:
                    try caPar as
                        CA.'parRe caRaw: TA.'parRe (rec caRaw)
                        CA.'parSp caFull: TA.'parSp (translateFullType env argsByName originalIdToNewId @errors caFull)

            TA.'typeFn pos taArgs (translateFullType env argsByName originalIdToNewId @errors caOut)

        CA.'typeRecord pos caAttrs:
            TA.'typeRecord pos 'nothing (Dict.map caAttrs rec)

        CA.'typeAnnotationVariable pos name:
            try Dict.get name argsByName as

                'nothing:
                    addErrorE env pos ('errorUndefinedTypeVariable name) @errors

                    TA.'typeError

                'just raw:
                    raw

        CA.'typeNamed pos usr pars:
            expandedPars as [ TA.RawType ] =
                List.map pars rec

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
                            List.map2 expandedAlias.pars expandedPars Tuple.pair >> Dict.fromList

                        expandTyvarsInType tyvarIdsToType expandedAlias.type


translateAnnotationType as fn Env, @State, CA.RawType: TA.RawType =
    fn env, @state, ca:
    nameToType =
        Dict.map env.annotatedTyvarsByName (TA.'typeVar Pos.'g __)

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


doDefinition as fn (fn Name: Ref), Env, CA_ValueDef, @State: TA.ValueDef & Env =
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

    typedBody & bodyType =
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

                        doExpression @state localEnv ('just full) body >> Tuple.mapFirst 'just __

                    'nothing:
                        typed & inferredType =
                            doExpression @state localEnv 'nothing body

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
        >> List.filterMap __ (fn entry: entry.maybeAnnotation)
        >> List.for Dict.empty __ (fn acc, annotation: Dict.join annotation.univars acc)
        >> Dict.for Dict.empty __ fn acc, annotatedId, 'none:
            try Dict.get annotatedId localEnv.annotatedUnivarsByOriginalId as
                'nothing: acc
                'just newId: Dict.insert acc newId { annotatedId }

    # TODO explain what is happening here
    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        # TODO: not all tyvars are bindable.
        # for example, in `fn a: a` a would be free unless it also appears somewhere else?
        allBindableTyvarsIn as fn TA.TyvarId: Set TA.TyvarId =
            fn tyvarId:
            try Hash.get @state.tyvarSubs tyvarId as
                'nothing: Set.empty
                'just raw: Dict.map (TA.typeTyvars raw) (fn v: 'none)

        resolvedParentBoundTyvars as Set TA.TyvarId =
            Hash.for_ Set.empty @parentBoundTyvars fn parentTyvar, _, set:
                set
                >> Set.insert parentTyvar __
                >> Set.for __ (allBindableTyvarsIn parentTyvar) Set.insert

        tyvarAnnotatedNameByTyvarId =
            Dict.for Dict.empty localEnv.annotatedTyvarsByName (fn acc, name, tyvarId: Dict.insert acc tyvarId name)

        defType.raw
        >> TA.typeTyvars
        >> Dict.for Dict.empty __ fn acc, id, _:
            if Set.member id resolvedParentBoundTyvars then
                acc
            else
                try Dict.get id tyvarAnnotatedNameByTyvarId as

                    'nothing:
                        { maybeAnnotated = 'nothing }

                    # TODO: actually set allowFunctions!!!
                    'just name:
                        { maybeAnnotated = 'just { allowFunctions = 'true, name } }
                >> Dict.insert acc id __

    #
    # Add instance
    #
    caNames =
        CA.patternNames def.pattern >> List.indexBy __ (fn e: e.name)

    instance as fn Name, { pos as Pos, type as TA.FullType }: Instance =
        fn name, { pos, type = unresolvedType }:
        type =
            { unresolvedType with raw = applyAllSubs @state .raw }

        typeTyvars =
            TA.typeTyvars type.raw

        actualTyvars =
            Dict.filterWithKey (fn k, v: Dict.has k typeTyvars) freeTyvars

        tryAsStillBreaksIfUsedImperatively =
            try Dict.get name caNames as
                'just { with  maybeAnnotation = 'just annotation, pos = p }: addErrorIf (Dict.length annotation.tyvars > Dict.length actualTyvars) localEnv p ('errorTyvarNotIndependent name) @state
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
        >> Dict.for __ (TA.patternNames patternOut.typedPattern) fn vars, name, stuff:
            Dict.insert vars (nameToRef name) (instance name stuff)

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
# Expression
#
#
doLiteralNumber as fn @State, Env, Maybe TA.FullType, Pos, Int: TA.Expression & TA.FullType =
    fn @state, env, expectedType, pos, n:
    maybeInvalidRaw =
        try expectedType as

            'nothing:
                'nothing

            'just { with  raw = TA.'typeExact p typeUsr [] }:
                if typeUsr == CoreDefs.numberDef.usr then
                    'nothing
                else
                    'just (TA.'typeExact p typeUsr [])

            'just { with  raw }:
                'just raw

    newAddErrorMaybe maybeInvalidRaw @state env pos fn invalidRaw:
        """

        The annotation says that this number should be of type:

        """
        .. typeToHuman env invalidRaw

        """

        However, this is a number literal, which must always be of type `Number` (the one in the core library).

        The two types are not compatible!
        """

    TA.'literalNumber pos n & { raw = coreTypeNumber, uni = 'uni }


doLiteralText as fn @State, Env, Maybe TA.FullType, Pos, Text: TA.Expression & TA.FullType =
    fn @state, env, expectedType, pos, text:
    maybeInvalidRaw =
        try expectedType as

            'nothing:
                'nothing

            'just { with  raw = TA.'typeExact p typeUsr [] }:
                if typeUsr == CoreDefs.textDef.usr then
                    'nothing
                else
                    'just (TA.'typeExact p typeUsr [])

            'just { with  raw }:
                'just raw

    newAddErrorMaybe maybeInvalidRaw @state env pos fn invalidRaw:
        """

        The annotation says that this text should be of type:

        """
        .. typeToHuman env invalidRaw

        """

        However, this is a text literal, which must always be of type `Text` (the one in the core library).

        The two types are not compatible!
        """

    TA.'literalText pos text & { raw = coreTypeText, uni = 'uni }


doVariable =
    fn @state, env, expectedType, pos, ref:
    type =
        try getVariableByRef ref env as

            'nothing:
                addError env pos ('errorVariableNotFound ref) @state

                fullTypeError

            'just variableInstance:
                generalizedType =
                    generalize env pos ref variableInstance @state

                Maybe.map expectedType fn fullType:
                    checkUni env pos { given = generalizedType.uni, required = fullType.uni } @state

                    addEquality env pos 'why_Annotation generalizedType.raw fullType.raw @state

                generalizedType

    TA.'variable pos ref & type


doConstructor =
    fn @state, env, expectedType, pos, usr:
    type =
        try getConstructorByUsr usr env as

            'nothing:
                addError env pos ('errorConstructorNotFound usr) @state

                fullTypeError

            'just cons:
                generalizedType =
                    generalize env pos ('refGlobal usr) cons @state

                # Constructor literal is always unique, so we don't care about uniqueness
                Maybe.map expectedType fn { with  raw }:
                    try raw as

                        TA.'typeExact _ _ _:
                            addEquality env pos 'why_Annotation generalizedType.raw raw @state

                        _:
                            """

                            The annotation says that this variant should be of type:


                            """
                            .. typeToHuman env raw
                            .. """

                            However variant literals must always be of a var(iant) type!

                            The two types are not compatible.

                            """
                            >> newAddError @state env pos __

                generalizedType

    TA.'constructor pos usr & type


doParameter as fn @State, Env, Int, Maybe TA.ParType, CA.Parameter: TA.Parameter & TA.ParType & Env =
    fn @state, env, index, expectedParameterType, caParameter:
    try caParameter as

        CA.'parameterPattern originalUni caPattern:
            try expectedParameterType as

                'nothing:
                    out =
                        inferPattern env originalUni caPattern @state

                    Dict.each (TA.typeTyvars out.patternType) fn tyvarId, _:
                        # Inside the function definition, the tyvars act as bound
                        Hash.insert @state.boundTyvars tyvarId 'none

                    fullType =
                        { raw = out.patternType, uni = originalUni }

                    TA.'parameterPattern fullType out.typedPattern & TA.'parSp fullType & out.env

                'just (TA.'parSp expectedPatternType):
                    uni =
                        translateUni env.annotatedUnivarsByOriginalId originalUni

                    addErrorIf (uni /= expectedPatternType.uni) env (CA.patternPos caPattern) ('errorUniquenessDoesNotMatchParameter uni expectedPatternType) @state

                    taPattern & newEnv =
                        checkPattern env expectedPatternType caPattern @state

                    taParameter as TA.Parameter =
                        TA.'parameterPattern expectedPatternType taPattern

                    taParType as TA.ParType =
                        TA.'parSp expectedPatternType

                    taParameter & taParType & newEnv

                'just (TA.'parRe _):
                    addError env (CA.patternPos caPattern) 'errorRecyclingDoesNotMatch @state

                    o =
                        inferPattern env 'uni caPattern @state

                    taParameter as TA.Parameter =
                        TA.'parameterPattern { raw = o.patternType, uni = 'uni } o.typedPattern

                    taParType as TA.ParType =
                        TA.'parSp fullTypeError

                    taParameter & taParType & o.env

        CA.'parameterRecycle pos name:
            raw =
                try expectedParameterType as

                    'just (TA.'parSp _):
                        addError env pos 'errorRecyclingDoesNotMatch @state

                        TA.'typeError

                    'just (TA.'parRe expectedRaw):
                        expectedRaw

                    'nothing:
                        tyvarId =
                            newTyvarId @state

                        Hash.insert @state.boundTyvars tyvarId 'none

                        TA.'typeVar Pos.'g tyvarId

            variable as Instance =
                {
                , definedAt = pos
                , freeTyvars = Dict.empty
                , freeUnivars = Dict.empty
                , type = { raw, uni = 'uni }
                }

            localEnv as Env =
                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with variables = Dict.insert .variables ('refLocal name) variable }

            TA.'parameterRecycle pos raw name & TA.'parRe raw & localEnv

        CA.'parameterPlaceholder num:
            #
            #     (someFunction a __ b __)
            #     |
            #     V
            #     (fn p1 p2: someFunction a p1 b p2)
            #
            try expectedParameterType as

                'just (TA.'parRe _):
                    todo "TA.ParRe"

                'just (TA.'parSp type):
                    variable as Instance =
                        {
                        , definedAt = Pos.'g
                        , freeTyvars = Dict.empty
                        , freeUnivars = Dict.empty
                        , type
                        }

                    newEnv =
                        { env with variables = Dict.insert .variables ('refPlaceholder num) variable }

                    TA.'parameterPlaceholder type num & TA.'parSp type & newEnv

                'nothing:
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
                        { env with variables = Dict.insert .variables ('refPlaceholder num) instance }

                    TA.'parameterPlaceholder type num & TA.'parSp type & newEnv


doFunction as fn @State, Env, Maybe TA.FullType, Pos, [ CA.Parameter ], CA.Expression: TA.Expression & TA.FullType =
    fn @state, env, expectedType, pos, caParameters, body:
    arity =
        List.length caParameters

    errorToOk as fn Text: TA.Expression & TA.FullType =
        fn errorMessage:
        newAddError @state env pos errorMessage

        TA.'error pos & fullTypeError

    Result.resolveErrorsWith errorToOk fn _:
    try expectedType as

        'just { with  raw = TA.'typeFn _ parameterTypes returnType }:
            List.map parameterTypes 'just & 'just returnType
            #
            >> 'ok

        'just _:
            'err "This expression is a function, which means its type is always a `fn` type."

        'nothing:
            List.repeat arity 'nothing & 'nothing
            #
            >> 'ok
    >> onOk fn expectedParameterTypes & expectedBodyType:
    expectedArity =
        List.length expectedParameterTypes

    if expectedArity /= arity then
        """
        The definition of this function says that it requires
        """
        .. Text.fromNumber arity
        .. """
        arguments.
               However its annotation says that it requires
        """
        .. Text.fromNumber expectedArity
        .. """
        .
                Which one is the correct one?

        """
        >> 'err
    else
        'ok 'none
    >> onOk fn _:
    !typedParameters =
        Array.fromList []

    !parameterTypes =
        Array.fromList []

    [#
      - Get all tyvars in the param types
      - Inside the function body, all these tyvars are treated as bound
      - Outside the function body, the resolved tyvars are free, unless already bound in the parent scope

      ----> At the end of a Definition we take the type tyvars, see which ones are free, then resolve them.
    #]
    bodyEnv =
        List.for2WithIndex env caParameters expectedParameterTypes fn bodyEnvAcc, index, caParameter, expectedParameterType:
            typedParameter & parameterType & envX =
                doParameter @state bodyEnvAcc index expectedParameterType caParameter

            Array.push @typedParameters typedParameter

            Array.push @parameterTypes parameterType

            envX

    typedBody & bodyType =
        doExpression @state bodyEnv expectedBodyType body

    type as TA.FullType =
        {
        , raw = TA.'typeFn pos (Array.toList @parameterTypes) bodyType
        , uni = 'uni
        }

    expression =
        TA.'fn pos (Array.toList @typedParameters) typedBody bodyType

    expression & type >> 'ok


doCall as fn @State, Env, Maybe TA.FullType, Pos, CA.Expression, [ CA.Argument ]: TA.Expression & TA.FullType =
    fn @state, env, expectedType, pos, reference, givenArgs:
    # `reference givenArg1 givenArg2 ...` must be of `expectedType`

    typedReference & inferredReferenceType =
        doExpression @state env 'nothing reference

    typedArguments as [ TA.Argument ] =
        givenArgs >> List.map __ (fn arg: inferArgument env arg @state)

    toTypeArg as fn TA.Argument: TA.ParType =
        fn arg:
        try arg as
            TA.'argumentExpression full _: TA.'parSp full
            TA.'argumentRecycle _ raw _ _: TA.'parRe raw

    expectedReturnType =
        try inferredReferenceType.raw as

            TA.'typeFn _ parTypes outType:
                given =
                    List.length typedArguments

                expected =
                    List.length parTypes

                if expected /= given then
                    addError env pos ('errorWrongNumberOfArguments { expected, given, reference }) @state

                    fullTypeError
                else
                    List.each2WithIndex typedArguments parTypes fn index, givenArg, parType:
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

                    try expectedType as

                        'nothing:
                            outType

                        'just e:
                            checkUni env pos { given = outType.uni, required = e.uni } @state

                            addEquality env pos 'why_Annotation outType.raw e.raw @state

                            e

            TA.'typeVar p id:
                returnType =
                    try expectedType as

                        'just e:
                            e

                        'nothing:
                            # TODO: `Imm` here is completely arbitrary
                            { raw = newRawType @state, uni = 'imm }

                refTy =
                    TA.'typeFn p (List.map typedArguments toTypeArg) returnType

                addEquality env pos 'why_CalledAsFunction refTy inferredReferenceType.raw @state

                returnType

            TA.'typeError:
                fullTypeError

            z:
                addError env pos ('errorCallingANonFunction z) @state

                fullTypeError

    TA.'call pos typedReference typedArguments & expectedReturnType


# TODO: merge in doRecord
inferRecord as fn Env, Pos, Maybe CA.Expression, Dict Name CA.Expression, @State: TA.Expression & TA.FullType =
    fn env, pos, maybeExt, caAttrs, @state:
    taAttrs as Dict Name (TA.Expression & TA.FullType) =
        Dict.map caAttrs (doExpression @state env 'nothing __)

    typedAttrs as Dict Name TA.Expression =
        Dict.map taAttrs Tuple.first

    attrTypes as Dict Name TA.RawType =
        Dict.map taAttrs (fn _ & t: t.raw)

    uni as Uniqueness =
        'uni
        >> Dict.for __ taAttrs fn u, k, _ & full:
            inferUni full.uni u

    try maybeExt as

        'nothing:
            TA.'record pos 'nothing typedAttrs & { raw = TA.'typeRecord pos 'nothing attrTypes, uni }

        'just caExt:
            typedExt & extType =
                doExpression @state env 'nothing caExt

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


doRecord as fn @State, Env, Maybe TA.FullType, Pos, Maybe CA.Expression, Dict Name CA.Expression: TA.Expression & TA.FullType =
    fn @state, env, expectedType, pos, maybeCaExt, caValueByName:
    try maybeCaExt & expectedType as

        _ & 'nothing:
            inferRecord env pos maybeCaExt caValueByName @state

        'just ext & 'just { raw = TA.'typeRecord tp 'nothing typeByName, uni }:
            # ext must have type expectedType
            typedExt & _ =
                doExpression @state env expectedType ext

            # all valueByName attrs must be in typeByName
            typedValueByName as Dict Name TA.Expression =
                Dict.mapWithKey caValueByName fn attrName, attrExpr:
                    try Dict.get attrName typeByName as

                        'nothing:
                            addError env pos ('errorRecordHasAttributesNotInAnnotation [ attrName ]) @state

                            doExpression @state env 'nothing attrExpr

                        'just attrType:
                            fullAttrType =
                                { raw = attrType, uni }

                            doExpression @state env ('just fullAttrType) attrExpr
                    >> Tuple.first

            exp as TA.Expression =
                TA.'record pos ('just typedExt) typedValueByName

            type as TA.FullType =
                { raw = TA.'typeRecord tp 'nothing typeByName, uni }

            exp & type

        'nothing & 'just { raw = TA.'typeRecord tp 'nothing typeByName, uni }:
            aOnly & both & bOnly =
                Dict.onlyBothOnly caValueByName typeByName

            if aOnly /= Dict.empty then
                addError env pos ('errorRecordHasAttributesNotInAnnotation (Dict.keys aOnly)) @state
            else if bOnly /= Dict.empty then
                addError env pos ('errorRecordIsMissingAttibutesInAnnotation (Dict.keys bOnly)) @state
            else
                'none

            typedAttrs as Dict Name TA.Expression =
                Dict.map both (fn value & type: doExpression @state env ('just { raw = type, uni }) value >> Tuple.first)

            fullType as TA.FullType =
                { raw = TA.'typeRecord tp 'nothing typeByName, uni }

            exp as TA.Expression =
                TA.'record pos 'nothing typedAttrs

            exp & fullType

        _ & 'just { with  raw }:
            """

            The annotation says that this record literal should be of type:

            """
            .. typeToHuman env raw

            """

            However records must always be of a record type!

            The two types are not compatible!
            """
            >> newAddError @state env pos __

            TA.'error pos & fullTypeError


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
                        TA.'typeRecord p ('just newExtId) (Dict.insert extensionAttrTypes attrName newAttrType)

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


doRecordAccess as fn @State, Env, Maybe TA.FullType, Pos, Name, CA.Expression: TA.Expression & TA.FullType =
    fn @state, env, maybeExpectedType, pos, attrName, recordExpression:
    typedRecord & recordType =
        doExpression @state env 'nothing recordExpression

    fullType =
        try maybeExpectedType as

            'nothing:
                { recordType with raw = inferRecordAccess env pos attrName .raw @state }

            'just expectedType:
                newId =
                    newTyvarId @state

                requiredType =
                    expectedType.raw
                    >> Dict.ofOne attrName __
                    >> TA.'typeRecord pos ('just newId) __

                addEquality env pos 'why_RecordAccess recordType.raw requiredType @state

                checkUni env pos { given = recordType.uni, required = expectedType.uni } @state

                expectedType

    TA.'recordAccess pos attrName typedRecord & fullType


doIfThenElse as fn @State, Env, Maybe TA.FullType, Pos, CA.Expression, CA.Expression, CA.Expression: TA.Expression & TA.FullType =
    fn @state, env, maybeExpectedType, pos, caCondition, caTrue, caFalse:
    # Should this be a check against bool instead?
    typedCondition & conditionType =
        doExpression @state env 'nothing caCondition

    addEquality env pos 'why_IfCondition coreTypeBool conditionType.raw @state

    typedTrue & trueType =
        doExpression @state env maybeExpectedType caTrue

    typedFalse & falseType =
        doExpression @state env maybeExpectedType caFalse

    fullType =
        try maybeExpectedType as

            'just expectedType:
                expectedType

            'nothing:
                addEquality env pos 'why_IfBranches trueType.raw falseType.raw @state

                # TODO What if "depends" tyvars get resolved to the same?
                # Shouldn't we test this only AFTER solving constraints?
                uni as Uniqueness =
                    inferUni trueType.uni falseType.uni

                { raw = trueType.raw, uni }

    expression =
        TA.'if
            pos
            {
            , condition = typedCondition
            , false = typedFalse
            , true = typedTrue
            }

    expression & fullType


doExpression as fn @State, Env, Maybe TA.FullType, CA.Expression: TA.Expression & TA.FullType =
    fn @state, env, type, exp:
    try exp as

        CA.'literalNumber pos n:
            #
            doLiteralNumber @state env type pos n

        CA.'literalText pos text:
            doLiteralText @state env type pos text

        CA.'variable pos ref:
            doVariable @state env type pos ref

        CA.'constructor pos usr:
            doConstructor @state env type pos usr

        CA.'fn pos fnPars body:
            doFunction @state env type pos fnPars body

        CA.'call pos reference args:
            doCall @state env type pos reference args

        CA.'record pos maybeExt attrs:
            doRecord @state env type pos maybeExt attrs

        CA.'recordAccess pos attrName recordExpression:
            doRecordAccess @state env type pos attrName recordExpression

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
                doExpression @state defEnv type rest

            TA.'letIn typedDef typedRest restType & restType

        CA.'if pos { condition, false, true }:
            doIfThenElse @state env type pos condition true false

        CA.'try pos { patternsAndExpressions, value }:
            try type as
                'nothing: newRawType @state
                'just t: t.raw
            >> doTry env pos __ value patternsAndExpressions @state

        CA.'introspect pos introspect usr:
            doIntrospect @state env pos type introspect usr


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


doIntrospect as fn @State, Env, Pos, Maybe TA.FullType, Token.Introspect, USR: TA.Expression & TA.FullType =
    fn @state, env, pos, expectedType, introspect, usr:
    #
    if expectedType /= 'nothing then
        todo "TODO: checkIntrospect"
    else
        ""

    selfUsr as USR =
        'USR (CoreDefs.makeUmr "Self") "Self"

    selfType as TA.RawType =
        try Dict.get selfUsr env.expandedAliases as
            'nothing: bug "no self?"
            'just expandedAlias: expandedAlias.type

    TA.'introspect usr & { raw = selfType, uni = 'uni }


doTry as fn Env, Pos, TA.RawType, CA.Expression, [ Uniqueness & CA.Pattern & CA.Expression ], @State: TA.Expression & TA.FullType =
    fn env, pos, expectedRaw, value, caPatternsAndExpressions, @state:
    #
    typedValue & valueType =
        doExpression @state env 'nothing value

    uni & patternsAndExpressions =
        'uni & []
        >> List.forReversed __ caPatternsAndExpressions fn uniX & acc, u & pa & exp:
            patternOut as PatternOut =
                inferPattern env u pa @state

            addEquality env pos 'why_TryPattern patternOut.patternType valueType.raw @state

            checkUni env pos { given = valueType.uni, required = u } @state

            newEnv =
                { patternOut.env with
                , context = 'context_TryBranch
                }

            typedExpression & expressionType =
                doExpression @state newEnv 'nothing exp

            addEquality newEnv (CA.expressionPos exp) 'why_TryExpression expectedRaw expressionType.raw @state

            uf =
                inferUni uniX expressionType.uni

            l =
                patternOut.typedPattern & typedExpression :: acc

            uf & l

    TA.'try pos { patternsAndExpressions, value = typedValue, valueType } & { raw = expectedRaw, uni }


inferArgument as fn Env, CA.Argument, @State: TA.Argument =
    fn env, arg, @state:
    try arg as

        CA.'argumentExpression exp:
            typedExp & expType =
                doExpression @state env 'nothing exp

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
                        >> List.for __ attrPath fn tyAcc, attrName:
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
                >> List.forReversed __ arguments fn argOuts & envX, arg:
                    out =
                        inferPattern envX uni arg @state

                    (out :: argOuts) & out.env

            typedArguments =
                List.map argumentOuts (fn out: out.typedPattern)

            argumentTypes =
                List.map argumentOuts (fn out: out.patternType)

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

                        List.each2WithIndex parTypes argumentTypes fn index, parType, argType:
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
                >> Dict.for __ pas fn dict & envX, name, pa:
                    out =
                        inferPattern envX uni pa @state

                    Dict.insert dict name out & out.env

            patternExt as Maybe TA.TyvarId =
                try completeness as
                    CA.'complete: 'nothing
                    CA.'partial: 'just (newTyvarId @state)

            raw =
                TA.'typeRecord pos patternExt (outs >> Dict.map __ (fn out: out.patternType))

            {
            , env = newEnv
            , maybeFullAnnotation = 'nothing
            , patternType = raw
            , typedPattern = TA.'patternRecord pos (outs >> Dict.map __ (fn o: o.typedPattern & o.patternType))
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
                    Dict.for baseEnv.annotatedTyvarsByName annotation.tyvars fn acc, name, { nonFn }:
                        if Dict.has name acc then
                            acc
                        else
                            Dict.insert acc name (newTyvarId @state)

                annotatedUnivarsByOriginalId =
                    Dict.for baseEnv.annotatedUnivarsByOriginalId annotation.univars fn acc, id, _:
                        if Dict.has id acc then
                            acc
                        else
                            Dict.insert acc id (newTyvarId @state)

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
                , variables = Dict.insert .variables ('refLocal name) variable
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
                        , variables = Dict.insert .variables ('refLocal name) variable
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
                >> Dict.for __ both fn acc & envX, name, pa & raw:
                    taPa & envX0 =
                        checkPattern { envX with context = 'context_AttributeName name env.context } { raw, uni } pa @state

                    Dict.insert acc name (taPa & raw) & { envX0 with context = env.context }

            TA.'patternRecord pos taPas & envF

        TA.'typeRecord _ ('just tyvarId) a:
            bug "can't annotate extensible types"

        _:
            addError env pos 'errorNotCompatibleWithRecord @state

            envF =
                env
                >> Dict.for __ pas fn envX, name, pa:
                    out =
                        inferPattern envX expectedType.uni pa @state

                    out.env

            patternError pos & envF


checkPatternConstructor as fn Env, Pos, TA.FullType, USR, [ CA.Pattern ], @State: TA.Pattern & Env =
    fn env, pos, expectedType, usr, arguments, @state:
    insertArgsOnError as fn Env: Env =
        List.for __ arguments fn envX, arg:
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
                checkArg as fn Env & [ TA.Pattern ], CA.Pattern & TA.ParType: Env & [ TA.Pattern ] =
                    fn envX & args, arg & parType:
                        taArg & envX1 =
                            try parType as
                                TA.'parSp full: checkPattern envX full arg @state
                                TA.'parRe raw: bug "should not happen???"

                        envX1 & (taArg :: args)

                (newEnv as Env) & (typedArgs as [ TA.Pattern ]) =
                    env & [] >> List.forReversed __ (List.map2 arguments requiredParTypes Tuple.pair) checkArg

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

    !state as State =
        initState (cloneUni @lastUnificationVarId)

    nameToIdAndClasses as Dict Name (TA.TyvarId & TA.Tyvar) =
        zzzz =
            fn tyvarName, { nonFn }:
            newTyvarId @state & { maybeAnnotated = 'just { allowFunctions = nonFn == 'nothing, name = tyvarName } }

        def.maybeAnnotation
        >> Maybe.map __ (fn ann: Dict.mapWithKey ann.tyvars zzzz)
        >> Maybe.withDefault __ Dict.empty

    nameToType as Dict Name TA.RawType =
        Dict.map nameToIdAndClasses (fn id & classes: TA.'typeVar Pos.'g id)

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
                >> Dict.mapWithKey __ (fn annotatedId, 'none: newTyvarId @state & { annotatedId })

            'nothing:
                Dict.empty

    originalIdToUniqueness as Dict UnivarId UnivarId =
        originalIdToNewIdAndUnivar >> Dict.map __ (fn newId & univar: newId)

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

                { env with variables = Dict.insert .variables ref instance }

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
        List.map caConstructor.ins (fn in: CA.'parSp { raw = in, uni = 'depends 1 })

    caRaw =
        if ins == [] then
            caConstructor.out
        else
            CA.'typeFn Pos.'g ins { raw = caConstructor.out, uni = 'depends 1 }

    tyvarNamesAndIds as [ Name & TA.TyvarId ] =
        caRaw
        >> CA.typeTyvars
        >> Dict.keys
        >> List.mapWithIndex __ (fn index, n: n & -index)

    paramsByName as Dict Name TA.RawType =
        List.for Dict.empty tyvarNamesAndIds (fn d, n & id: Dict.insert d n (TA.'typeVar Pos.'g id))

    raw =
        translateRawType env paramsByName Dict.empty @errors caRaw

    freeTyvars as Dict TA.TyvarId TA.Tyvar =
        List.for Dict.empty tyvarNamesAndIds (fn d, n & id: Dict.insert d id { maybeAnnotated = 'just { allowFunctions = 'true, name = n } })

    taConstructor as Instance =
        {
        , definedAt = Pos.'g
        , freeTyvars
        , freeUnivars = Dict.ofOne 1 { annotatedId = 1 }
        , type = toUni raw
        }

    { env with constructors = Dict.insert .constructors ('USR umr name) taConstructor }


expandAndInsertAlias as fn @Array Error, Env, CA.AliasDef, ByUsr ExpandedAlias: ByUsr ExpandedAlias =
    fn @errors, env, aliasDef, aliasAccum:
    pars & typeByName =
        namedParsToIdParsAndDict aliasDef.pars

    originalIdToNewId as Dict UnivarId UnivarId =
        # TODO ----> We should probably do something with these
        Dict.empty

    type as TA.RawType =
        translateRawType { env with expandedAliases = aliasAccum } typeByName originalIdToNewId @errors aliasDef.type

    Dict.insert aliasAccum aliasDef.usr { pars, type }


namedParsToIdParsAndDict as fn [ Name & Pos ]: [ TA.TyvarId ] & Dict Name TA.RawType =
    fn atPars:
    idPars =
        atPars >> List.mapWithIndex __ (fn index, atName: -index)

    typeByName =
        atPars
        >> List.mapWithIndex __ (fn index, name & pos: name & TA.'typeVar pos -index)
        >> Dict.fromList

    idPars & typeByName


getAliasDependencies as fn ByUsr aliasDef, CA.AliasDef: CA.Deps =
    fn allAliases, aliasDef:
    aliasDef.directDeps >> Dict.filter (fn usr, _: Dict.has usr allAliases) __


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
                List.each2WithIndex args2 args1 fn index, raw1, raw2:
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

                List.each2WithIndex pars1 pars2 (compareParTypes env head __ __ __ @state)

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
        TA.'typeFn _ ins out: List.any ins (fn t: t >> TA.toRaw >> rec) or rec out.raw
        TA.'typeVar _ id: id == tyvarId
        TA.'typeExact _ usr args: List.any args rec
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
