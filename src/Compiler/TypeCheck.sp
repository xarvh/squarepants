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


# TODO move this to Dict
onlyBothOnly as Dict key a: Dict key b: Dict key a & Dict key (a & b) & Dict key b =
    da: db:

    onAOnly =
        key: a: (aOnly & both & bOnly):
        Dict.insert key a aOnly & both & bOnly

    onBOnly =
        key: b: (aOnly & both & bOnly):
        aOnly & both & Dict.insert key b bOnly

    onBoth =
        key: a: b: (aOnly & both & bOnly):
        aOnly & Dict.insert key (a & b) both & bOnly

    Dict.merge onAOnly onBoth onBOnly da db (Dict.empty & Dict.empty & Dict.empty)



list_eachWithIndex2 as Int: [a]: [b]: (Int: a: b: None): None =
    index: aa: bb: f:

    try aa & bb as
        (a :: at) & (b :: bt):
            f index a b
            list_eachWithIndex2 (index + 1) at bt f
        _:
            None



alias State = {
    , equalities as Array Equality
    , errors as Array (Pos & Context & Error_)
    , lastUnificationVarId as Int
    , tyvarsById as Hash TA.UnificationVariableId TA.Tyvar
    }


initState as Int: State =
    lastUnificationVarId:
    {
    , equalities = Array.fromList []
    , errors = Array.fromList []
    , lastUnificationVarId
    , tyvarsById = Hash.empty
    }


alias Instance = {
    , definedAt as Pos
    , type as TA.Type
    , freeTyvars as Dict TA.UnificationVariableId TA.Tyvar
    }


alias ExpandedAlias =
    {
    , pars as [TA.UnificationVariableId]
    , type as TA.Type
    }



alias Env = {
    , context as Context
    , constructors as ByUsr Instance
    , variables as Dict TA.Ref Instance

    , expandedAliases as ByUsr ExpandedAlias
    , exactTypes as ByUsr [At Name]

    , annotatedTyvarsByName as Dict Name TA.UnificationVariableId
    }


initEnv as Env =
    {
    , context = Context_Global
    , constructors = Dict.empty
    , variables = Dict.empty
    , expandedAliases = Dict.empty
    , exactTypes = Dict.empty
    , annotatedTyvarsByName = Dict.empty
    }


union Error_ =
    , ErrorCircularValue [CA.Pattern]
    , ErrorVariableNotFound CA.Ref
    , ErrorConstructorNotFound USR
    , ErrorNotCompatibleWithRecord
    , ErrorRecordDoesNotHaveAttribute Name [# TODO other attrs to give context? #]
    , ErrorRecordHasAttributesNotInAnnotation # TODO which attrs?
    , ErrorRecordIsMissingAttibutesInAnnotation # TODO which attrs?
    , ErrorTryingToAccessAttributeOfNonRecord Name TA.Type
    , ErrorIncompatibleTypes CA.Expression TA.Type
    , ErrorIncompatiblePattern CA.Pattern TA.Type
    , ErrorCallingANonFunction
    , ErrorTooManyArguments
    , ErrorNotEnoughArguments
    , ErrorIncompatibleRecycling
    , ErrorUniquenessDoesNotMatch
    , ErrorRecyclingDoesNotMatch
    , ErrorUndefinedTypeVariable Name
    , ErrorWrongNumberOfTypeArguments USR [TA.UnificationVariableId] [TA.Type]
    , ErrorNamedTypeNotFound USR
    , ErrorCircularAlias [USR]
    , ErrorTypeAllowsFunctions TA.Type


union Context =
    , Context_Global # this is never actually used =|
    , Context_Module UMR
    , Context_Argument Name Context
    , Context_LetInBody
    , Context_FnPar Int Context
    , Context_FnBody Pos Context
    , Context_TryBranch
    , Context_IfCondition
    , Context_IfFalse
    , Context_IfTrue
    , Context_AttributeName Name Context


union Why =
    , Why_Annotation
    , Why_LetIn
    , Why_Record
    , Why_RecordExt
    , Why_RecordAccess
    , Why_IfBranches
    , Why_TryPattern
    , Why_TryExpression
    , Why_AttributeAccess
    , Why_ReturnType
    , Why_Argument Int
    , Why_CalledAsFunction
    , Why_Todo
    , Why_Attribute Why
    , Why_FunctionInput Int Why
    , Why_FunctionOutput Why
    , Why_TypeArgument USR Int Why


# TODO turn this into a record
union Equality =
    , Equality Context Pos Why TA.Type TA.Type



#
# Core types
#
coreTypeBool as TA.Type =
    TA.TypeExact CoreTypes.p CoreTypes.boolDef.usr []

coreTypeNumber as TA.Type =
    TA.TypeExact CoreTypes.p CoreTypes.numberDef.usr []

coreTypeText as TA.Type =
    TA.TypeExact CoreTypes.p CoreTypes.textDef.usr []


#
#
#
newTyvarId as State@: TA.UnificationVariableId =
    state@:
    @state.lastUnificationVarId += 1
    state.lastUnificationVarId


newType as State@: TA.Type =
    state@:
    TA.TypeUnificationVariable (newTyvarId @state)


addEquality as Env: Pos: Why: TA.Type: TA.Type: State@: None =
    # TODO: should t1 be "expected" and t2 "actual" or something like that?
    env: pos: why: t1: t2: state@:

    Array.push @state.equalities << Equality env.context pos why t1 t2


addError as Env: Pos: Error_: State@: None =
    env: pos: error: state@:

    Array.push @state.errors (pos & env.context & error)


getConstructorByUsr as USR: Env: Maybe Instance =
    usr: env:

    Dict.get usr env.constructors


getVariableByRef as CA.Ref: Env: Maybe Instance =
    ref: env:

    Dict.get ref env.variables


#
#
# Generalize
#
#
generalize as Env: Instance: State@: TA.Type =
    env: instance: state@:

    instance.type
    >> Dict.for instance.freeTyvars originalTyvarId: tyvar:

        generalizedTyvarId =
            newTyvarId @state

        # We need to remember that this new tyvar has typeclass contraints
        Hash.insert @state.tyvarsById generalizedTyvarId tyvar

        applySubstitutionToType originalTyvarId (TA.TypeUnificationVariable generalizedTyvarId)


#
#
# Uniqueness
#
#
variableOfThisTypeMustBeFlaggedUnique as TA.Type: Bool =
    ca:

    try ca as
       TA.TypeUnique p ty:
         True

       TA.TypeExact p usr args:
          List.any variableOfThisTypeMustBeFlaggedUnique args

       TA.TypeFn p args out:
          False

       TA.TypeRecord p attrs:
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TA.TypeRecordExt tyvarId attrs:
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TA.TypeUnificationVariable _:
          # At worst a tyvar can be ImmutableOrUnique, in which case it still doesn't need to be flagged
          False


#
#
# CA to TA translation
#
#
expandTyvarsInType as State@: Context: Dict TA.UnificationVariableId TA.Type: TA.Type: TA.Type =
    state@: context: tyvarIdsToType: type:

    rec =
      expandTyvarsInType @state context tyvarIdsToType

    try type as
        TA.TypeExact pos usr args:
            TA.TypeExact pos usr (List.map rec args)

        TA.TypeFn pos ins out:
            TA.TypeFn pos (List.map (Tuple.mapSecond rec) ins) (rec out)

        TA.TypeRecord pos attrs:
            TA.TypeRecord pos (Dict.map (k: rec) attrs)

        TA.TypeUnique pos type:
            TA.TypeUnique pos (rec type)

        TA.TypeUnificationVariable id:
            try Dict.get id tyvarIdsToType as
                Nothing: todo << "Compiler bug: this is not supposed to happen"
                Just ty: ty

        TA.TypeRecordExt id attrs:
            TA.TypeRecordExt id (Dict.map (k: rec) attrs)

        TA.TypeError pos:
            type


expandParamsAndAliases as State@: Context: ByUsr ExpandedAlias: Dict Name TA.Type: CA.Type: TA.Type =
    state@: context: allAliases: argsByName: caType:

    rec =
        expandParamsAndAliases @state context allAliases argsByName

    try caType as
        CA.TypeNamed pos usr pars:

            expandedPars =
                List.map rec pars

            try Dict.get usr allAliases as
                Nothing:
                    TA.TypeExact pos usr expandedPars

                Just expandedAlias:
                    if List.length expandedAlias.pars /= List.length expandedPars then
                        Array.push @state.errors (pos & context & ErrorWrongNumberOfTypeArguments usr expandedAlias.pars expandedPars)
                        TA.TypeError pos

                    else
                        tyvarIdsToType as Dict TA.UnificationVariableId TA.Type =
                            List.map2 Tuple.pair expandedAlias.pars expandedPars
                            >> Dict.fromList

                        expandTyvarsInType @state context tyvarIdsToType expandedAlias.type

        CA.TypeFn p modsAndArgs out:
            args = List.map (Tuple.mapSecond rec) modsAndArgs
            TA.TypeFn p args (rec out)

        CA.TypeRecord p attrs:
            TA.TypeRecord p (Dict.map (k: rec) attrs)

        CA.TypeUnique p ty:
            TA.TypeUnique p (rec ty)

        CA.TypeAnnotationVariable pos name:
            try Dict.get name argsByName as
                Nothing:
                    Array.push @state.errors (pos & context & ErrorUndefinedTypeVariable name)
                    TA.TypeError pos

                Just ty:
                    ty


annotationToTaType as State@: Env: CA.Type: TA.Type =
    state@: env: ca:

    nameToType =
        Dict.map (k: TA.TypeUnificationVariable) env.annotatedTyvarsByName

    expandParamsAndAliases @state env.context env.expandedAliases nameToType ca


#
#
# Definitions
#
#
doDefinition as Env: CA.ValueDef: State@: TA.ValueDef & Env =
    env: def: state@:

    (freeTyvars as Dict TA.UnificationVariableId TA.Tyvar) & (annotatedTyvarsByName as Dict Name TA.UnificationVariableId) =
        Dict.empty & env.annotatedTyvarsByName
        >> Dict.for def.tyvars tyvarName: typeClasses: (ftById & atByName):
            try Dict.get tyvarName env.annotatedTyvarsByName as

                Just _:
                    # TODO ensure that this definition is not adding flags to tyvars defined in the parent
                    ftById & atByName

                Nothing:
                    tyvarId =
                        newTyvarId @state

                    tyvar as TA.Tyvar =
                        { allowFunctions = typeClasses.allowFunctions
                        , allowUniques = typeClasses.allowUniques
                        , originalName = tyvarName
                        }

                    Dict.insert tyvarId tyvar ftById & Dict.insert tyvarName tyvarId atByName

    patternOut =
        inferPattern { env with annotatedTyvarsByName } False def.pattern @state

    envWithContext =
        { patternOut.env with
        , context = Context_LetInBody
        }

    typedBody & bodyType =
        if def.native then
            TA.LiteralText Pos.N "native" & patternOut.patternType
        else
            try patternOut.maybeFullAnnotation as
                Just annotationType:
                    taType = annotationToTaType @state envWithContext annotationType
                    checkExpression envWithContext taType def.body @state & taType
                Nothing:
                    inferExpression envWithContext def.body @state


    if patternOut.maybeFullAnnotation == Nothing then
        addEquality envWithContext (CA.patternPos def.pattern) Why_LetIn patternOut.patternType bodyType @state
    else
        None

    updateInstance as Instance: Instance =
        instance: { instance with freeTyvars }

    finalEnv as Env =
        if freeTyvars == Dict.empty then
            patternOut.env
        else
            { patternOut.env with
            , variables = .variables
                >> Dict.for (TA.patternNames patternOut.typedPattern) name: _: vars:
                    Dict.update (CA.RefLocal name) (Maybe.map updateInstance) vars
            }

    {
    , pattern = patternOut.typedPattern
    , native = def.native
    , body = typedBody
    , directValueDeps = def.directValueDeps
    , freeTyvars
    , isFullyAnnotated = patternOut.maybeFullAnnotation /= Nothing
    }
    &
    finalEnv



#
#
# Expressions
#
#
inferExpression as Env: CA.Expression: State@: TA.Expression & TA.Type =
    env: caExpression: state@:

    try caExpression as

        # TODO would be nice to be able to say that from here on, `caExpression as Expression any`
        CA.LiteralNumber pos n:
            TA.LiteralNumber pos n & coreTypeNumber


        CA.LiteralText pos text:
            TA.LiteralText pos text & coreTypeText


        CA.Variable pos ref:
            ty =
                try getVariableByRef ref env as
                    Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        newType @state

                    Just var:
                        generalize env var @state

            TA.Variable pos ref & ty


        CA.Constructor pos usr:
            ty =
                try getConstructorByUsr usr env as
                    Nothing:
                        addError env pos (ErrorConstructorNotFound usr) @state
                        newType @state

                    Just cons:
                        generalize env cons @state

            TA.Constructor pos usr & ty


        CA.Fn pos caPars body:
            inferFn env pos caPars body @state


        CA.Call pos reference args:

            callType =
                newType @state

            checkCallCo env callType pos reference args @state & callType


        CA.Record pos Nothing attrs:

            typedValueAndValueTypeByName =
                attrs >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName as Dict Name TA.Expression =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name TA.Type =
                Dict.map (k: Tuple.second) typedValueAndValueTypeByName

            TA.Record pos Nothing typedValueByName & TA.TypeRecord pos valueTypeByName


        CA.Record pos (Just ext) attrExpressions:


            typedValueAndValueTypeByName as Dict Name (TA.Expression & TA.Type) =
                attrExpressions >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName as Dict Name TA.Expression =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name TA.Type =
                Dict.map (k: Tuple.second) typedValueAndValueTypeByName

            typedExt & extType =
                inferExpression env ext @state

            TA.Record pos (Just typedExt) typedValueByName & inferRecordExtended env pos extType valueTypeByName @state


        CA.RecordAccess pos attrName recordExpression:

            typedExpr & inferredType =
                inferExpression env recordExpression @state

            TA.RecordAccess pos attrName typedExpr & inferRecordAccess env pos attrName inferredType @state


        CA.LetIn def rest:

            typedDef & defEnv =
                doDefinition env def @state

            typedRest & restType =
                inferExpression defEnv rest @state

            TA.LetIn typedDef typedRest & restType


        CA.If pos { condition, true, false }:

            typedCondition =
                checkExpression { env with context = Context_IfCondition } coreTypeBool condition @state

            typedTrue & trueType =
                inferExpression { env with context = Context_IfTrue } true @state

            typedFalse & falseType =
                inferExpression { env with context = Context_IfFalse } false @state

            addEquality env pos Why_IfBranches trueType falseType @state

            expression =
                TA.If pos {
                    , condition = typedCondition
                    , true = typedTrue
                    , false = typedFalse
                    }

            expression & trueType


        CA.Try pos { value, patternsAndExpressions }:

            typedValue & valueType =
                inferExpression env value @state

            finalType =
                newType @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    patternOut as PatternOut =
                        inferPattern env False pa @state

                    addEquality env pos Why_TryPattern patternOut.patternType valueType @state

                    newEnv =
                        { patternOut.env with
                        , context = Context_TryBranch
                        }

                    typedExpression & expressionType =
                        inferExpression newEnv exp @state

                    addEquality newEnv (CA.patternPos pa) Why_TryExpression finalType expressionType @state

                    patternOut.typedPattern & typedExpression

            TA.Try pos { value = typedValue, type = valueType, patternsAndExpressions = typedPatternsAndExpressions } & finalType


        CA.DestroyIn name expression:
            typedExpression & expressionType =
                inferExpression env expression @state

            TA.DestroyIn name typedExpression & expressionType


inferParam as Env: CA.Parameter: State@: TA.Parameter & TA.Type & Env =
    env: par: state@:

    try par as
        CA.ParameterPattern pa:
            patternOut =
                inferPattern env True pa @state

            TA.ParameterPattern patternOut.typedPattern & patternOut.patternType & patternOut.env

        CA.ParameterRecycle pos name:

            # TODO check name already in env?

            type = TA.TypeUnique pos (newType @state)

            typeWithClasses as Instance =
                {
                , definedAt = pos
                , type
                , freeTyvars = Dict.empty
                }

            newEnv as Env =
                { env with variables = Dict.insert (CA.RefLocal name) typeWithClasses .variables }

            TA.ParameterRecycle pos name & type & newEnv




inferFn as Env: Pos: [CA.Parameter]: CA.Expression: State@: TA.Expression & TA.Type =
    env: pos: caPars: body: state@:

    typedPars @= Array.fromList []
    parTypes @= Array.fromList []
    # TODO use typedPars length instead?
    parIndex @= 0

    newEnv as Env =
        env >> List.for caPars par: envX:
            typedPar & parType & envX1 =
                inferParam { envX with context = Context_FnPar parIndex .context } par @state

            isRecycling =
                try par as
                    CA.ParameterPattern _: False
                    CA.ParameterRecycle _ _: True

            Array.push @typedPars (typedPar & parType)
            Array.push @parTypes (isRecycling & parType)
            @parIndex += 1
            envX1

    typedBody & bodyType =
        inferExpression { newEnv with context = Context_FnBody pos env.context } body @state

    type as TA.Type =
        TA.TypeFn pos (Array.toList parTypes) bodyType

    exp =
        TA.Fn pos (Array.toList typedPars) typedBody

    exp & type





inferRecordAccess as Env: Pos: Name: TA.Type: State@: TA.Type =
    env: pos: attrName: inferredType: state@:

    try inferredType as
        TA.TypeRecord _ attrTypes:
            try Dict.get attrName attrTypes as
                Just type:
                    type

                Nothing:
                    addError env pos (ErrorRecordDoesNotHaveAttribute attrName) @state
                    newType @state

        TA.TypeRecordExt tyvarId extensionAttrTypes:
            try Dict.get attrName extensionAttrTypes as
                Just type:
                    type

                Nothing:
                    newExtId = newTyvarId @state
                    newAttrType = newType @state

                    type = TA.TypeRecordExt newExtId (Dict.insert attrName newAttrType extensionAttrTypes)

                    addEquality env pos Why_RecordAccess (TA.TypeUnificationVariable tyvarId) type @state

                    newAttrType


        TA.TypeUnificationVariable id:
            newExtId = newTyvarId @state
            newAttrType = newType @state

            type as TA.Type =
                TA.TypeRecordExt newExtId (Dict.singleton attrName newAttrType)

            addEquality env pos Why_RecordAccess (TA.TypeUnificationVariable id) type @state

            newAttrType

        _:
            addError env pos (ErrorTryingToAccessAttributeOfNonRecord attrName inferredType) @state
            newType @state



inferRecordExtended as Env: Pos: TA.Type: Dict Name TA.Type: State@: TA.Type =
    env: pos: extType: valueTypeByName: state@:

    try extType as
        TA.TypeRecord _ attrTypes:

            Dict.each valueTypeByName name: valueType:
                try Dict.get name attrTypes as

                    Nothing:
                        addError env pos (ErrorRecordDoesNotHaveAttribute name) @state

                    Just ty:
                        addEquality env pos Why_Record ty valueType @state

            extType


        TA.TypeRecordExt tyvarId extensionAttrTypes:

            expressionOnly & both & extensionOnly =
                onlyBothOnly valueTypeByName extensionAttrTypes

            Dict.each both name: (inAttr & extAttr):
                addEquality env pos Why_Record inAttr extAttr @state

            # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
            newExtId =
                newTyvarId @state

            TA.TypeRecordExt newExtId (Dict.join valueTypeByName extensionOnly)


        TA.TypeUnificationVariable id:
            ty = TA.TypeRecordExt (newTyvarId @state) valueTypeByName

            addEquality env pos Why_RecordExt extType ty @state

            ty


        _:
            addError env pos ErrorNotCompatibleWithRecord @state
            newType @state


#
# Check
#


checkParameter as Env: Bool: TA.Type: CA.Parameter: State@: TA.Parameter & Env =
    env: isRecycling: expectedType: par: state@:

    try par as
        CA.ParameterPattern pa:

            if isRecycling then
                addError env (CA.patternPos pa) ErrorRecyclingDoesNotMatch @state
            else
                None

            typedPa & env1 =
                checkPattern env expectedType True pa @state

            TA.ParameterPattern typedPa & env1

        CA.ParameterRecycle pos name:

            if not isRecycling then
                addError env pos ErrorRecyclingDoesNotMatch @state
            else
                None

            # TODO: expectedType must be unique!!!

            variable as Instance =
                {
                , definedAt = pos
                , type = expectedType
                , freeTyvars = Dict.empty
                }

            localEnv as Env =
                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with variables = Dict.insert (CA.RefLocal name) variable .variables }

            TA.ParameterRecycle pos name & localEnv




checkExpression as Env: TA.Type: CA.Expression: State@: TA.Expression =
    env: expectedType: caExpression: state@:

    try caExpression & expectedType as

        CA.LiteralNumber pos n & TA.TypeExact _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                addError env pos (ErrorIncompatibleTypes caExpression expectedType) @state
            else
                None

            TA.LiteralNumber pos n


        CA.LiteralText pos text & TA.TypeExact _ typeUsr []:
            if typeUsr /= CoreTypes.textDef.usr then
                addError env pos (ErrorIncompatibleTypes caExpression expectedType) @state
            else
                None

            TA.LiteralText pos text


        CA.Variable pos ref & _:
            try getVariableByRef ref env as
                Nothing:
                    addError env pos (ErrorVariableNotFound ref) @state

                Just var:
                    addEquality env pos Why_Annotation var.type expectedType @state

            TA.Variable pos ref


        CA.Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    addError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    addEquality env pos Why_Annotation cons.type expectedType @state

            TA.Constructor pos usr


        CA.Fn pos pars body & TA.TypeFn _ parsT out:

            if List.length pars > List.length parsT then
                addError env pos ErrorTooManyArguments @state
                TA.Error pos
            else if List.length pars < List.length parsT then
                addError env pos ErrorNotEnoughArguments @state
                TA.Error pos
            else
                typedPars @= Array.fromList []
                parIndex @= 0

                localEnv as Env =
                    env >> List.for (List.map2 Tuple.pair pars parsT) (par & (isRecyclingT & parT)): envX:
                        typedPar & envX1 =
                            checkParameter { envX with context = Context_FnPar parIndex .context } isRecyclingT parT par @state

                        Array.push @typedPars (typedPar & parT)
                        @parIndex += 1

                        envX1

                typedBody =
                    checkExpression localEnv out body @state

                TA.Fn pos (Array.toList typedPars) typedBody


        CA.Call pos reference args & _:
            checkCallCo env expectedType pos reference args @state


        CA.Record pos (Just ext) valueByName & TA.TypeRecord _ typeByName:

            # ext must have type expectedType
            # TODO: add context
            typedExt =
                checkExpression env expectedType ext @state

            # all valueByName attrs must be in typeByName
            typedValueByName =
                valueByName >> Dict.map attrName: attrExpr:
                    try Dict.get attrName typeByName as
                        Nothing:
                            addError env pos ErrorRecordHasAttributesNotInAnnotation @state
                            # TODO: is there a smarter way?
                            Tuple.first (inferExpression env attrExpr @state)
                        Just attrType:
                            # TODO: add context
                            checkExpression env attrType attrExpr @state

            TA.Record pos (Just typedExt) typedValueByName


        CA.Record pos Nothing valueByName & TA.TypeRecord _ typeByName:
            aOnly & both & bOnly =
                onlyBothOnly valueByName typeByName

            if aOnly /= Dict.empty then
                addError env pos ErrorRecordHasAttributesNotInAnnotation @state

            else if bOnly /= Dict.empty then
                addError env pos ErrorRecordIsMissingAttibutesInAnnotation @state

            else
                None

            typedAttrs =
                both >> Dict.map name: (value & type):
                    # TODO add attribute name to env!?
                    checkExpression env type value @state

            TA.Record pos Nothing typedAttrs


        CA.RecordAccess pos attrName exp & _:

            typedExpression & expressionType =
                inferExpression env exp @state

            newId =
                newTyvarId @state

            requiredType =
                expectedType
                >> Dict.singleton attrName
                >> TA.TypeRecordExt newId

            addEquality env pos Why_AttributeAccess expressionType requiredType @state

            TA.RecordAccess pos attrName typedExpression


        CA.LetIn def rest & _:

            typedDef & defEnv =
                doDefinition env def @state

            typedRest =
                checkExpression defEnv expectedType rest @state

            TA.LetIn typedDef typedRest


        CA.If pos { condition, true, false } & _:

            typedCondition =
                checkExpression { env with context = Context_IfCondition } coreTypeBool condition @state

            typedTrue =
                checkExpression { env with context = Context_IfTrue } expectedType true @state

            typedFalse =
                checkExpression { env with context = Context_IfFalse } expectedType false @state

            TA.If pos {
                , condition = typedCondition
                , true = typedTrue
                , false = typedFalse
                }


        CA.Try pos { value, patternsAndExpressions } & _:

            typedValue & valueType =
                inferExpression env value @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    inferredPattern =
                        inferPattern env False pa @state

                    newEnv =
                        { inferredPattern.env with
                        , context = Context_TryBranch
                        }

                    addEquality env pos Why_TryPattern inferredPattern.patternType valueType @state

                    typedExpression =
                        checkExpression newEnv expectedType exp @state

                    inferredPattern.typedPattern & typedExpression

            TA.Try pos {
                , value = typedValue
                , type = valueType
                , patternsAndExpressions = typedPatternsAndExpressions
                }


        CA.DestroyIn name exp & _:
            TA.DestroyIn name << checkExpression env expectedType exp @state


        _ & TA.TypeError pos:
            TA.Error pos


        _:
            pos = CA.expressionPos caExpression
            addError env pos (ErrorIncompatibleTypes caExpression expectedType) @state
            TA.Error pos




checkCallCo as Env: TA.Type: Pos: CA.Expression: [CA.Argument]: State@: TA.Expression =
    env: expectedType: pos: reference: givenArgs: state@:

    # `reference givenArg1 givenArg2 ...` must be of `expectedType`

    typedReference & inferredReferenceType =
        inferExpression env reference @state

    typedArgumentsAndArgumentTypes as [TA.Argument & TA.Type] =
        givenArgs >> List.map arg:
            inferArgument env arg @state

    toTypeArg as (TA.Argument & TA.Type): Bool & TA.Type =
        (arg & type):
        try arg as
            TA.ArgumentExpression _: False & type
            TA.ArgumentRecycle _ _ _: True & type

    expectedReferenceType as TA.Type =
        TA.TypeFn pos (List.map toTypeArg typedArgumentsAndArgumentTypes) expectedType

    addEquality env pos Why_CalledAsFunction inferredReferenceType expectedReferenceType @state

    TA.Call pos typedReference typedArgumentsAndArgumentTypes



inferArgument as Env: CA.Argument: State@: TA.Argument & TA.Type =
    env: arg: state@:

    try arg as
        CA.ArgumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            TA.ArgumentExpression typedExp & expType

        CA.ArgumentRecycle pos name attrPath:
            ref =
                CA.RefLocal name

            type =
                try getVariableByRef ref env as

                    Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        newType @state

                    Just var:
                        todo "apply attrPath"
                        var.type

            TA.ArgumentRecycle pos attrPath name & type


#
#
# Patterns
#
#
alias PatternOut = {
    , patternType as TA.Type
    , typedPattern as TA.Pattern
    , maybeFullAnnotation as Maybe CA.Type
    , env as Env
    }


inferPattern as Env: Bool: CA.Pattern: State@: PatternOut =
    # TODO remove isParam
    env: isParam: pattern: state@:

    try pattern as
        CA.PatternAny pos args:
            inferPatternAny env isParam pos args @state


        CA.PatternLiteralText pos text:
            {
            , typedPattern = TA.PatternLiteralText pos text
            , patternType = coreTypeText
            , env
            , maybeFullAnnotation = Just CoreTypes.text
            }


        CA.PatternLiteralNumber pos n:
            {
            , typedPattern = TA.PatternLiteralNumber pos n
            , patternType = coreTypeNumber
            , env
            , maybeFullAnnotation = Just CoreTypes.number
            }


        CA.PatternConstructor pos usr arguments:

            typedArguments & argumentTypes & newEnv =
                [] & [] & env >> List.forReversed arguments arg: (typedPas & paTypes & envX):

                    out =
                        inferPattern envX isParam arg @state

                    (out.typedPattern :: typedPas) & (out.patternType :: paTypes) & out.env

            finalType =
                try getConstructorByUsr usr env as

                    Nothing:
                        addError env pos (ErrorConstructorNotFound usr) @state
                        newType @state

                    Just cons:
                        argModAndTypes & returnType =
                            try generalize env cons @state as
                                TA.TypeFn _ ins out: ins & out
                                TA.TypeExact _ usr args: [] & cons.type
                                _: todo << "compiler bug: cons.type is not a cons but a " .. toHuman cons.type

                        rl =
                            List.length argModAndTypes

                        gl =
                            List.length arguments

                        if rl > gl then
                            addError env pos ErrorNotEnoughArguments @state
                        else
                            None

                        if rl < gl then
                            addError env pos ErrorTooManyArguments @state
                        else
                            None

                        list_eachWithIndex2 0 argModAndTypes argumentTypes index: (mod & paramType): argType:
                            addEquality env pos (Why_Argument index) paramType argType @state

                        ##    { x } = blah
                        ##
                        ## `blah` /could/ be unique, but in this case we'll just assume it is NOT

                        if List.any variableOfThisTypeMustBeFlaggedUnique argumentTypes then
                            TA.TypeUnique pos returnType
                        else
                            returnType

            {
            , typedPattern = TA.PatternConstructor pos usr typedArguments
            , patternType = finalType
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }


        CA.PatternRecord pos completeness pas:

            typedPatternsAndPatternTypesByName & newEnv =
                Dict.empty & env >> Dict.for pas name: pa: (dict & envX):

                    out =
                        inferPattern envX isParam pa @state

                    Dict.insert name (out.typedPattern & out.patternType) dict & out.env

            patternTypeByName =
                typedPatternsAndPatternTypesByName >> Dict.map name: Tuple.second

            patternType as TA.Type =
                try completeness as
                    CA.Complete: TA.TypeRecord pos patternTypeByName
                    CA.Partial: TA.TypeRecordExt (newTyvarId @state) patternTypeByName

            {
            , typedPattern = TA.PatternRecord pos typedPatternsAndPatternTypesByName
            , patternType
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }



inferPatternAny as Env: Bool: Pos: { isUnique as Bool, maybeName as Maybe Name, maybeAnnotation as Maybe CA.Type }: State@: PatternOut =
    env: isParam: pos: ({ isUnique, maybeName, maybeAnnotation }): state@:

    patternType as TA.Type =
        try maybeAnnotation as
            Nothing:
                if isUnique then
                    TA.TypeUnique pos (newType @state)
                else
                    newType @state

            Just annotation:
                t =
                    annotationToTaType @state env annotation

                if variableOfThisTypeMustBeFlaggedUnique t /= isUnique then
                    addError env pos ErrorUniquenessDoesNotMatch @state
                else
                    None

                t

    envWithVariable as Env =
        try maybeName as
            Nothing:
                env

            Just name:
                variable as Instance =
                    {
                    , definedAt = pos
                    , type = patternType
                    , freeTyvars = Dict.empty
                    }

                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with
                , variables = Dict.insert (CA.RefLocal name) variable .variables
                }

    typedPattern =
        TA.PatternAny pos { isUnique, maybeName, maybeAnnotation, type = patternType }

    {
    , typedPattern
    , patternType
    , env = envWithVariable
    , maybeFullAnnotation = maybeAnnotation
    }








checkPattern as Env: TA.Type: Bool: CA.Pattern: State@: TA.Pattern & Env =
    env: expectedType: isParam: pattern: state@:

    try pattern & expectedType as
        CA.PatternAny pos { isUnique, maybeName, maybeAnnotation } & _:
            try maybeAnnotation as
                Nothing: None
                Just annotation: todo "Just annotation"

            newEnv as Env =
                try maybeName as
                    Nothing:
                        env

                    Just name:
                        variable as Instance =
                            {
                            , definedAt = pos
                            , type = expectedType
                            , freeTyvars = Dict.empty
                            }

                        # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                        { env with
                        , variables = Dict.insert (CA.RefLocal name) variable .variables
                        }

            TA.PatternAny pos { isUnique, maybeName, maybeAnnotation, type = expectedType } & newEnv


        CA.PatternLiteralText pos text & TA.TypeExact _ typeUsr []:
            if typeUsr /= CoreTypes.textDef.usr then
                addError env pos (ErrorIncompatiblePattern pattern expectedType) @state
            else
                None

            TA.PatternLiteralText pos text & env


        CA.PatternLiteralNumber pos text & TA.TypeExact _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                addError env pos (ErrorIncompatiblePattern pattern expectedType) @state
            else
                None

            TA.PatternLiteralNumber pos text & env


        CA.PatternConstructor pos usr arguments & _:
            todo """
            {
            , typedPattern = TA.PatternConstructor pos usr typedArguments
            , patternType = finalType
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }
            """


        CA.PatternRecord pos completeness pas & _:
            todo """
            {
            , typedPattern = TA.PatternRecord pos typedPatternsAndPatternTypesByName
            , patternType
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }
            """



#
# Def Resolution
#
# After we have resolved all equalities, we want to go through each def and replace all tyvars with the resolved type
#
resolveTypesInValueDef as Dict TA.UnificationVariableId TA.Type: TA.ValueDef: TA.ValueDef =
    substitutions: def:

    { def with pattern = subsOnPattern substitutions .pattern }

#    freeTyvars =
#        if def.isFullyAnnotated then
#            def.tyvars
#        else
#            Dict.empty
#            >> Dict.for def.freeTyvars tyvarId: typeClasses: acc:
#                try Dict.get tyvarId substitutions as
#                    Nothing:
#                        acc
#
#                    Just type:
#                        Dict.join (Dict.map (k: v: typeClasses ) << TA.typeTyvars type) acc
#
#
#    # TODO: actually resolve all types in the def body
#
#    { def with freeTyvars }



subsOnPattern as Dict TA.UnificationVariableId TA.Type: TA.Pattern: TA.Pattern =
    substitutions: pattern:

    try pattern as
        TA.PatternLiteralNumber pos _: pattern
        TA.PatternLiteralText pos _: pattern

        TA.PatternAny pos stuff:
            TA.PatternAny pos { stuff with type = applyAllSubstitutions substitutions .type }

        TA.PatternConstructor pos path ps:
            TA.PatternConstructor pos path (List.map (subsOnPattern substitutions) ps)

        TA.PatternRecord pos ps:
            TA.PatternRecord pos (Dict.map (k: (p & t): subsOnPattern substitutions p & applyAllSubstitutions substitutions t) ps)



#
#
# Module
#
#
insertAnnotatedAndNonAnnotated as CA.Pattern: CA.ValueDef: [CA.ValueDef] & [CA.ValueDef]: [CA.ValueDef] & [CA.ValueDef] =
    pa: def: (ann & nonAnn):

    isFullyAnnotated =
        pa
        >> CA.patternNames
        >> Dict.values
        >> List.all stuff: stuff.maybeAnnotation /= Nothing

    if isFullyAnnotated then
        (def :: ann) & nonAnn
    else
        ann & (def :: nonAnn)


doModule as Int: Env: CA.Module: Res TA.Module =
    state0: env: caModule:

    Debug.benchStart None

    # state is per module
    state as State @=
        initState state0

    annotated & nonAnnotated =
        Dict.for caModule.valueDefs insertAnnotatedAndNonAnnotated ([] & [])

    if List.length nonAnnotated > 1 then
      todo "Right now the compiler supports only one root-level non-annotated value per module. =("
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

    #
    # inference and check
    #
    (typedValueDefs as Dict CA.Pattern TA.ValueDef) & (envF as Env) =
        Dict.empty & env
        >> List.for allOrdered def: (accum & env0):
              typedDef & env1 =
                  doDefinition env0 def @state

              Dict.insert def.pattern typedDef accum & env1

    #
    # constraint resolution
    #
    erState0 =
        {
        , equalities = Array.toList state.equalities
        , errors = []
        , substitutions = Dict.empty
        }

#    List.each (Array.toList state.equalities) eq:
#        log "EQ" eq

    erStateF =
        solveEqualities erState0

    Hash.each state.tyvarsById tyvarId: typeClasses:
        if typeClasses.allowFunctions /= Just False then
            None
        else
            try Dict.get tyvarId erStateF.substitutions as
                Nothing:
                    None
                Just taType:
                    if TA.typeAllowsFunctions taType then
                        addError env Pos.G (ErrorTypeAllowsFunctions taType) @state
                    else
                        None

    Debug.benchStop "type check"

    Debug.benchStart None

    resolvedValueDefs =
        Dict.map (k: resolveTypesInValueDef erStateF.substitutions) typedValueDefs

    Debug.benchStop "type resolution"

    #
    # packaging & closing
    #
    typedModule as TA.Module =
        {
        , umr = caModule.umr
        , asText = caModule.asText
        , valueDefs = resolvedValueDefs
        , substitutions = erStateF.substitutions
        }

#    List.each (Dict.toList erStateF.substitutions) (id & t):
#        log (Text.fromNumber id) t

    errors as [Error] =
        [
        , state.errors
          >> Array.toList
          >> List.map (makeInferenceAndCheckError env caModule)
        , erStateF.errors
          >> List.map (makeResolutionError env caModule)
        ]
        >> List.concat


    if errors == [] then
        Ok typedModule

    else
        errors
        >> Error.Nested
        >> Err


applyAllSubstitutions as Dict TA.UnificationVariableId TA.Type: TA.Type: TA.Type =
    subs:
    Dict.for subs applySubstitutionToType


makeInferenceAndCheckError as Env: CA.Module: (Pos & Context & Error_): Error =
    env: caModule: (pos & context & error):

    Error.Simple pos eenv:
        [
        , Debug.toHuman error
        , Debug.toHuman context
        ]


makeResolutionError as Env: CA.Module: (Equality & Text): Error =
    env: caModule: (Equality context pos why t1 t2 & message):

    Error.Simple pos eenv:
        [
        , message
        , Debug.toHuman context
        , Debug.toHuman why
        , Debug.toHuman t1
        , Debug.toHuman t2
        ]


#
#
# Populate global Env
#
#
addValueToGlobalEnv as State@: UMR: CA.ValueDef: Env: Env =
    state@: umr: def: env:

    nameToIdAndClasses as Dict Name (TA.UnificationVariableId & TA.Tyvar) =
        def.tyvars
        >> Dict.map name: ({ allowFunctions, allowUniques }): newTyvarId @state & { originalName = name, allowFunctions, allowUniques }

    nameToType as Dict Name TA.Type =
        nameToIdAndClasses >> Dict.map k: (id & classes): TA.TypeUnificationVariable id

    unificationIdToClasses as Dict TA.UnificationVariableId TA.Tyvar =
        nameToIdAndClasses
        >> Dict.values
        >> Dict.fromList

    env >> Dict.for (CA.patternNames def.pattern) valueName: valueStuff: envX:
        try valueStuff.maybeAnnotation as
            Nothing:
                envX

            Just annotation:

                ref as TA.Ref =
                    valueName
                    >> USR umr
                    >> CA.RefGlobal

                type as TA.Type =
                    expandParamsAndAliases @state Context_Global env.expandedAliases nameToType annotation

                instance as Instance =
                    {
                    , definedAt = valueStuff.pos
                    , type
                    , freeTyvars = Dict.intersect unificationIdToClasses (TA.typeTyvars type)
                    }

                { envX with variables = Dict.insert ref instance .variables }


addConstructorToGlobalEnv as State@: Dict Name TA.Type: Dict TA.UnificationVariableId TA.Tyvar: Name: CA.Constructor: Env: Env =
    state@: paramsByName: freeTyvars: name: caConstructor: env:

    USR umr _ =
        caConstructor.typeUsr

    type as TA.Type =
        expandParamsAndAliases @state Context_Global env.expandedAliases paramsByName caConstructor.type

    consTyvars =
        TA.typeTyvars type

    taConstructor as Instance =
        {
        , definedAt = Pos.G
        , type
        , freeTyvars = freeTyvars >> Dict.filter k: v: Dict.member k consTyvars
        }

    { env with constructors = Dict.insert (USR umr name) taConstructor .constructors }


addUnionTypeAndConstructorsToGlobalEnv as State@: a: CA.UnionDef: Env: Env =
    state@: _: caUnionDef: env:

    paramsByName as Dict Name TA.Type =
        caUnionDef.pars
        >> List.indexedMap (index: ((At pos name): name & TA.TypeUnificationVariable -index))
        >> Dict.fromList

    freeTyvars as Dict TA.UnificationVariableId TA.Tyvar =
        caUnionDef.pars
        >> List.indexedMap (index: ((At pos name): -index & { originalName = name, allowFunctions = Just True, allowUniques = Just True }))
        >> Dict.fromList

    { env with exactTypes = Dict.insert caUnionDef.usr caUnionDef.pars .exactTypes }
    >> Dict.for caUnionDef.constructors (addConstructorToGlobalEnv @state paramsByName freeTyvars)








namedParsToIdParsAndDict as [At Name]: [TA.UnificationVariableId] & Dict Name TA.Type =
    atPars:

    idPars =
        atPars
        >> List.indexedMap (index: atName: -index)

    typeByName =
        atPars
        >> List.indexedMap (index: (At pos name): name & TA.TypeUnificationVariable -index)
        >> Dict.fromList

    idPars & typeByName


expandAndInsertAlias as State@: USR: CA.AliasDef: ByUsr ExpandedAlias: ByUsr ExpandedAlias =
    state@: usr: aliasDef: aliasAccum:

    pars & typeByName =
        namedParsToIdParsAndDict aliasDef.pars

    type as TA.Type =
        expandParamsAndAliases @state Context_Global aliasAccum typeByName aliasDef.type

    Dict.insert usr { pars, type } aliasAccum


getAliasDependencies as ByUsr aliasDef: CA.AliasDef: Set USR =
    allAliases: aliasDef:

    aliasDef.directTypeDeps
    >> Dict.filter (usr: _: Dict.member usr allAliases)

    # TODO: RefHierarchy should accept and Dict, not just a Set
    # Then we can remove this
    >> Dict.map k: v: None





initStateAndGlobalEnv as [CA.Module]: Int & Env =
    allModules:

    state @=
        initState 0

    # Before we expand the aliases, we need to reorder them
    allAliases as ByUsr CA.AliasDef =
        Dict.empty
        >> List.for allModules mod:
            Dict.for mod.aliasDefs name: aliasDef:
                Dict.insert aliasDef.usr aliasDef

    circulars & orderedAliases =
        RefHierarchy.reorder (getAliasDependencies allAliases) allAliases

    if circulars /= [] then
        # TODO: should I even use expandAndInsertAlias if there are circulars?
        log "=========> ERROR circulars aliases!!" circulars
        List.each circulars circular:
            Array.push @state.errors (Pos.G & Context_Global & ErrorCircularAlias circular)
    else
        None

    # Expand all aliases
    expandedAliases as ByUsr ExpandedAlias =
        Dict.empty >> Dict.for allAliases (expandAndInsertAlias @state)

    doStuff as CA.Module: Env: Env =
        caModule: env:
        env
        >> Dict.for caModule.unionDefs (addUnionTypeAndConstructorsToGlobalEnv @state)
        >> Dict.for caModule.valueDefs (pattern: addValueToGlobalEnv @state caModule.umr)

    env =
        { initEnv with
        , expandedAliases
        }
        >> List.for CoreTypes.allDefs (addUnionTypeAndConstructorsToGlobalEnv @state None)
        >> List.for allModules doStuff

    # TODO this shenanigan of pulling out `id` is needed because mutation is bugged
    id = state.lastUnificationVarId

    id & env






#
#
# Equalities resolution
#
#
alias ERState = {
    , equalities as [Equality]
    , errors as [Equality & Text]
    , substitutions as Dict TA.UnificationVariableId TA.Type
    }


addErError as Equality: Text: ERState: ERState =
    equality: message: state:

    { state with errors = (equality & message) :: .errors }


addErErrorIf as Bool: Equality: Text: ERState: ERState =
    test: equality: message: state:

    if test then
        { state with errors = (equality & message) :: .errors }
    else
        state


solveEqualities as ERState: ERState =
    oldState:

    try oldState.equalities as
        []:
            oldState

        head :: tail:

            Equality context pos why type1 type2 =
                head

            state =
                { oldState with equalities = tail }

            try type1 & type2 as

                TA.TypeUnificationVariable tyvarId & t2:
                    replaceUnificationVariable tyvarId t2 state


                t1 & TA.TypeUnificationVariable tyvarId:
                    replaceUnificationVariable tyvarId t1 state


                TA.TypeExact _ usr1 args1 & TA.TypeExact _ usr2 args2:
                    if usr1 /= usr2 then
                        state
                        >> addErError head "types are incompatible"
                        >> solveEqualities
                    else
                        newEqualities as [Equality] =
                            List.indexedMap2 (index: Equality context pos (Why_TypeArgument usr1 index why)) args1 args2

                        solveEqualities { state with equalities = List.append .equalities newEqualities }


                TA.TypeFn _ pars1 out1 & TA.TypeFn _ pars2 out2:
                    if List.length pars1 /= List.length pars2 then
                        state
                        >> addErError head "functions expect a different number of arguments"
                        >> solveEqualities

                    else
                        both =
                            List.map2 Tuple.pair pars1 pars2

                        inEqualities as [Equality] =
                            both >> List.indexedMap index: ((m1 & in1) & (m2 & in2)):
                                Equality context pos (Why_FunctionInput index why) in1 in2

                        outEquality as Equality =
                            Equality context pos (Why_FunctionOutput why) out1 out2

                        { state with equalities = List.concat [inEqualities, [outEquality], .equalities] }
                        >> List.for both (((m1 & _) & (m2 & _)): addErErrorIf (m1 /= m2) head "argument modifiers don't match")
                        >> solveEqualities


                TA.TypeRecord _ attrs1 & TA.TypeRecord _ attrs2:
                    only1 & both & only2 =
                        onlyBothOnly attrs1 attrs2

                    equalities as [Equality] =
                        state.equalities >> Dict.for both attrName: (attrType1 & attrType2): eqs:
                              Equality context pos (Why_Attribute why) attrType1 attrType2 :: eqs

                    { state with equalities }
                    >> addErErrorIf (only1 /= Dict.empty or only2 /= Dict.empty) head "record attrs don't match"
                    >> solveEqualities


                TA.TypeRecordExt tyvar1 attrs1 & TA.TypeRecord _ attrs2:
                    solveRecordExt head tyvar1 attrs1 attrs2 state


                TA.TypeRecord _ attrs1 & TA.TypeRecordExt tyvar2 attrs2:
                    solveRecordExt head tyvar2 attrs2 attrs1 state


                TA.TypeRecordExt tyvar1 attrs1 & TA.TypeRecordExt tyvar2 attr2:
                    todo "will this actually happen?"


                TA.TypeUnique _ m1 & TA.TypeUnique _ m2:
                    solveEqualities { state with equalities = Equality context pos why m1 m2 :: .equalities }


                _:
                    state
                    >> addErError head "types are incompatible"
                    >> solveEqualities


solveRecordExt as Equality: TA.UnificationVariableId: Dict Name TA.Type: Dict Name TA.Type: ERState: ERState =
    equality: tyvar1: attrs1: attrs2: state:

    #
    # { tyvar1 with attrs1 } == { attrs2 }
    #
    # =>    tyvar1 == { attrs2 }
    #
    # =>    all of attrs1 must be in attrs2 and match
    #

    Equality context pos why _ _ =
        equality

    newState =
        state >> Dict.for attrs1 name: type1: s:
            try Dict.get name attrs2 as
                Nothing:
                    state >> addErError equality ("missing attribute " .. name)
                Just type2:
                    { state with equalities = Equality (Context_AttributeName name context) pos why type1 type2 :: .equalities }

    replaceUnificationVariable tyvar1 (TA.TypeRecord pos attrs2) newState


replaceUnificationVariable as TA.UnificationVariableId: TA.Type: ERState: ERState =
    tyvarId: replacingType: state:

    #TODO: check that replacingType does not contain tyvarId

    equalities =
        # TODO we don't care about map preserving order
        state.equalities >> List.map (Equality context pos why t1 t2):
            Equality context pos why
                (applySubstitutionToType tyvarId replacingType t1)
                (applySubstitutionToType tyvarId replacingType t2)

    substitutions =
        state.substitutions
        >> Dict.map (tyvarId: type: applySubstitutionToType tyvarId replacingType type)
        >> Dict.insert tyvarId replacingType

    solveEqualities { state with substitutions, equalities }



applySubstitutionToType as TA.UnificationVariableId: TA.Type: TA.Type: TA.Type =
    tyvarId: replacingType: originalType:

    rec as TA.Type: TA.Type =
        applySubstitutionToType tyvarId replacingType

    try originalType as
        TA.TypeExact pos usr pars:
            TA.TypeExact pos usr (List.map rec pars)

        [#
        TypeAnnotationVariable pos name:
            originalType
        #]

        TA.TypeFn pos pars out:
            TA.TypeFn pos (CA.mapmod rec pars) (rec out)

        TA.TypeRecord pos attrs:
            TA.TypeRecord pos (Dict.map (k: rec) attrs)

        TA.TypeUnique pos t:
            TA.TypeUnique pos (rec t)

        TA.TypeUnificationVariable id:
            if id == tyvarId then
                replacingType
            else
                originalType

        TA.TypeRecordExt id attrs:
            if id == tyvarId then
                replacingType
            else
                TA.TypeRecordExt id (Dict.map (k: rec) attrs)

