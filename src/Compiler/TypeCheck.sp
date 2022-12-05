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
    , classesByTyvarId as Hash TA.UnificationVariableId CA.TypeClasses
    }


initState as Int: State =
    lastUnificationVarId:
    {
    , equalities = Array.fromList []
    , errors = Array.fromList []
    , lastUnificationVarId
    , classesByTyvarId = Hash.empty
    }


alias Instance = {
    , definedAt as Pos
    , type as TA.Type

    # TODO Should this be only for annotated tyvars?
    # TODO: or: should this be only for free tyvars?
    , tyvars as Dict TA.UnificationVariableId TA.TypeClasses
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

    [#
       Every time we use a value in an expression, we must re-instantiate its free variables, because each time they can be used in a different way.

       Because of this each Definition has a `freeTypeVariables` field.

       To figure out `freeTypeVariables` we can't simply take all tyvars we find in the definition's type, because:

       1) a tyvar that is free in a parent definition should not be considered free in children definitions:

         parent =
           as List a

           child =
             as a: a
             ...

           ...

       2) the placeholder tyvar of an argument should not be considered free while we infer the type of a function, otherwise we keep reinstantitiating it and we never figure out anything.

         f x =
           { first } = x
           first

       By keeping track of which tyvars should NOT be considered free, we can figure out the correct `freeTypeVariables` field for each child definition.
    #]
    , nonFreeTyvars as Dict TA.UnificationVariableId None

    # This is used to give meaningfule errors?
    , annotatedTyvarToGeneratedTyvar as Dict Name TA.UnificationVariableId
    }


initEnv as Env =
    {
    , context = Context_Global
    , constructors = Dict.empty
    , variables = Dict.empty
    , expandedAliases = Dict.empty
    , exactTypes = Dict.empty
    , nonFreeTyvars = Dict.empty
    , annotatedTyvarToGeneratedTyvar = Dict.empty
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
    , ErrorIncompatibleTypes CA.Expression CA.Type
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


union Equality =
    , Equality Context Pos Why TA.Type TA.Type






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
    env: typeWithClasses: state@:

    typeWithClasses.type >> Dict.for typeWithClasses.tyvars tyvarId: typeClasses:

        replacementType =
            try Dict.get tyvarId env.nonFreeTyvars as
                Just _:
                    # TODO also check that typeclasses are compatible? Should MakeCanonical do it?
                    TA.TypeUnificationVariable tyvarId

                Nothing:
                    tyvarId =
                        newTyvarId @state

                    # We need to remember that this new tyvar has typeclass contraints
                    Hash.insert @state.classesByTyvarId tyvarId typeClasses

                    TA.TypeUnificationVariable tyvarId

        applySubstitutionToType tyvarId replacementType


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


typeCa2Ta as State@: Env: CA.Type: TA.Type =
    state@: env: ca:

    nameToType =
        Dict.map (k: TA.TypeUnificationVariable) env.annotatedTyvarToGeneratedTyvar

    expandParamsAndAliases @state env.context env.expandedAliases nameToType ca


#
#
# Definitions
#
#
doDefinition as Env: CA.ValueDef: State@: TA.ValueDef & Env =
    env: def: state@:

    patternOut =
        inferPattern env False def.pattern @state

    envWithContext =
        { patternOut.env with
        , context = Context_LetInBody
        }

    # TODO do not check body if native!

    typedBody & bodyType =
        try patternOut.maybeFullAnnotation as
            Just annotationType:
                checkExpression envWithContext annotationType def.body @state & typeCa2Ta @state envWithContext annotationType
            Nothing:
                inferExpression envWithContext def.body @state

    # TODO clean all the `== Nothing`, maybe put them together

    if patternOut.maybeFullAnnotation == Nothing then
        addEquality envWithContext (CA.patternPos def.pattern) Why_LetIn patternOut.patternType bodyType @state
    else
        None

    tyvars as Dict TA.UnificationVariableId TA.TypeClasses =
        if patternOut.maybeFullAnnotation == Nothing then
            Dict.diff (TA.typeTyvars patternOut.patternType) env.nonFreeTyvars
            >> Dict.map tyvarId: None: { allowFunctions = Nothing, allowUniques = Nothing }
        else
            Dict.empty
            >> Dict.for def.tyvars name: typeClasses:
                try Dict.get name envWithContext.annotatedTyvarToGeneratedTyvar as
                    Nothing: todo << "compiler error: missing annotatedTyvarToGeneratedTyvar for " .. name
                    Just tyvarId: Dict.insert tyvarId typeClasses

    nonFreeTyvars =
        if patternOut.maybeFullAnnotation /= Nothing then
            patternOut.env.nonFreeTyvars
        else
            patternOut.env.nonFreeTyvars >> Dict.for (TA.typeTyvars patternOut.patternType) (k: v: Dict.insert k None)

    {
    , pattern = patternOut.typedPattern
    , native = def.native
    , body = typedBody
    , directValueDeps = def.directValueDeps
    , tyvars
    , isFullyAnnotated = patternOut.maybeFullAnnotation /= Nothing
    }
    &
    { patternOut.env with nonFreeTyvars }



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
            TA.LiteralNumber pos n & (typeCa2Ta @state env CoreTypes.number)


        CA.LiteralText pos text:
            TA.LiteralText pos text & (typeCa2Ta @state env CoreTypes.text)


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
                checkExpression { env with context = Context_IfCondition } CoreTypes.bool condition @state

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



insertNonFreeTyvars as TA.Type: Env: Env =
      type: env:

      { env with
      , nonFreeTyvars = .nonFreeTyvars >> Dict.for (TA.typeTyvars type) id: _: Dict.insert id None
      }


inferParam as Env: CA.Parameter: State@: TA.Parameter & TA.Type & Env =
    env: par: state@:

    try par as
        CA.ParameterPattern pa:
            patternOut =
                inferPattern env True pa @state

            TA.ParameterPattern patternOut.typedPattern & patternOut.patternType & insertNonFreeTyvars patternOut.patternType patternOut.env

        CA.ParameterRecycle pos name:

            # TODO check name already in env?

            type = TA.TypeUnique pos (newType @state)

            typeWithClasses as Instance =
                {
                , definedAt = pos
                , type
                , tyvars = Dict.empty
                }

            newEnv as Env =
                { env with variables = Dict.insert (CA.RefLocal name) typeWithClasses .variables }
                >> insertNonFreeTyvars type

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


checkParameter as Env: Bool: CA.Type: CA.Parameter: State@: TA.Parameter & Env =
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
                , type = typeCa2Ta @state env expectedType
                # TODO not sure about this here???
                , tyvars = Dict.empty
                }

            localEnv as Env =
                # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                { env with variables = Dict.insert (CA.RefLocal name) variable .variables }

            TA.ParameterRecycle pos name & localEnv




checkExpression as Env: CA.Type: CA.Expression: State@: TA.Expression =
    env: expectedType: caExpression: state@:

    try caExpression & expectedType as

        CA.LiteralNumber pos n & CA.TypeNamed _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                addError env pos (ErrorIncompatibleTypes caExpression expectedType) @state
            else
                None

            TA.LiteralNumber pos n


        CA.LiteralText pos text & CA.TypeNamed _ typeUsr []:
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
                    addEquality env pos Why_Annotation var.type (typeCa2Ta @state env expectedType) @state

            TA.Variable pos ref


        CA.Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    addError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    addEquality env pos Why_Annotation cons.type (typeCa2Ta @state env expectedType) @state

            TA.Constructor pos usr


        CA.Fn pos pars body & CA.TypeFn _ parsT out:

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

                        Array.push @typedPars (typedPar & typeCa2Ta @state env parT)
                        @parIndex += 1

                        envX1

                typedBody =
                    checkExpression localEnv out body @state

                TA.Fn pos (Array.toList typedPars) typedBody


        CA.Call pos reference args & _:
            checkCallCo env (typeCa2Ta @state env expectedType) pos reference args @state


        CA.Record pos (Just ext) valueByName & CA.TypeRecord _ typeByName:

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


        CA.Record pos Nothing valueByName & CA.TypeRecord _ typeByName:
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
                >> typeCa2Ta @state env
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
                checkExpression { env with context = Context_IfCondition } CoreTypes.bool condition @state

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


        _ & CA.TypeError pos:
            TA.Error pos


        _:
            # TODO (todo "CA.expressionPos caExpression")
            addError env Pos.G (ErrorIncompatibleTypes caExpression expectedType) @state
            TA.LiteralText Pos.G "todo?"




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
    env: isParam: pattern: state@:

    try pattern as
        CA.PatternAny pos { isUnique, maybeName, maybeAnnotation }:

            (patternType as TA.Type) & (env1 as Env) =
                try maybeAnnotation as
                    Nothing:
                        if isUnique then
                            (TA.TypeUnique pos << newType @state) & env
                        else
                            newType @state & env

                    Just annotation:

                        blahEnv =
                            { env with
                            , annotatedTyvarToGeneratedTyvar =
                                env.annotatedTyvarToGeneratedTyvar
                                >> Dict.for (CA.typeTyvars annotation) name: _:
                                      Dict.update name (v: v >> Maybe.withDefault (newTyvarId @state) >> Just)
                            }

                        t =
                            typeCa2Ta @state blahEnv annotation

                        if variableOfThisTypeMustBeFlaggedUnique t /= isUnique then
                            addError env pos ErrorUniquenessDoesNotMatch @state
                        else
                            None

                        t & blahEnv


            env2 as Env =
                try maybeName as

                    Nothing:
                        env1

                    Just name:
                        [#
                           Function parameters instead are **totally determined by how they are used in the function's body**,
                           so they are always added to the scope as non-free type variables.
                           If we turned them into free type variables, the block would not be able to constrain them.

                           Defined instance values are **totally determined by their definition**, how they are used must not
                           affect their type, so each time they are used they can be used with different free type vars.

                           TODO: what about instance variables added by try..as patterns?


                           ?????? Is this still valid for uniques?
                           Mutables can mutate to any value of a given specific type, so they can't be polymorphic.
                           i.e: `x = Nothing` has type `Maybe a`, but once I mutate it to `@x := Just 1` its type would
                           change to `Maybe Number` and we don't want that.
                           ?????

                        #]
                        tyvars =
                            # TODO should tyvar be only for annotated tyvars? (ie, to store typeclasses?)
                            if isParam then
                                Dict.empty
                            else
                                #]
                                patternType
                                >> TA.typeTyvars
                                >> Dict.map k: v: { allowFunctions = Nothing, allowUniques = Nothing }

                        variable as Instance =
                            {
                            , definedAt = pos
                            , type = patternType
                            , tyvars
                            }

                        nonFreeTyvars =
                            if isParam then
                                Dict.join (TA.typeTyvars patternType) env1.nonFreeTyvars
                            else
                                env1.nonFreeTyvars

                        # We don't check for duplicate var names / shadowing here, it's MakeCanonical's responsibility
                        { env1 with
                        , variables = Dict.insert (CA.RefLocal name) variable .variables
                        , nonFreeTyvars
                        }

            typedPattern =
                TA.PatternAny pos { isUnique, maybeName, maybeAnnotation, type = patternType }

            {
            , typedPattern
            , patternType
            , env = env2
            , maybeFullAnnotation = maybeAnnotation
            }


        CA.PatternLiteralText pos text:
            {
            , typedPattern = TA.PatternLiteralText pos text
            , patternType = typeCa2Ta @state env CoreTypes.text
            , env
            , maybeFullAnnotation = Just CoreTypes.text
            }


        CA.PatternLiteralNumber pos n:
            {
            , typedPattern = TA.PatternLiteralNumber pos n
            , patternType = typeCa2Ta @state env CoreTypes.number
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
                            todo "linearizeCurriedParameters (generalize env cons @state) []"

                        rl =
                            List.length argModAndTypes

                        gl =
                            List.length arguments

                        if rl > gl then
                            addError env pos ErrorNotEnoughArguments @state
                        else
                            None

                        if rl < gl then
                            addError env pos ErrorNotEnoughArguments @state
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



checkPattern as Env: CA.Type: Bool: CA.Pattern: State@: TA.Pattern & Env =
    env: expectedType: isParam: pattern: state@:

    # TODO
    out = inferPattern env isParam pattern @state

    addEquality env (CA.patternPos pattern) Why_Todo out.patternType (typeCa2Ta @state env expectedType) @state

    out.typedPattern & out.env



#
# Def Resolution
#
# After we have resolved all equalities, we want to go through each def and replace all tyvars with the resolved type
#
resolveTypesInValueDef as Dict TA.UnificationVariableId TA.Type: TA.ValueDef: TA.ValueDef =
    substitutions: def:

    typeClasses =
        {
        , allowFunctions = Nothing
        , allowUniques = Nothing
        }

    tyvars =
        if def.isFullyAnnotated then
            def.tyvars
        else
            Dict.empty
            >> Dict.for def.tyvars tyvarId: typeClasses: acc:
                try Dict.get tyvarId substitutions as
                    Nothing: acc
                    Just type: Dict.join (Dict.map (k: v: typeClasses ) << TA.typeTyvars type) acc

    # TODO: actually resolve all types

    { def with tyvars }



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


doModule as State: Env: CA.Module: Res TA.Module =
    state0: env: caModule:

    Debug.benchStart None

    # state is per module
    state as State @=
        state0

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
        , substitutions = Dict.empty
        , errors = []
        }

#    List.each (Array.toList state.equalities) eq:
#        log "EQ" eq

    erStateF =
        solveEqualities (Array.toList state.equalities) erState0

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

    nameToIdAndClasses as Dict Name (TA.UnificationVariableId & TA.TypeClasses) =
        def.tyvars
        >> Dict.map name: classes: newTyvarId @state & classes

    nameToType as Dict Name TA.Type =
        nameToIdAndClasses >> Dict.map k: (id & classes): TA.TypeUnificationVariable id

    unificationIdToClasses as Dict TA.UnificationVariableId TA.TypeClasses =
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
                    , tyvars = Dict.intersect unificationIdToClasses (TA.typeTyvars type)
                    }

                { envX with variables = Dict.insert ref instance .variables }


addConstructorToGlobalEnv as State@: Dict Name TA.Type: Name: CA.Constructor: Env: Env =
    state@: paramsByName: name: caConstructor: env:

    USR umr _ =
        caConstructor.typeUsr

    type as TA.Type =
        expandParamsAndAliases @state Context_Global env.expandedAliases paramsByName caConstructor.type

    tyvars as Dict TA.UnificationVariableId CA.TypeClasses =
        type
        >> TA.typeTyvars
        >> Dict.map k: v: { allowFunctions = Just True, allowUniques = Just True }

    taConstructor as Instance =
        {
        , definedAt = Pos.G
        , type
        , tyvars
        }

    { env with constructors = Dict.insert (USR umr name) taConstructor .constructors }


addUnionTypeAndConstructorsToGlobalEnv as State@: a: CA.UnionDef: Env: Env =
    state@: _: caUnionDef: env:

    paramsByName as Dict Name TA.Type =
        caUnionDef.pars
        >> List.map ((At pos name): name & newType @state)
        >> Dict.fromList

    { env with exactTypes = Dict.insert caUnionDef.usr caUnionDef.pars .exactTypes }
    >> Dict.for caUnionDef.constructors (addConstructorToGlobalEnv @state paramsByName)








namedParsToIdParsAndDict as [At Name]: [TA.UnificationVariableId] & Dict Name TA.Type =
    atPars:

    idPars =
        atPars
        >> List.indexedMap (index: atName: index)

    typeByName =
        atPars
        >> List.indexedMap (index: (At pos name): name & TA.TypeUnificationVariable index)
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





initStateAndGlobalEnv as [CA.Module]: State & Compiler/TypeCheck.Env =
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

    { initEnv with
    , expandedAliases
    }
    >> List.for CoreTypes.allDefs (addUnionTypeAndConstructorsToGlobalEnv @state None)
    >> List.for allModules doStuff
    >> Tuple.pair state






#
#
# Equalities resolution
#
#
alias ERState = {
    , substitutions as Dict TA.UnificationVariableId TA.Type
    , errors as [Equality & Text]
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


solveEqualities as [Equality]: ERState: ERState =
    remainingEqualities: state:

    try remainingEqualities as
        []:
            state

        head :: tail:

            Equality context pos why type1 type2 =
                head

            try type1 & type2 as

                TA.TypeUnificationVariable tyvarId & t2:
                    replaceUnificationVariable tyvarId t2 tail state


                t1 & TA.TypeUnificationVariable tyvarId:
                    replaceUnificationVariable tyvarId t1 tail state


                TA.TypeExact _ usr1 args1 & TA.TypeExact _ usr2 args2:
                    if usr1 /= usr2 then
                        state
                        >> addErError head "types are incompatible"
                        >> solveEqualities tail
                    else
                        newEqualities as [Equality] =
                            List.indexedMap2 (index: Equality context pos (Why_TypeArgument usr1 index why)) args1 args2

                        solveEqualities (List.append tail newEqualities) state


                TA.TypeFn _ pars1 out1 & TA.TypeFn _ pars2 out2:
                    if List.length pars1 /= List.length pars2 then
                        state
                        >> addErError head "functions expect a different number of arguments"
                        >> solveEqualities tail

                    else
                        both =
                            List.map2 Tuple.pair pars1 pars2

                        inEqualities as [Equality] =
                            both >> List.indexedMap index: ((m1 & in1) & (m2 & in2)):
                                Equality context pos (Why_FunctionInput index why) in1 in2

                        outEquality as Equality =
                            Equality context pos (Why_FunctionOutput why) out1 out2

                        state
                        >> List.for both (((m1 & _) & (m2 & _)): addErErrorIf (m1 /= m2) head "argument modifiers don't match")
                        >> solveEqualities (List.concat [inEqualities, [outEquality], tail])


                TA.TypeRecord _ attrs1 & TA.TypeRecord _ attrs2:
                    only1 & both & only2 =
                        onlyBothOnly attrs1 attrs2

                    newEqualities as [Equality] =
                        tail >> Dict.for both attrName: (attrType1 & attrType2): eqs:
                              Equality context pos (Why_Attribute why) attrType1 attrType2 :: eqs

                    state
                    >> addErErrorIf (only1 /= Dict.empty or only2 /= Dict.empty) head "record attrs don't match"
                    >> solveEqualities newEqualities


                TA.TypeRecordExt tyvar1 attrs1 & TA.TypeRecord _ attrs2:
                    solveRecordExt why tyvar1 attrs1 attrs2 tail state


                TA.TypeRecord _ attrs1 & TA.TypeRecordExt tyvar2 attrs2:
                    solveRecordExt why tyvar2 attrs2 attrs1 tail state


                TA.TypeRecordExt tyvar1 attrs1 & TA.TypeRecordExt tyvar2 attr2:
                    todo "will this actually happen?"


                TA.TypeUnique _ m1 & TA.TypeUnique _ m2:
                    solveEqualities (Equality context pos why m1 m2 :: tail) state


                _:
                    state
                    >> addErError head "types are incompatible"
                    >> solveEqualities tail



#solveNamedVsNonAlias as USR: [TA.Type]: TA.Type: Equality: [Equality]: ERState: ERState =
#    usr1: args1: t2: head: tail: state:
#
#    try Dict.get usr1 state.namedTypes as
#        Nothing:
#            todo << "namedType not found: " .. Debug.toHuman usr1
#
#        Just (AliasType aliasDef):
#
#            Equality context pos why _ _ = head
#
#            solveEqualities (Equality context pos why t2 (expandAlias aliasDef args1) :: tail) state
#
#        _:
#            state
#            >> addErError head "these named types are incompatible")
#            >> solveEqualities tail




solveRecordExt as Why: TA.UnificationVariableId: Dict Name TA.Type: Dict Name TA.Type: [Equality]: ERState: ERState =
    why: tyvar1: attrs1: type2: remainingEqualities:

    todo "solveRecordExt"
#            # { attrs1 } == { tyvar2 with attrs2 }
#            only1 & both & only2 =
#                onlyBothOnly attrs1 attrs2
#
#            only2 must be empty
#
#            tyvar2 must == attrs1
#
#            all `both` must match









replaceUnificationVariable as TA.UnificationVariableId: TA.Type: [Equality]: ERState: ERState =
    tyvarId: replacingType: remainingEqualities: state:

    #TODO: check that replacingType does not contain tyvarId

    newEqualities =
        # TODO we don't care about map preserving order
        remainingEqualities >> List.map (Equality context pos why t1 t2):
            Equality context pos why
                (applySubstitutionToType tyvarId replacingType t1)
                (applySubstitutionToType tyvarId replacingType t2)

    substitutions =
        state.substitutions
        >> Dict.map (tyvarId: type: applySubstitutionToType tyvarId replacingType type)
        >> Dict.insert tyvarId replacingType

    solveEqualities newEqualities { state with substitutions }



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

