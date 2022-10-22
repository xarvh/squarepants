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


alias TypeWithClasses = {
    , type as TA.Type
    , tyvars as Dict TA.UnificationVariableId TA.TypeClasses
    }


alias Env = {
    , context as Context
    , constructors as Dict Meta.UniqueSymbolReference TypeWithClasses
    , variables as Dict CA.Ref TypeWithClasses
    , tyvarsInParentAnnotations as Dict TA.UnificationVariableId TA.Type

    # This is used to give meaningfule errors?
    , annotatedTyvarToGeneratedTyvar as Dict Name TA.UnificationVariableId
    }



union Error_ =
    , ErrorVariableNotFound CA.Ref
    , ErrorConstructorNotFound Meta.UniqueModuleReference
    , ErrorNotCompatibleWithRecord
    , ErrorRecordDoesNotHaveAttribute Name [# TODO other attrs to give context? #]
    , ErrorRecordHasAttributesNotInAnnotation # TODO which attrs?
    , ErrorRecordIsMissingAttibutesInAnnotation # TODO which attrs?
    , ErrorTryingToAccessAttributeOfNonRecord Name TA.Type
    , ErrorIncompatibleTypes
    , ErrorVariableTypeIncompatible CA.Ref TypeWithClasses CA.CanonicalType
    , ErrorConstructorTypeIncompatible Meta.UniqueSymbolReference TA.Type CA.CanonicalType
    , ErrorCallingANonFunction
    , ErrorTooManyArguments
    , ErrorNotEnoughArguments
    , ErrorIncompatibleLambdaModifier
    , ErrorUniquenessDoesNotMatch




union Context =
    , Context_Module Meta.UniqueModuleReference
    , Context_Argument Name Context
    , Context_LetInBody
    , Context_LambdaBody Pos Context
    , Context_TryBranch
    , Context_IfCondition
    , Context_IfFalse
    , Context_IfTrue


union Why =
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
    , Why_FunctionInput Why
    , Why_FunctionOutput Why
    , Why_TypeArgument Meta.UniqueSymbolReference Int Why


union Equality =
    , Equality Context Why TA.Type TA.Type






newTyvarId as State@: TA.UnificationVariableId =
    state@:
    @state.lastUnificationVarId += 1
    state.lastUnificationVarId



newType as State@: TA.Type =
    state@:
    TypeExtra (TypeUnificationVariable (newTyvarId @state))


addEquality as Env: Why: TA.Type: TA.Type: State@: None =
    # TODO: should t1 be "expected" and t2 "actual" or something like that?
    env: why: t1: t2: state@:

    Array.push @state.equalities << Equality env.context why t1 t2



addError as Env: Pos: Error_: State@: None =
    env: pos: error: state@:

    Array.push @state.errors (pos & env.context & error)




linearizeCurriedParameters as CA.Type: [LambdaModifier & CA.Type]: [LambdaModifier & CA.Type] & CA.Type =
    type: accum:

    try type as
        TypeFunction pos from modifier to:
            linearizeCurriedParameters to << (modifier & from) :: accum

        _:
            List.reverse accum & type



getConstructorByUsr as Meta.UniqueSymbolReference: Env: Maybe TypeWithClasses =
    usr: env:

    Dict.get usr env.constructors


getVariableByRef as CA.Ref: Env: Maybe TypeWithClasses =
    ref: env:

    Dict.get ref env.variables


#
#
# Generalize
#
#
generalize as Env: TypeWithClasses: State@: TA.Type =
    env: typeWithClasses: state@:

    typeWithClasses.type >> Dict.for typeWithClasses.tyvars tyvarId: typeClasses:

        replacementType =
            try Dict.get tyvarId env.tyvarsInParentAnnotations as
                Just type:
                    # TODO also check that typeclasses are compatible? Should MakeCanonical do it?
                    type

                Nothing:
                    tyvarId =
                        newTyvarId @state

                    # We need to remember that this new tyvar has typeclass contraints
                    Hash.insert @state.classesByTyvarId tyvarId typeClasses

                    TypeExtra (TypeUnificationVariable tyvarId)

        applySubstitutionToType tyvarId replacementType


#
#
# Types
#
#
variableIsCompatibleWith as Env: CA.CanonicalType: TA.Type: Bool =
    # This function is used to ensure that when a variable (or a constructor)
    # from env can be used with the annotated/expected type
    env: expected: variableType:

    try expected & variableType as
        TypeOpaque _ usrC argsC & TypeOpaque _ usrU argsU:
            if usrC /= usrU then
                False
            else
                # TODO this is not efficient, we should stop at the first
                List.map2 (variableIsCompatibleWith env) argsC argsU
                >> List.all identity

        TypeFunction _ inC modC outC & TypeFunction _ inU modU outU:
            modC /= modU and variableIsCompatibleWith env inC inU and variableIsCompatibleWith env outC outU

        TypeRecord _ attrC & TypeRecord _ attrU:
            onlyC & both & onlyU =
                onlyBothOnly attrC attrU

            if onlyC /= Dict.empty or onlyU /= Dict.empty then
                False
            else
                both
                >> Dict.values
                >> List.all (c & u): variableIsCompatibleWith env c u

        TypeUnique _ c & TypeUnique _ u:
            variableIsCompatibleWith env c u

        TypeExtra (TypeAnnotationVariable _ nameC) & TypeExtra (TypeUnificationVariable idU):
            Dict.get nameC env.annotatedTyvarToGeneratedTyvar == Just idU

        #TypeRecord _ attrsC & TypeExtra (TypeRecordExt idU attrsU):
        #    ????

        # TODO Probably needs to manage more TypeUnificationVariable?

        _:
            False


canonicalToUnificationType as Env: CA.CanonicalType: TA.Type =
    env: ca:

    ctu = canonicalToUnificationType env

    try ca as
       TypeOpaque p usr args:
          TypeOpaque p usr (List.map ctu args)

       TypeFunction p in mod out:
         TypeFunction p (ctu in) mod (ctu out)

       TypeRecord p attrs:
         TypeRecord p (Dict.map (k: ctu) attrs)

       TypeUnique p ty:
         TypeUnique p (ctu ty)

       TypeExtra (TypeAnnotationVariable _ name):
          try Dict.get name env.annotatedTyvarToGeneratedTyvar as
              Nothing:
                  todo "compiler error: canonicalToUnificationType: name not found"
              Just tyvarId:
                  TypeExtra (TypeUnificationVariable tyvarId)


variableOfThisTypeMustBeFlaggedUnique as TA.Type: Bool =
    ca:

    try ca as
       TypeUnique p ty:
         True

       TypeOpaque p usr args:
          # TODO: How do we deal with an Opaque that *must* be unique?
          List.any variableOfThisTypeMustBeFlaggedUnique args

       TypeFunction p in mod out:
          False

       TypeRecord p attrs:
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TypeExtra (TypeRecordExt tyvarId attrs):
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TypeExtra (TypeUnificationVariable _):
          # At worst a tyvar can be ImmutableOrUnique, in which case it still doesn't need to be flagged
          False






#
#
# Definitions
#
#
doDefinition as Env: CA.ValueDef: State@: TA.ValueDef & Env =
    env: def: state@:

    patternOut =
        inferPattern env def.pattern @state

    newEnv =
        { patternOut.env with
        , context = Context_LetInBody
        }

    # TODO do not check body if native!

    typedBody & bodyType =
        try patternOut.maybeFullAnnotation as
            Just annotationType:
                # TODO Should I use patternOut.type or (canonicalToUnificationType annotationType)?
                checkExpression newEnv annotationType def.body @state & patternOut.patternType
            Nothing:
                inferExpression newEnv def.body @state

    if patternOut.maybeFullAnnotation == Nothing then
        addEquality newEnv Why_LetIn patternOut.patternType bodyType @state
    else
        None

    {
    , pattern = patternOut.typedPattern
    , native = def.native
    , body = typedBody
    }
    &
    patternOut.env



#
#
# Expressions
#
#
inferExpression as Env: CA.Expression: State@: TA.Expression & TA.Type =
    env: caExpression: state@:

    try caExpression as

        # TODO would be nice to be able to say that from here on, `caExpression as Expression any`
        LiteralNumber pos n:
            LiteralNumber pos n & todo "CoreTypes.number"


        LiteralText pos text:
            LiteralText pos text & todo "CoreTypes.text"


        Variable pos ref:
            ty =
                try getVariableByRef ref env as
                    Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        newType @state

                    Just var:
                        generalize env var @state

            Variable pos ref & ty


        Constructor pos usr:
            ty =
                try getConstructorByUsr usr env as
                    Nothing:
                        addError env pos (ErrorConstructorNotFound usr) @state
                        newType @state

                    Just cons:
                        generalize env cons @state

            Constructor pos usr & ty


        Lambda pos pattern modifier body:

            patternOut =
                inferPattern env pattern @state

            newEnv =
                { patternOut.env with
                , context = Context_LambdaBody pos .context
                }

            unificationBody & bodyType =
                inferExpression newEnv body @state

            type =
                TypeFunction Pos.G
                    patternOut.patternType
                    modifier
                    bodyType

            exp =
                Lambda pos patternOut.typedPattern modifier unificationBody

            exp & type


        Call pos reference argument unusedType:

            callType =
                newType @state

            checkCallCo env callType pos reference [argument & unusedType] @state & callType


        CallCo pos reference argsAndUnusedTypes:

            callType =
                newType @state

            checkCallCo env callType pos reference argsAndUnusedTypes @state & callType


        Record pos Nothing attrs:

            typedValueAndValueTypeByName =
                attrs >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName as Dict Name (Expression) =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name TA.Type =
                Dict.map (k: Tuple.second) typedValueAndValueTypeByName

            Record pos Nothing typedValueByName & TypeRecord pos valueTypeByName


        Record pos (Just ext) attrExpressions:

            typedValueAndValueTypeByName as Dict Name (Expression & TA.Type) =
                attrExpressions >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName as Dict Name (Expression) =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name TA.Type =
                Dict.map (k: Tuple.second) typedValueAndValueTypeByName

            typedExt & extType =
                inferExpression env ext @state

            Record pos (Just typedExt) typedValueByName & inferRecordExtended env pos extType valueTypeByName @state


        RecordAccess pos attrName recordExpression:

            typedExpr & inferredType =
                inferExpression env recordExpression @state

            RecordAccess pos attrName typedExpr & inferRecordAccess env pos attrName inferredType @state


        LetIn def rest:

            typedDef & defEnv =
                doDefinition env def @state

            typedRest & restType =
                inferExpression defEnv rest @state

            LetIn typedDef typedRest & restType


        If pos { condition, true, false }:

            typedCondition =
                checkExpression { env with context = Context_IfCondition } (todo "CoreTypes.bool") condition @state

            typedTrue & trueType =
                inferExpression { env with context = Context_IfTrue } true @state

            typedFalse & falseType =
                inferExpression { env with context = Context_IfFalse } false @state

            addEquality env Why_IfBranches trueType falseType @state

            expression =
                If pos {
                    , condition = typedCondition
                    , true = typedTrue
                    , false = typedFalse
                    }

            expression & trueType


        Try pos { value, type = _, patternsAndExpressions }:

            typedValue & valueType =
                inferExpression env value @state

            finalType =
                newType @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    patternOut as PatternOut =
                        inferPattern env pa @state

                    addEquality env Why_TryPattern patternOut.patternType valueType @state

                    newEnv =
                        { patternOut.env with
                        , context = Context_TryBranch
                        }

                    typedExpression & expressionType =
                        inferExpression newEnv exp @state

                    addEquality newEnv Why_TryExpression finalType expressionType @state

                    patternOut.typedPattern & typedExpression

            Try pos { value = typedValue, type = valueType, patternsAndExpressions = typedPatternsAndExpressions } & finalType


        DestroyIn name expression:
            typedExpression & expressionType =
                inferExpression env expression @state

            DestroyIn name typedExpression & expressionType


inferRecordAccess as Env: Pos: Name: TA.Type: State@: TA.Type =
    env: pos: attrName: inferredType: state@:

    try inferredType as
        TypeRecord _ attrTypes:
            try Dict.get attrName attrTypes as
                Just type:
                    type

                Nothing:
                    addError env pos (ErrorRecordDoesNotHaveAttribute attrName) @state
                    newType @state

        TypeExtra (TypeRecordExt tyvarId extensionAttrTypes):
            try Dict.get attrName extensionAttrTypes as
                Just type:
                    type

                Nothing:
                    newExtId = newTyvarId @state
                    newAttrType = newType @state

                    type = TypeExtra << TypeRecordExt newExtId (Dict.insert attrName newAttrType extensionAttrTypes)

                    addEquality env Why_RecordAccess (TypeExtra << TypeUnificationVariable tyvarId) type @state

                    newAttrType


        TypeExtra (TypeUnificationVariable id):
            newExtId = newTyvarId @state
            newAttrType = newType @state

            type as TA.Type =
                TypeExtra << TypeRecordExt newExtId (Dict.singleton attrName newAttrType)

            addEquality env Why_RecordAccess (TypeExtra << TypeUnificationVariable id) type @state

            newAttrType

        _:
            addError env pos (ErrorTryingToAccessAttributeOfNonRecord attrName inferredType) @state
            newType @state



inferRecordExtended as Env: Pos: TA.Type: Dict Name TA.Type: State@: TA.Type =
    env: pos: extType: valueTypeByName: state@:

    try extType as
        TypeRecord _ attrTypes:

            Dict.each valueTypeByName name: valueType:
                try Dict.get name attrTypes as

                    Nothing:
                        addError env pos (ErrorRecordDoesNotHaveAttribute name) @state

                    Just ty:
                        addEquality env Why_Record ty valueType @state

            extType


        TypeExtra (TypeRecordExt tyvarId extensionAttrTypes):

            expressionOnly & both & extensionOnly =
                onlyBothOnly valueTypeByName extensionAttrTypes

            Dict.each both name: (inAttr & extAttr):
                addEquality env Why_Record inAttr extAttr @state

            # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
            newExtId =
                newTyvarId @state

            TypeExtra (TypeRecordExt newExtId (Dict.join valueTypeByName extensionOnly))


        TypeExtra (TypeUnificationVariable id):
            ty = TypeExtra (TypeRecordExt id valueTypeByName)

            addEquality env Why_RecordExt extType ty @state

            ty

        _:
            addError env pos ErrorNotCompatibleWithRecord @state
            newType @state



checkExpression as Env: CA.CanonicalType: CA.Expression: State@: TA.Expression =
    env: expectedType: caExpression: state@:

    try caExpression & expectedType as

        LiteralNumber pos n & TypeOpaque _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                addError env pos ErrorIncompatibleTypes @state
            else
                None

            LiteralNumber pos n


        LiteralText pos text & TypeOpaque _ typeUsr []:
            if typeUsr /= CoreTypes.textDef.usr then
                addError env pos ErrorIncompatibleTypes @state
            else
                None

            LiteralText pos text


        Variable pos ref & _:
            try getVariableByRef ref env as
                Nothing:
                    addError env pos (ErrorVariableNotFound ref) @state

                Just var:
                    if variableIsCompatibleWith env expectedType var.type then
                        None
                    else
                        addError env pos (ErrorVariableTypeIncompatible ref var expectedType) @state

            Variable pos ref


        Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    addError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    if variableIsCompatibleWith env expectedType cons.type then
                        None
                    else
                        addError env pos (ErrorConstructorTypeIncompatible usr cons.type expectedType) @state

            Constructor pos usr


        Lambda pos pattern lambdaModifier body & TypeFunction _ in mod out:
            typedPattern & localEnv =
                checkPattern env in pattern @state

            typedBody =
                checkExpression localEnv out body @state

            if lambdaModifier == mod then
                None
            else
                addError env pos ErrorIncompatibleLambdaModifier @state

            Lambda pos typedPattern mod typedBody


        Call pos reference argument unusedType & _:
            checkCallCo env (canonicalToUnificationType env expectedType) pos reference [argument & unusedType] @state


        CallCo pos reference argsAndUnusedTypes & _:
            checkCallCo env (canonicalToUnificationType env expectedType) pos reference argsAndUnusedTypes @state


        Record pos (Just ext) valueByName & TypeRecord _ typeByName:

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

            Record pos (Just typedExt) typedValueByName


        Record pos Nothing valueByName & TypeRecord _ typeByName:
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

            Record pos Nothing typedAttrs


        RecordAccess pos attrName exp & _:

            typedExpression & expressionType =
                inferExpression env exp @state

            newId =
                newTyvarId @state

            requiredType =
                TypeExtra << TypeRecordExt newId (Dict.singleton attrName expectedType)

            addEquality env Why_AttributeAccess expressionType requiredType @state

            RecordAccess pos attrName typedExpression


        LetIn def rest & _:

            typedDef & defEnv =
                doDefinition env def @state

            typedRest =
                checkExpression defEnv expectedType rest @state

            LetIn typedDef typedRest


        If pos { condition, true, false } & _:

            typedCondition =
                checkExpression { env with context = Context_IfCondition } (todo "CoreTypes.bool") condition @state

            typedTrue =
                checkExpression { env with context = Context_IfTrue } expectedType true @state

            typedFalse =
                checkExpression { env with context = Context_IfFalse } expectedType false @state

            If pos {
                , condition = typedCondition
                , true = typedTrue
                , false = typedFalse
                }


        Try pos { value, type = __unused__, patternsAndExpressions } & _:

            typedValue & valueType =
                inferExpression env value @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    inferredPattern =
                        inferPattern env pa @state

                    newEnv =
                        { inferredPattern.env with
                        , context = Context_TryBranch
                        }

                    addEquality env Why_TryPattern inferredPattern.patternType valueType @state

                    typedExpression =
                        checkExpression newEnv expectedType exp @state

                    inferredPattern.typedPattern & typedExpression

            Try pos {
                , value = typedValue
                , type = valueType
                , patternsAndExpressions = typedPatternsAndExpressions
                }


        DestroyIn name exp & _:
            DestroyIn name << checkExpression env expectedType exp @state


        _:
            addError env (todo "CA.expressionPos caExpression") ErrorIncompatibleTypes @state
            LiteralText Pos.G "todo?"




checkCallCo as Env: TA.Type: Pos: CA.Expression: [CA.Argument & unusedType]: State@: TA.Expression =
    env: expectedType: pos: reference: givenArgs: state@:

    typedReference & referenceType =
        inferExpression env reference @state

    typedArgumentsAndArgumentTypes as [CA.Argument TA.Type & TA.Type] =
        givenArgs >> List.map (arg & unusedType):
            inferArgument env arg @state

    referenceArgs & referenceReturn =
        linearizeCurriedParameters referenceType []

    addEquality env Why_ReturnType referenceReturn expectedType @state

    if referenceArgs /= [] then

        rl =
            List.length referenceArgs

        gl =
            List.length givenArgs

        if rl > gl then
            addError env pos ErrorNotEnoughArguments @state

        else if rl < gl then
            addError env pos ErrorTooManyArguments @state

        else
            None

        list_eachWithIndex2 0 referenceArgs typedArgumentsAndArgumentTypes index: (refMod & refType): (tyArg & argTy):
            addEquality env (Why_Argument index) refType argTy @state

        CallCo pos typedReference typedArgumentsAndArgumentTypes

    else
        try referenceType as
            TypeExtra (TypeUnificationVariable id):
                None
            _:
                addError env pos ErrorCallingANonFunction @state

        referenceTypeFromArguments =
            expectedType
            >> List.forReversed typedArgumentsAndArgumentTypes (tyArg & argTy): type:
                # TODO if expected type says this should be LambdaConsuming, then use LambdaConsuming
                TypeFunction Pos.G argTy LambdaNormal type

        addEquality env Why_CalledAsFunction referenceType referenceTypeFromArguments @state

#        typedArgs as [Argument TA.Type & TA.Type] =
#            List.map Tuple.first typedArgumentsAndArgumentTypes

        CallCo pos typedReference typedArgumentsAndArgumentTypes



inferArgument as Env: CA.Argument: State@: TA.Argument & TA.Type =
    env: arg: state@:

    try arg as
        ArgumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            ArgumentExpression typedExp & expType

        ArgumentRecycle pos attrPath ref:
            type =
                try getVariableByRef ref env as

                    Nothing:
                        addError env pos (ErrorVariableNotFound ref) @state
                        newType @state

                    Just var:
                        todo "apply attrPath"
                        var.type

            ArgumentRecycle pos attrPath ref & type


#
#
# Patterns
#
#
alias PatternOut = {
    , patternType as TA.Type
    , typedPattern as TA.Pattern
    , maybeFullAnnotation as Maybe CA.CanonicalType
    , env as Env
    }


inferPattern as Env: CA.Pattern: State@: PatternOut =
    env: pattern: state@:

    try pattern as
        PatternAny pos { isUnique, maybeName, maybeAnnotation, type = __unused__ }:

            patternType =
                try maybeAnnotation as
                    Nothing:
                        if isUnique then
                            TypeUnique pos << newType @state
                        else
                            newType @state

                    Just annotation:
                        t =
                            canonicalToUnificationType env annotation

                        if variableOfThisTypeMustBeFlaggedUnique t /= isUnique then
                            addError env pos ErrorUniquenessDoesNotMatch @state
                        else
                            None

                        t



            newEnv =
                try maybeName as

                    Nothing:
                        env

                    Just name:
                        # We don't check for duplicate var names / shadowig here, it's MakeCanonical's responsibility
                        { env with variables = Dict.insert (RefLocal name) { type = patternType, tyvars = todo "???" } .variables }

            typedPattern =
                PatternAny pos { isUnique, maybeName, maybeAnnotation, type = patternType }

            {
            , typedPattern
            , patternType
            , env = newEnv
            , maybeFullAnnotation = maybeAnnotation
            }


        PatternLiteralText pos text:
            {
            , typedPattern = PatternLiteralText pos text
            , patternType = todo "CoreTypes.text"
            , env
            , maybeFullAnnotation = todo "CoreTypes.text"
            }


        PatternLiteralNumber pos n:
            {
            , typedPattern = PatternLiteralNumber pos n
            , patternType = todo "CoreTypes.number"
            , env
            , maybeFullAnnotation = todo "CoreTypes.number"
            }


        PatternConstructor pos usr arguments:

            typedArguments & argumentTypes & newEnv =
                [] & [] & env >> List.forReversed arguments arg: (typedPas & paTypes & envX):

                    out =
                        inferPattern envX arg @state

                    (out.typedPattern :: typedPas) & (out.patternType :: paTypes) & out.env

            finalType =
                try getConstructorByUsr usr env as

                    Nothing:
                        addError env pos (ErrorConstructorNotFound usr) @state
                        newType @state

                    Just cons:
                        argModAndTypes & returnType =
                            linearizeCurriedParameters (generalize env cons @state) []

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
                            addEquality env (Why_Argument index) paramType argType @state

                        ##    { x } = blah
                        ##
                        ## `blah` /could/ be unique, but in this case we'll just assume it is NOT

                        if List.any variableOfThisTypeMustBeFlaggedUnique argumentTypes then
                            TypeUnique pos returnType
                        else
                            returnType

            {
            , typedPattern = PatternConstructor pos usr typedArguments
            , patternType = finalType
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }


        PatternRecord pos pasAndUnusedTypesByName:

            typedPatternsAndPatternTypesByName & newEnv =
                Dict.empty & env >> Dict.for pasAndUnusedTypesByName name: (pa & __unused__): (dict & envX):

                    out =
                        inferPattern envX pa @state

                    Dict.insert name (out.typedPattern & out.patternType) dict & out.env

            patternTypeByName =
                typedPatternsAndPatternTypesByName >> Dict.map name: Tuple.second

            {
            , typedPattern = PatternRecord pos typedPatternsAndPatternTypesByName
            , patternType = TypeExtra (TypeRecordExt (newTyvarId @state) patternTypeByName)
            , env = newEnv
            , maybeFullAnnotation = Nothing # TODO
            }



checkPattern as Env: CA.CanonicalType: CA.Pattern: State@: TA.Pattern & Env =
    env: expectedType: pattern: state@:

    # TODO
    out = inferPattern env pattern @state

    addEquality env Why_Todo out.patternType (canonicalToUnificationType env expectedType) @state

    out.typedPattern & out.env



#
#
# Equalities resolution
#
#
alias ERState = {
    , substitutions as Dict TA.UnificationVariableId TA.Type
    , errors as [Why & Text]
    }


addErrorIf as Bool: Why: Text: ERState: ERState =
    test: why: message: state:

    if test then
        { state with errors = (why & message) :: .errors }
    else
        state


solveEqualities as [Equality]: ERState: ERState =
    remainingEqualities: state:

    try remainingEqualities as
        []:
            state

        Equality context why type1 type2 :: tail:
            try type1 & type2 as

                TypeExtra (TypeUnificationVariable tyvarId) & t2:
                    replaceUnificationVariable tyvarId t2 tail state


                t1 & TypeExtra (TypeUnificationVariable tyvarId):
                    replaceUnificationVariable tyvarId t1 tail state


                TypeOpaque _ usr1 args1 & TypeOpaque _ usr2 args2:
                    newEqualities as [Equality] =
                        List.indexedMap2 (index: Equality context (Why_TypeArgument usr1 index why)) args1 args2

                    state
                    >> addErrorIf (usr1 /= usr2) why "types don't match"
                    >> solveEqualities (List.append tail newEqualities)


                TypeFunction _ in1 modifier1 out1 & TypeFunction _ in2 modifier2 out2:
                    newEqualities as [Equality] =
                        Equality context (Why_FunctionInput why) in1 in2 :: Equality context (Why_FunctionOutput why) out1 out2 :: tail

                    state
                    >> addErrorIf (modifier1 /= modifier2) why "lambda modifiers don't match"
                    >> solveEqualities newEqualities


                TypeRecord _ attrs1 & TypeRecord _ attrs2:
                    only1 & both & only2 =
                        onlyBothOnly attrs1 attrs2

                    newEqualities as [Equality] =
                        tail >> Dict.for both attrName: (attrType1 & attrType2): eqs:
                              Equality context (Why_Attribute why) attrType1 attrType2 :: eqs

                    state
                    >> addErrorIf (only1 /= Dict.empty or only2 /= Dict.empty) why "record attrs don't match"
                    >> solveEqualities newEqualities


                TypeExtra (TypeRecordExt tyvar1 attrs1) & TypeRecord _ attrs2:
                    solveRecordExt why tyvar1 attrs1 attrs2 tail state


                TypeRecord _ attrs1 & TypeExtra (TypeRecordExt tyvar2 attrs2):
                    solveRecordExt why tyvar2 attrs2 attrs1 tail state


                TypeExtra (TypeRecordExt tyvar1 attrs1) & TypeExtra (TypeRecordExt tyvar2 attr2):
                    todo "will this actually happen?"


                TypeUnique _ m1 & TypeUnique _ m2:
                    solveEqualities (Equality context why m1 m2 :: tail) state

                [#
                TypeAnnotationVariable _ name1 & TypeAnnotationVariable _ name2:
                    if name1 == name2 then
                        solveEqualities tail state
                    else
                        addError why "these tyvars should be the same!!" state
                #]

                _:
                    todo "I have no clue what to do here"




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
        remainingEqualities >> List.map (Equality context why t1 t2):
            Equality context why
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
        TypeOpaque pos usr args:
            TypeOpaque pos usr (List.map rec args)

        [#
        TypeAnnotationVariable pos name:
            originalType
        #]

        TypeFunction pos in mod out:
            TypeFunction pos (rec in) mod (rec out)

        TypeRecord pos attrs:
            TypeRecord pos (Dict.map (k: rec) attrs)

        TypeUnique pos t:
            TypeUnique pos (rec t)

        TypeExtra (TypeUnificationVariable id):
            if id == tyvarId then
                replacingType
            else
                originalType

        TypeExtra (TypeRecordExt id attrs):
            if id == tyvarId then
                replacingType
            else
                TypeExtra (TypeRecordExt id (Dict.map (k: rec) attrs))

