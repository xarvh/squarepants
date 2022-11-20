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


initState as Int: State =
    lastUnificationVarId:
    {
    , equalities = Array.fromList []
    , errors = Array.fromList []
    , lastUnificationVarId
    , classesByTyvarId = Hash.empty
    }


alias TypeWithClasses = {
    , type as TA.Type
    , tyvars as Dict TA.UnificationVariableId TA.TypeClasses
    }


union NamedType =
    , OpaqueType [At Name]
    , UnionType CA.UnionDef
    , AliasType CA.AliasDef


alias Env = {
    , context as Context
    , constructors as ByUsr TypeWithClasses
    , variables as Dict TA.Ref TypeWithClasses
    , namedTypes as ByUsr NamedType

    , tyvarsInParentAnnotations as Dict TA.UnificationVariableId TA.Type

    # This is used to give meaningfule errors?
    , annotatedTyvarToGeneratedTyvar as Dict Name TA.UnificationVariableId
    }


initEnv as Env =
    {
    , context = Context_Global
    , constructors = Dict.empty
    , variables = Dict.empty
    , namedTypes = Dict.empty
    , tyvarsInParentAnnotations = Dict.empty
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
    , ErrorIncompatibleTypes
    , ErrorVariableTypeIncompatible CA.Ref TypeWithClasses CA.Type
    , ErrorConstructorTypeIncompatible USR TA.Type CA.Type
    , ErrorCallingANonFunction
    , ErrorTooManyArguments
    , ErrorNotEnoughArguments
    , ErrorIncompatibleRecycling
    , ErrorUniquenessDoesNotMatch
    , ErrorUndefinedTypeVariable Name
    , ErrorWrongNumberOfTypeArguments USR [At Name] [CA.Type]
    , ErrorNamedTypeNotFound USR
    , ErrorCircularAlias USR


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


#linearizeCurriedParameters as TA.Type: [LambdaModifier & TA.Type]: [LambdaModifier & TA.Type] & TA.Type =
#    type: accum:
#
#    try type as
#        TA.TypeFn pos from modifier to:
#            linearizeCurriedParameters to << (modifier & from) :: accum
#
#        _:
#            List.reverse accum & type


getConstructorByUsr as USR: Env: Maybe TypeWithClasses =
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

                    TA.TypeUnificationVariable tyvarId

        applySubstitutionToType tyvarId replacementType


#
#
# Types
#
#
variableIsCompatibleWith as Env: CA.Type: TA.Type: Bool =
    # This function is used to ensure that when a variable (or a constructor)
    # from env can be used with the annotated/expected type
    env: expected: variableType:

    try expected & variableType as
        CA.TypeNamed _ usrC argsC & TA.TypeOpaque _ usrU argsU:
            if usrC /= usrU then
                False
            else
                # TODO this is not efficient, we should stop at the first
                List.map2 (variableIsCompatibleWith env) argsC argsU
                >> List.all identity

        CA.TypeFn _ modsAndArgsC outC & TA.TypeFn _ modsAndArgsU outU:
            # TODO clean this up
            x =
                List.map2 Tuple.pair modsAndArgsC modsAndArgsU
                >> List.all ((modC & inC) & (modU & inU)):
                    modC /= modU and variableIsCompatibleWith env inC inU

            x and variableIsCompatibleWith env outC outU

        CA.TypeRecord _ attrC & TA.TypeRecord _ attrU:
            onlyC & both & onlyU =
                onlyBothOnly attrC attrU

            if onlyC /= Dict.empty or onlyU /= Dict.empty then
                False
            else
                both
                >> Dict.values
                >> List.all (c & u): variableIsCompatibleWith env c u

        CA.TypeUnique _ c & TA.TypeUnique _ u:
            variableIsCompatibleWith env c u

        CA.TypeAnnotationVariable _ nameC & TA.TypeUnificationVariable idU:
            Dict.get nameC env.annotatedTyvarToGeneratedTyvar == Just idU

        #TypeRecord _ attrsC & TypeExtra (TypeRecordExt idU attrsU):
        #    ????

        # TODO Probably needs to manage more TypeUnificationVariable?

        _:
            False


typeCa2Ta as Env: CA.Type: TA.Type =
    env: ca:

    f as Name: TA.UnificationVariableId =
        name:
        try Dict.get name env.annotatedTyvarToGeneratedTyvar as
            Just tyvarId: tyvarId
            Nothing: todo << "compiler error: typeCa2Ta: name `" .. name .. "` not found"

    typeCa2Ta_ f ca


typeCa2Ta_ as (Name: TA.UnificationVariableId): CA.Type: TA.Type =
    f: ca:

    ctu = typeCa2Ta_ f

    try ca as
       CA.TypeNamed p usr pars:
           TA.TypeOpaque p usr (List.map ctu pars)

       CA.TypeFn p modsAndArgs out:
           xxx = List.map (Tuple.mapSecond ctu) modsAndArgs
           TA.TypeFn p xxx (ctu out)

       CA.TypeRecord p attrs:
           TA.TypeRecord p (Dict.map (k: ctu) attrs)

       CA.TypeUnique p ty:
           TA.TypeUnique p (ctu ty)

       CA.TypeAnnotationVariable _ name:
            TA.TypeUnificationVariable (f name)


variableOfThisTypeMustBeFlaggedUnique as TA.Type: Bool =
    ca:

    try ca as
       TA.TypeUnique p ty:
         True

       TA.TypeOpaque p usr pars:
          # Opaques cannot contain uniques?
          List.any variableOfThisTypeMustBeFlaggedUnique pars

       TA.TypeFn p args out:
          False

       TA.TypeRecord p attrs:
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TA.TypeRecordExt tyvarId attrs:
          Dict.any (k: variableOfThisTypeMustBeFlaggedUnique) attrs

       TA.TypeUnificationVariable _:
          # At worst a tyvar can be ImmutableOrUnique, in which case it still doesn't need to be flagged
          False



expandType as Env: State@: CA.Type: CA.Type =
    env: state@: type:

    try type as
        CA.TypeNamed pos usr args:

            checkArgsLengthThen as [At Name]: (a: CA.Type): CA.Type =
                pars: continue:

                if List.length args /= List.length pars then
                    addError env pos (ErrorWrongNumberOfTypeArguments usr pars args) @state
                    CA.TypeError pos
                else
                    continue None

            try Dict.get usr env.namedTypes as
                Nothing:
                    addError env pos (ErrorNamedTypeNotFound usr) @state
                    CA.TypeError pos

                Just (OpaqueType pars):
                    checkArgsLengthThen pars _:
                    type

                Just (UnionType unionDef):
                    checkArgsLengthThen unionDef.pars _:
                    type

                Just (AliasType aliasDef):
                    checkArgsLengthThen aliasDef.pars _:

                    typeByParName as Dict Name CA.Type =
                        List.map2 ((At pos name): p: name & p) aliasDef.pars args
                        >> Dict.fromList

                    replaceTyvarsInCaType env typeByParName @state aliasDef.usr aliasDef.type

        _:
            type


replaceTyvarsInCaType as Env: Dict Name CA.Type: State@: USR: CA.Type: CA.Type =
    env: dict: state@: aliasUsr: ty:

    rec as CA.Type: CA.Type =
        replaceTyvarsInCaType env dict @state aliasUsr

    try ty as

        CA.TypeNamed pos usr pars:
            if usr == aliasUsr then
                addError env pos (ErrorCircularAlias usr) @state
                CA.TypeError pos
            else
                CA.TypeNamed pos usr (List.map rec pars)

        CA.TypeFn pos fnPars fnOut:
            CA.TypeFn pos (List.map (Tuple.mapSecond rec) fnPars) (rec fnOut)

        CA.TypeRecord pos attrs:
            CA.TypeRecord pos (Dict.map (k: rec) attrs)

        CA.TypeUnique pos type:
            # To remove this, we can "just" not replace all the constructors args?
            CA.TypeUnique pos (rec type)

        CA.TypeAnnotationVariable pos name:
            try Dict.get name dict as
                Nothing:
                    addError env pos (ErrorUndefinedTypeVariable name) @state
                    CA.TypeError pos

                Just expanded:
                    expanded



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
                # TODO Should I use patternOut.type or (typeCa2Ta annotationType)?
                checkExpression newEnv annotationType def.body @state & patternOut.patternType
            Nothing:
                inferExpression newEnv def.body @state

    if patternOut.maybeFullAnnotation == Nothing then
        addEquality newEnv (CA.patternPos def.pattern) Why_LetIn patternOut.patternType bodyType @state
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
        CA.LiteralNumber pos n:
            TA.LiteralNumber pos n & (typeCa2Ta env CoreTypes.number)


        CA.LiteralText pos text:
            TA.LiteralText pos text & (typeCa2Ta env CoreTypes.text)


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
                        inferPattern env pa @state

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
                inferPattern env pa @state

            TA.ParameterPattern patternOut.typedPattern & patternOut.patternType & patternOut.env

        CA.ParameterRecycle pos name:

            # TODO check name already in env?

            type = TA.TypeUnique pos (newType @state)

            typeWithClasses as TypeWithClasses =
                {
                , type
                , tyvars = Dict.empty
                }

            newEnv as Env =
                { env with variables = Dict.insert (CA.RefLocal name) typeWithClasses .variables }

            TA.ParameterRecycle pos name & type & newEnv




inferFn as Env: Pos: [CA.Parameter]: CA.Expression: State@: TA.Expression & TA.Type =
    env: pos: caPars: body: state@:

    typedPars @= Array.fromList []
    parTypes @= Array.fromList []
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
        TA.TypeFn Pos.G (Array.toList parTypes) bodyType

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
            ty = TA.TypeRecordExt id valueTypeByName

            addEquality env pos Why_RecordExt extType ty @state

            ty

        _:
            addError env pos ErrorNotCompatibleWithRecord @state
            newType @state


checkExpression as Env: CA.Type: CA.Expression: State@: TA.Expression =
    env: rawExpectedType: caExpression: state@:

    expectedType as CA.Type =
        expandType env @state rawExpectedType

    try caExpression & expectedType as

        CA.LiteralNumber pos n & CA.TypeNamed _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                addError env pos ErrorIncompatibleTypes @state
            else
                None

            TA.LiteralNumber pos n


        CA.LiteralText pos text & CA.TypeNamed _ typeUsr []:
            if typeUsr /= CoreTypes.textDef.usr then
                addError env pos ErrorIncompatibleTypes @state
            else
                None

            TA.LiteralText pos text


        CA.Variable pos ref & _:
            try getVariableByRef ref env as
                Nothing:
                    addError env pos (ErrorVariableNotFound ref) @state

                Just var:
                    if variableIsCompatibleWith env expectedType var.type then
                        None
                    else
                        addError env pos (ErrorVariableTypeIncompatible ref var expectedType) @state

            TA.Variable pos ref


        CA.Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    addError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    if variableIsCompatibleWith env expectedType cons.type then
                        None
                    else
                        addError env pos (ErrorConstructorTypeIncompatible usr cons.type expectedType) @state

            TA.Constructor pos usr


        CA.Fn pos pars body & CA.TypeFn _ parsT out:
            todo "CA.Fn CA.TypeFn"
#            typedPattern & localEnv =
#                checkPattern env in pattern @state
#
#            typedBody =
#                checkExpression localEnv out body @state
#
#            if lambdaModifier == mod then
#                None
#            else
#                addError env pos ErrorIncompatibleLambdaModifier @state
#
#            TA.Lambda pos typedPattern mod typedBody


        CA.Call pos reference args & _:
            checkCallCo env (typeCa2Ta env expectedType) pos reference args @state


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
                >> typeCa2Ta env
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
                        inferPattern env pa @state

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


        _:
            addError env (todo "CA.expressionPos caExpression") ErrorIncompatibleTypes @state
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

            TA.ArgumentRecycle pos attrPath ref & type


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


inferPattern as Env: CA.Pattern: State@: PatternOut =
    env: pattern: state@:

    try pattern as
        CA.PatternAny pos { isUnique, maybeName, maybeAnnotation }:

            patternType as TA.Type =
                try maybeAnnotation as
                    Nothing:
                        if isUnique then
                            TA.TypeUnique pos << newType @state
                        else
                            newType @state

                    Just annotation:
                        t =
                            typeCa2Ta env annotation

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
                        tyvars =
                             patternType
                             >> TA.typeTyvars
                             >> Dict.map k: v: { allowFunctions = Nothing, allowUniques = Nothing }

                        variable =
                            {
                            , type = patternType
                            , tyvars
                            }

                        # We don't check for duplicate var names / shadowig here, it's MakeCanonical's responsibility
                        { env with variables = Dict.insert (CA.RefLocal name) variable .variables }

            typedPattern =
                TA.PatternAny pos { isUnique, maybeName, maybeAnnotation, type = patternType }

            {
            , typedPattern
            , patternType
            , env = newEnv
            , maybeFullAnnotation = maybeAnnotation
            }


        CA.PatternLiteralText pos text:
            {
            , typedPattern = TA.PatternLiteralText pos text
            , patternType = typeCa2Ta env CoreTypes.text
            , env
            , maybeFullAnnotation = Just CoreTypes.text
            }


        CA.PatternLiteralNumber pos n:
            {
            , typedPattern = TA.PatternLiteralNumber pos n
            , patternType = typeCa2Ta env CoreTypes.number
            , env
            , maybeFullAnnotation = Just CoreTypes.number
            }


        CA.PatternConstructor pos usr arguments:

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
                        inferPattern envX pa @state

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



checkPattern as Env: CA.Type: CA.Pattern: State@: TA.Pattern & Env =
    env: expectedType: pattern: state@:

    # TODO
    out = inferPattern env pattern @state

    addEquality env (CA.patternPos pattern) Why_Todo out.patternType (typeCa2Ta env expectedType) @state

    out.typedPattern & out.env


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

    erStateF =
        solveEqualities (Array.toList state.equalities) erState0


    #
    # packaging & closing
    #
    typedModule as TA.Module =
        {
        , umr = caModule.umr
        , asText = caModule.asText
        , valueDefs = typedValueDefs
        , substitutions = erStateF.substitutions
        }

    errors as [Error] =
        [
        , state.errors
          >> Array.toList
          >> List.map (makeInferenceAndCheckError env caModule)
        , erStateF.errors
          >> List.map (makeResolutionError env caModule)
        ]
        >> List.concat

    Debug.benchStop "type check"

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
nameToTyvarId as State@: (Hash Name TA.UnificationVariableId)@: Name: TA.UnificationVariableId =
    state@: hash@: varname:
    try Hash.get hash varname as
        Just tyvarId: tyvarId
        Nothing:
            tyvarId = newTyvarId @state
            Hash.insert @hash varname tyvarId
            tyvarId


translateTyvars as State@: (Hash Name TA.UnificationVariableId)@: Dict Name CA.TypeClasses: Dict TA.UnificationVariableId TA.TypeClasses =
    state@: hash@: classesByName:

    zot =
       name: classes:
        Dict.insert (nameToTyvarId @state @hash name) classes

    Dict.empty >> Dict.for classesByName zot


addValueToGlobalEnv as State@: UMR: CA.ValueDef: Env: Env =
    state@: umr: def: env:

    hash @=
        Hash.empty

    env >> Dict.for (CA.patternNames def.pattern) valueName: valueStuff: envX:
        try valueStuff.maybeAnnotation as
            Nothing:
                envX

            Just annotation:

                ref as TA.Ref =
                    valueName
                    >> USR umr
                    >> CA.RefGlobal

                typeWithClasses as TypeWithClasses =
                    {
                    , type = typeCa2Ta_ (nameToTyvarId @state @hash) annotation
                    , tyvars = translateTyvars @state @hash def.tyvars
                    }

                { envX with variables = Dict.insert ref typeWithClasses .variables }


addConstructorToGlobalEnv as State@: [At Name]: Name: CA.Constructor: Env: Env =
    state@: args: name: caConstructor: env:

    USR umr _ =
        caConstructor.typeUsr

    hash @=
        Hash.empty

    caTyvars as Dict Name CA.TypeClasses =
        args
        >> List.map ((At p n): (n & { allowFunctions = Just True, allowUniques = Just True }))
        >> Dict.fromList

    taConstructor as TypeWithClasses =
        {
        , type = typeCa2Ta_ (nameToTyvarId @state @hash) caConstructor.type
        , tyvars = translateTyvars @state @hash caTyvars
        }

    { env with constructors = Dict.insert (USR umr name) taConstructor .constructors }


addUnionTypeAndConstructorsToGlobalEnv as State@: a: CA.UnionDef: Env: Env =
    state@: _: stuff: env:

    { usr, pars, constructors, directTypeDeps } =
        stuff

    taDef as TA.UnionDef =
        {
        , usr
        , pars
        #, constructors = taConstructors
        }

    #TODO{ env with types = Dict.insert usr (TA.TypeDefUnion taDef) .types }
    env
    >> Dict.for constructors (addConstructorToGlobalEnv @state pars)


initStateAndGlobalEnv as [CA.Module]: State & Compiler/TypeCheck.Env =
    allModules:

    state @=
        initState 0

    doStuff as CA.Module: Env: Env =
        caModule: env:
        env
        >> Dict.for caModule.unionDefs (addUnionTypeAndConstructorsToGlobalEnv @state)
        #TODO >> Dict.for caModule.aliasDefs (addAliasToGlobalEnv @state)
        >> Dict.for caModule.valueDefs (pattern: addValueToGlobalEnv @state caModule.umr)

    initEnv
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


addErrorIf as Bool: Equality: Text: ERState: ERState =
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


                TA.TypeOpaque _ usr1 args1 & TA.TypeOpaque _ usr2 args2:
                    newEqualities as [Equality] =
                        List.indexedMap2 (index: Equality context pos (Why_TypeArgument usr1 index why)) args1 args2

                    state
                    >> addErrorIf (usr1 /= usr2) head "different opaque types"
                    >> solveEqualities (List.append tail newEqualities)


                TA.TypeFn _ pars1 out1 & TA.TypeFn _ pars2 out2:
                    if List.length pars1 /= List.length pars2 then
                        state
                        >> addErrorIf True head "functions expect a different number of arguments"

                    else
                        both =
                            List.map2 Tuple.pair pars1 pars2

                        inEqualities as [Equality] =
                            both >> List.indexedMap index: ((m1 & in1) & (m2 & in2)):
                                Equality context pos (Why_FunctionInput index why) in1 in2

                        outEquality as Equality =
                            Equality context pos (Why_FunctionOutput why) out1 out2

                        state
                        >> List.for both (((m1 & _) & (m2 & _)): addErrorIf (m1 /= m2) head "argument modifiers don't match")
                        >> solveEqualities (List.concat [inEqualities, [outEquality], tail])


                TA.TypeRecord _ attrs1 & TA.TypeRecord _ attrs2:
                    only1 & both & only2 =
                        onlyBothOnly attrs1 attrs2

                    newEqualities as [Equality] =
                        tail >> Dict.for both attrName: (attrType1 & attrType2): eqs:
                              Equality context pos (Why_Attribute why) attrType1 attrType2 :: eqs

                    state
                    >> addErrorIf (only1 /= Dict.empty or only2 /= Dict.empty) head "record attrs don't match"
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
                    { state with errors = (head & "types are incompatible") :: .errors }
                    >> solveEqualities tail




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
        TA.TypeOpaque pos usr pars:
            TA.TypeOpaque pos usr (List.map rec pars)

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

