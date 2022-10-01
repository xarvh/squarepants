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



union Ref =
    # TODO use the same one as CanonicalAst
    Ref




alias UnificationVariableId =
    Int


union Type extra =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type extra]
    , TypeAnnotationVariable Pos Name
    , TypeFunction Pos (Type extra) LambdaModifier (Type extra)
    , TypeRecord Pos (Dict Name (Type extra))
    , TypeUnique Pos (Type extra)
    , TypeExtra extra


union CanonicalTypeExtra =
    Never CanonicalTypeExtra

alias CanonicalType =
    Type CanonicalTypeExtra


union UnificationTypeExtra =
    , TypeUnificationVariable UnificationVariableId
    , TypeRecordExt UnificationVariableId (Dict Name (Type extra))

alias UnificationType =
    Type UnificationTypeExtra






union Expression type =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos (Pattern type) LambdaModifier (Expression type)
    , Call Pos (Expression type) (Argument type) type
    , CallCo Pos (Expression type) [Argument type & type]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe (Expression type)) (Dict Name (Expression type))
    , RecordAccess Pos Name (Expression type)
    , LetIn (ValueDef type) (Expression type)
    , If Pos {
        , condition as (Expression type)
        , true as (Expression type)
        , false as (Expression type)
        }
    , Try Pos {
        , value as (Expression type)
        , type as type
        , patternsAndExpressions as [Pattern type & Expression type]
        }
    , DestroyIn Name (Expression type)


union Pattern type =
    , PatternAny Pos {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CanonicalType
        , type as type
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern type]
    , PatternRecord Pos (Dict Name (Pattern type & type))


union Argument type =
    , ArgumentExpression (Expression type)
    , ArgumentRecycle Pos Ref

alias ValueDef type = {
    , pattern as Pattern type
    , native as Bool
    , body as Expression type

    # Do we need these here?
#    , directTypeDeps as TypeDeps
#    , directConsDeps as Set Meta.UniqueSymbolReference
#    , directValueDeps as Set Meta.UniqueSymbolReference
    }




alias State = {
    , equalities as Array Equality
    , errors as Array (Pos & Context & Error_)
    , lastUnificationVarId as Int
    }


alias Env = {
    , variables as Dict Ref { type as UnificationType }
    , constructors as Dict Meta.UniqueSymbolReference { type as UnificationType }
    , annotatedTyvars as Dict Name CanonicalType
    , context as Context
    }



union Error_ =
    , ErrorVariableNotFound Ref
    , ErrorConstructorNotFound Meta.UniqueSymbolReference
    , ErrorNotCompatibleWithRecord
    , ErrorRecordDoesNotHaveAttribute Name [# TODO other attrs to give context? #]
    , ErrorRecordHasAttributesNotInAnnotation # TODO which attrs?
    , ErrorRecordIsMissingAttibutesInAnnotation # TODO which attrs?
    , ErrorTryingToAccessAttributeOfNonRecord Name UnificationType
    , ErrorIncompatibleTypes
    , ErrorVariableTypeIncompatible Ref { type as UnificationType } CanonicalType
    , ErrorConstructorTypeIncompatible Meta.UniqueSymbolReference UnificationType CanonicalType
    , ErrorCallingANonFunction
    , ErrorTooManyArguments
    , ErrorNotEnoughArguments




union Context =
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
    , Why_Duplicate
    , Why_Todo


union Equality =
    , Equality Context Why UnificationType UnificationType


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }





newTyvarId as State@: UnificationVariableId =
    state@:
    @state.lastUnificationVarId += 1
    state.lastUnificationVarId



newType as State@: UnificationType =
    state@:
    TypeExtra (TypeUnificationVariable (newTyvarId @state))


addEquality as Env: Why: UnificationType: UnificationType: State@: None =
    # TODO: should t1 be "expected" and t2 "actual" or something like that?
    env: why: t1: t2: state@:

    Array.push @state.equalities << Equality env.context why t1 t2



#
# Adds an error and, as a convenience, return a generated type
#
inferenceError as Env: Pos: Error_: State@: UnificationType =
    env: pos: error: state@:

    Array.push @state.errors (pos & env.context & error)

    newType @state



checkError as Env: Pos: Error_: State@: None =
    env: pos: error: state@:

    Array.push @state.errors (pos & env.context & error)








linearizeCurriedParameters as Type t: [LambdaModifier & Type t]: [LambdaModifier & Type t] & Type t =
    type: accum:

    try type as
        TypeFunction pos from modifier to:
            linearizeCurriedParameters to << (modifier & from) :: accum

        _:
            List.reverse accum & type



getConstructorByUsr as Meta.UniqueSymbolReference: Env: Maybe { type as UnificationType } =
    usr: env:

    Dict.get usr env.constructors


getVariableByRef as Ref: Env: Maybe { type as UnificationType } =
    ref: env:

    Dict.get ref env.variables



#
#
# Generalize
#
#
generalize as UnificationType: State@: UnificationType =
    caType: state@:

    # When I generalize, I need to add the typeclass constraints

    todo "generalize"


#
#
# Types
#
#
typeIsCompatibleWith as CanonicalType: UnificationType: Bool =
    expected: actual:

    todo "typeIsCompatibleWith"


canonicalToUnificationType as CanonicalType: UnificationType =
    ca:
    todo "canonicalToUnificationType"







#
#
# Definitions
#
#
doDefinition as Env: ValueDef CanonicalType: State@: ValueDef UnificationType & Env =
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
inferExpression as Env: Expression CanonicalType: State@: Expression UnificationType & UnificationType =
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
                        inferenceError env pos (ErrorVariableNotFound ref) @state

                    Just var:
                        generalize var.type @state

            Variable pos ref & ty


        Constructor pos usr:
            ty =
                try getConstructorByUsr usr env as
                    Nothing:
                        inferenceError env pos (ErrorConstructorNotFound usr) @state

                    Just cons:
                        generalize cons.type @state

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

            typedValueByName as Dict Name (Expression UnificationType) =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name UnificationType =
                Dict.map (k: Tuple.second) typedValueAndValueTypeByName

            Record pos Nothing typedValueByName & TypeRecord pos valueTypeByName


        Record pos (Just ext) attrExpressions:

            typedValueAndValueTypeByName as Dict Name (Expression UnificationType & UnificationType) =
                attrExpressions >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName as Dict Name (Expression UnificationType) =
                Dict.map (k: Tuple.first) typedValueAndValueTypeByName

            valueTypeByName as Dict Name UnificationType =
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


inferRecordAccess as Env: Pos: Name: UnificationType: State@: UnificationType =
    env: pos: attrName: inferredType: state@:

    try inferredType as
        TypeRecord _ attrTypes:
            try Dict.get attrName attrTypes as
                Just type:
                    type

                Nothing:
                    inferenceError env pos (ErrorRecordDoesNotHaveAttribute attrName) @state

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

            type as UnificationType =
                TypeExtra << TypeRecordExt newExtId (Dict.singleton attrName newAttrType)

            addEquality env Why_RecordAccess (TypeExtra << TypeUnificationVariable id) type @state

            newAttrType

        _:
            inferenceError env pos (ErrorTryingToAccessAttributeOfNonRecord attrName inferredType) @state



inferRecordExtended as Env: Pos: UnificationType: Dict Name UnificationType: State@: UnificationType =
    env: pos: extType: valueTypeByName: state@:

    try extType as
        TypeRecord _ attrTypes:

            Dict.each valueTypeByName name: valueType:
                try Dict.get name attrTypes as

                    Nothing:
                        checkError env pos (ErrorRecordDoesNotHaveAttribute name) @state

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
            inferenceError env pos ErrorNotCompatibleWithRecord @state



checkExpression as Env: CanonicalType: Expression CanonicalType: State@: Expression UnificationType =
    env: expectedType: caExpression: state@:

    try caExpression & expectedType as

        LiteralNumber pos n & TypeOpaque _ typeUsr []:
            if typeUsr /= CoreTypes.numberDef.usr then
                checkError env pos ErrorIncompatibleTypes @state
            else
                None

            LiteralNumber pos n


        LiteralText pos text & TypeOpaque _ typeUsr []:
            if typeUsr /= CoreTypes.textDef.usr then
                checkError env pos ErrorIncompatibleTypes @state
            else
                None

            LiteralText pos text


        Variable pos ref & _:
            try getVariableByRef ref env as
                Nothing:
                    checkError env pos (ErrorVariableNotFound ref) @state

                Just var:
                    if typeIsCompatibleWith expectedType var.type then
                        None
                    else
                        checkError env pos (ErrorVariableTypeIncompatible ref var expectedType) @state

            Variable pos ref


        Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    checkError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    if typeIsCompatibleWith expectedType cons.type then
                        None
                    else
                        checkError env pos (ErrorConstructorTypeIncompatible usr cons.type expectedType) @state

            Constructor pos usr


        Lambda pos pattern lambdaModifier body & TypeFunction _ in mod out:
            typedPattern & localEnv =
                checkPattern env in pattern @state

            typedBody =
                checkExpression localEnv out body @state

            todo "checkModifiers lambdaModifier mod @state"

            Lambda pos typedPattern mod typedBody


        Call pos reference argument unusedType & _:
            checkCallCo env (canonicalToUnificationType expectedType) pos reference [argument & unusedType] @state


        CallCo pos reference argsAndUnusedTypes & _:
            checkCallCo env (canonicalToUnificationType expectedType) pos reference argsAndUnusedTypes @state


        Record pos (Just ext) valueByName & TypeRecord _ typeByName:
#            all values must exist in typeByName
#            ext must have type expected type
            todo "Record"



        Record pos Nothing valueByName & TypeRecord _ typeByName:
            aOnly & both & bOnly =
                onlyBothOnly valueByName typeByName

            if aOnly /= Dict.empty then
                checkError env pos ErrorRecordHasAttributesNotInAnnotation @state

            else if bOnly /= Dict.empty then
                checkError env pos ErrorRecordIsMissingAttibutesInAnnotation @state

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
            checkError env (todo "CA.expressionPos caExpression") ErrorIncompatibleTypes @state
            LiteralText Pos.G "todo?"




checkCallCo as Env: UnificationType: Pos: Expression CanonicalType: [Argument CanonicalType & unusedType]: State@: Expression UnificationType =
    env: expectedType: pos: reference: givenArgs: state@:

    typedReference & referenceType =
        inferExpression env reference @state

    typedArgumentsAndArgumentTypes as [Argument UnificationType & UnificationType] =
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
            checkError env pos ErrorNotEnoughArguments @state

        else if rl < gl then
            checkError env pos ErrorTooManyArguments @state

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
                checkError env pos ErrorCallingANonFunction @state

        referenceTypeFromArguments =
            expectedType
            #>> canonicalToUnificationType
            >> List.forReversed typedArgumentsAndArgumentTypes (tyArg & argTy): type:
                TypeFunction Pos.G argTy (todo "modifier") type

        addEquality env Why_CalledAsFunction referenceType referenceTypeFromArguments @state

#        typedArgs as [Argument UnificationType & UnificationType] =
#            List.map Tuple.first typedArgumentsAndArgumentTypes

        CallCo pos typedReference typedArgumentsAndArgumentTypes



inferArgument as Env: Argument CanonicalType: State@: Argument UnificationType & UnificationType =
    env: arg: state@:

    try arg as
        ArgumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            ArgumentExpression typedExp & expType

        ArgumentRecycle pos ref:
            type =
                try getVariableByRef ref env as

                    Nothing:
                        inferenceError env pos (ErrorVariableNotFound ref) @state

                    Just var:
                        var.type

            ArgumentRecycle pos ref & type


#
#
# Patterns
#
#
alias PatternOut = {
    , patternType as UnificationType
    , typedPattern as Pattern UnificationType
    , maybeFullAnnotation as Maybe CanonicalType
    , env as Env
    }


inferPattern as Env: Pattern CanonicalType: State@: PatternOut =
    env: pattern: state@:

    try pattern as
        PatternAny pos { isUnique, maybeName, maybeAnnotation, type = __unused__ }:

            patternType =
                try maybeAnnotation as
                    Nothing:
                        if isUnique then
                            TypeUnique << newType @state
                        else
                            newType @state

                    Just annotation:
                        ensure annotation matches isUnique
                        canonicalToUnificationType annotation


            newEnv =
                try maybeName as

                    Nothing:
                        env

                    Just name:

                        if Dict.member name env.variables then
                            addError (Why_Duplicate name) "shadowing!" @state
                        else
                            None

                        { env with variables = Dict.insert name type .variables }

            typedPattern =
                PatternAny pos { isUnique, maybeName, maybeAnnotation, type }

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
                        inferenceError env pos (ErrorVariableNotFound usr) @state

                    Just cons:
                        argModAndTypes & returnType =
                            linearizeCurriedParameters (generalize cons.type @state) []

                        rl =
                            List.length referenceArgs

                        gl =
                            List.length givenArgs

                        if rl > gl then
                            addError "not enough arguments" @state
                        else
                            None

                        if rl < gl then
                            addError "too many arguments" @state
                        else
                            None

                        list_eachWithIndex2 argModAndTypes argumentTypes index: (mod & paramType): argType:
                            addEquality env (Why_Argument index) paramType argType @state

                        ##    { x } = blah
                        ##
                        ## `blah` /could/ be unique, but in this case we'll just assume it is NOT

                        if todo "there is any unique" then
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



checkPattern as Env: CanonicalType: Pattern CanonicalType: State@: Pattern UnificationType & Env =
    env: expectedType: pattern: state@:

    # TODO
    out = inferPattern env pattern @state

    addEquality env Why_Todo out.patternType (canonicalToUnificationType expectedType) @state

    out.typedPattern & out.env



[#
#
#
# Equalities resolution
#
#
alias State = {
    , substitutions as Dict UnificationVariableId UnificationType
    , errors as [Why & Text]
    }


solveEqualities as [Equality]: State: State =
    remainingEqualities: state:

    try remainingEqualities as
        []:
            state

        Equality why type1 type2 :: tail:
            try type1 & type2 as

                TypeExtra (TypeUnificationVariable tyvarId) & t2:
                    replaceUnificationVariable tyvarId t2 tail state

                t1 & TypeExtra (TypeUnificationVariable tyvarId):
                    replaceUnificationVariable tyvarId t1 tail state

                TypeOpaque _ usr1 args1 & TypeOpaque _ usr2 args2:
                    if usr1 /= usr2 then
                        addError why "types don't match" state
                    else
                        newEqualities =
                            List.indexMap2 (index: a1: a2: Equality (WhyTypeArgument usr1 index why)) args1 args2

                        solveEqualities (List.append tail newEqualities) state

                TypeFunction _ in1 modifier1 out1 & TypeFunction _ in2 modifier2 out2:
                    if modifier1 /= modifier2 then
                        state >> addError why "lambda modifiers don't match"
                    else
                        newEqualities =
                            Equality (WhyFunctionInput why) in1 in2 :: Equality (WhyFunctionOutput why) out1 out2 :: tail

                        solveEqualities newEqualities state

                TypeRecord _ attrs1 & t2:
                    solveRecord why attrs1 t2 tail state

                t1 & TypeRecord _ attrs2:
                    solveRecord why attrs2 t1 tail state

                TypeRecordExt _ name1 attrs1 & t2:
                    solveRecordExt why name1 attrs1 t2 tail state

                t1 & TypeRecordExt _ name2 attrs2:
                    solveRecordExt why name2 attrs2 t1 tail state

                TypeUnique _ m1 & TypeUnique _ m2:
                    solveEqualities (Equality why m1 m2 :: tail) state

                TypeAnnotationVariable _ name1 & TypeAnnotationVariable _ name2:
                    if name1 == name2 then
                        solveEqualities tail state
                    else
                        addError why "these tyvars should be the same!!" state


solveRecord as Why: Dict Name UnificationType: UnificationType: [Equality]: State: State =
    why: attrs1: type2: remainingEqualities:

    try type2 as
        TypeRecord _ attrs2:
            ...

        TypeRecordExt _ name2 attrs2:
            ...

        _:
            addError why "should be a record" state


solveRecordExt as Why: ...: Dict Name UnificationType: UnificationType: [Equality]: State: State =
    why: attrs1: type2: remainingEqualities:

    try type2 as
        TypeRecord _ attrs2:
            ...

        TypeRecordExt _ name2 attrs2:
            ...

        _:
            addError why "should be a record" state


replaceUnificationVariable as UnificationVariableId: UnificationType: [Equality]: State: State =
    uniVarId: replacingType: remainingEqualities: state:

    #TODO: check that replacingType does not contain uniVarId

    equalities =
        # TODO we don't care about map preserving order
        remainingEqualities >> List.map (Equality why t1 t2):
            Equality why
                (applySubstitutionToType uniVarId replacingType t1)
                (applySubstitutionToType uniVarId replacingType t2)

    substitutions =
        state.substitutions
        >> Dict.map (tyvarId: type: applySubstitutionToType univarId replacingType type)
        >> Dict.insert uniVarId replacingType

    solveEqualities newEqualities { state with substitutions }


applySubstitutionToType as UnificationVariableId: UnificationType: UnificationType: UnificationType =
    uniVarId: replacingType: originalType:

    rec =
        applySubstitutionToType uniVarId replacingType

    try originalType as
        TypeOpaque pos usr args:
            TypeOpaque pos usr (List.map rec args)

        TypeAnnotationVariable pos name:
            originalType

        TypeFunction pos in mod out:
            TypeFunction pos (rec in) mod (rec out)

        TypeRecord pos attrs:
            TypeRecord pos (Dict.map (k: rec) attrs)

        TypeRecordExt pos id attrs:
            if id == univarId then
                replacingType
            else
                TypeRecordExt pos id (Dict.map (k: rec) attrs)

        TypeUnique pos t:
            TypeUnique pos (rec t)

        TypeExtra (TypeUnificationVariable id):
            if id == univarId then
                replacingType
            else
                originalType

#]
