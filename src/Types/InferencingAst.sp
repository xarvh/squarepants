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
    , TypeRecordExt Pos UnificationVariableId (Dict Name (Type extra))

alias UnificationType =
    Type UnificationTypeExtra







union Expression type =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos (Pattern type) LambdaModifier Expression
    , Call Pos Expression Argument type
    , CallCo Pos Expression [Argument & type]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe (Expression type)) (Dict Name Expression)
    , RecordAccess Pos Name (Expression type)
    , LetIn ValueDef Expression
    , If Pos {
        , condition as Expression
        , true as Expression
        , false as Expression
        }
    , Try Pos {
        , value as Expression
        , type as type
        , patternsAndExpressions as [Pattern type & Expression type]
        }
    , DestroyIn Name Expression


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
    , PatternRecord Pos (Dict Name (Pattern & type))


union Argument type =
    , ArgumentExpression (Expression type)
    , ArgumentRecycle Pos Ref

alias ValueDef type = {
    , pattern as (Pattern type)
    , native as Bool
    , body as Expression type

    # Do we need these here?
    , directTypeDeps as TypeDeps
    , directConsDeps as Set Meta.UniqueSymbolReference
    , directValueDeps as Set Meta.UniqueSymbolReference
    }




alias State = {
    , equalities as [Equalities]
    , errors as [Error]
    , nextUnificationVarId as Int
    }


alias Env = {
    , variables as Dict Name UnificationType
    , annotatedTyvars as Dict Name Type
    , context as ???
    }



union Why =
  ???


union Equality =
    , Equality Why UnificationType UnificationType


#union Constraint =
#    , C_Nest [Constraint]
#    , C_Class Why UnificationType TypeClasses
#    , C_Equality Why UnificationType UnificationType
    # TODO I don't know how to use this one
    #, C_Implication Name Constraint Constraint


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }




















linearizeCurriedParameters as Type: [LambdaModifier & Type]: [LambdaModifier & Type] & Type =
    type: accum:

    try type as
        CA.TypeFunction pos from modifier to:
            linearizeCurriedParameters to << (modifier & from) :: accum

        _:
            List.reverse accum & type


#
#
# Expressions
#
#
inferExpression as Env: Expression CanonicalType: State@: Expression UnificationType & UnificationType =
    env: caExpression: @state:

    try caExpression as

        # TODO would be nice to be able to say that from here on, `caExpression as Expression any`
        LiteralNumber pos n:
            LiteralNumber pos n & CoreTypes.numberType


        LiteralText pos text:
            LiteralText pos text & CoreTypes.textType


        Variable pos ref:
            try getVariableByRef ref env as
                Nothing:
                    inferenceError env (ErrorVariableNotFound ref pos) @state

                Just var:
                    Variable pos ref & generalize var.type @state


        Constructor pos usr:
            try getConstructorByUsr usr env as
                Nothing:
                    inferenceError env (ErrorConstructorNotFound usr pos) @state

                Just cons:
                    Constructor pos usr & generalize cons.type @state


        Lambda pos pattern modifier body:

            inferredPattern =
                inferPattern env pattern @state

            newEnv =
                { inferredPattern.env with
                , context = LambdaBody pos .context
                }

            unificationBody & bodyType =
                inferExpression newEnv body @state

            type =
                TypeFunction Pos.G
                    inferredPattern.type
                    modifier
                    expressionType

            exp =
                Lambda pos inferredPattern.unificationPattern modifier unificationBody

            exp & type


        Call pos reference argument __unused__:

            callType =
                newType @state

            checkCallCo env callType pos reference [argument & __unused__] @state


        CallCo pos reference argsAndUnusedTypes:

            callType =
                newType @state

            checkCallCo env callType pos reference argsAndUnusedTypes @state


        Record pos Nothing attrs:

            typedValueAndValueTypeByName =
                attrs >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName =
                Dict.map Tuple.first typedValueAndValueTypeByName

            valueTypeByName =
                Dict.map Tuple.second typedValueAndValueTypeByName


            Record pos Nothing typedValueByName & TypeRecord pos valueTypeByName


        Record pos (Just ext) attrExpressions:

            typedValueAndValueTypeByName =
                attrs >> Dict.map name: value:
                    inferExpression { env with context = Context_Argument name .context } value @state

            typedValueByName =
                Dict.map Tuple.first typedValueAndValueTypeByName

            valueTypeByName =
                Dict.map Tuple.second typedValueAndValueTypeByName


            typedExt & extType =
                inferExpression env ext @state


            finalType =
                try extType as
                    TypeRecord _ attrTypes:

                        Dict.each valueTypeByName name: valueType:
                            try Dict.get name attrTypes as

                                Nothing:
                                    checkError "missing attribute" name @state

                                Just ty:
                                    addEquality env Why_Record ty valueType @state

                        extType


                    TypeExtra (TypeRecordExt _ tyvarId extensionAttrTypes):

                        expressionOnly & both & extensionOnly =
                            onlyA_both_onlyB inferredAttrTypes extensionAttrTypes

                        Dict.each both name: (inAttr & extAttr):
                            addEquality env Why_Record inAttr extAttr @state


                        # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
                        newTyvarId =
                            newTyvarId @state

                        TypeRecordExt _ newTyvarId (Dict.join inferredAttrTypes extensionOnly)


                    TypeUnificationVariable id:
                        ty = TypeExtra (TypeRecordExt _ id valueTypeByName)

                        addEquality env Why_RecordExt extType ty @state

                        ty

                    _:
                        not compatible with record

            Record pos (Just typedExt) typedValueByName & finalType


        RecordAccess pos attrName recordExpression:

            unificationExpr & inferredType =
                inferExpression env recordExpression @state

            try inferredType as
                TypeRecord _ attrTypes:
                    check that attrName exists in attrTypes, return relevant type

                TypeRecordExt _ tyvarId extensionAttrTypes:
                    check that attrName exists in attrTypes, return relevant type

                    otherwise
                        create newExtTyvar
                        create newAttrType

                        type = TypeRecordExt newExtTyvar (Dict.insert attrName newAttrType extensionAttrTypes)
                        replaceUnificationVariable tyvarId type

                TypeUnificationVariable id:
                    create newExtTyvar
                    create newAttrType

                    type = TypeRecordExt newExtTyvar (Dict.singleton attrName newAttrType extensionAttrTypes)
                    replaceUnificationVariable tyvarId type

                _:
                    addError "not a record!" @state


        LetIn { pattern, body } rest:

            inferredPattern =
                inferPattern env @state

            newEnv =
                { inferredPattern.env with
                , context = LetInBody pos .context
                }

            unificationBody & bodyType =
                inferExpression newEnv body @state


        If pos { condition, true, false }:

            typedCondition =
                checkExpression { env with context = IfCondition pos } CoreTypes.bool condition @state

            typedTrue & trueType =
                inferExpression { env with context = IfTrue } true @state

            typedFalse & falseType =
                inferExpression { env with context = IfFalse } false @state

            addEquality env Why_IfBranches trueType falseType @state

            expression =
                If pos {
                    , condition = typedCondition
                    , true = typedTrue
                    , false = typedFalse
                    }

            expression & trueType


        Try pos { value, type = __unused __, patternsAndExpressions }:

            typedValue & valueType =
                inferExpression env value @state

            finalType =
                newType @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    inferredPattern =
                        inferPattern env pa @state

                    addEquality env Why_TryPattern inferredPattern.type valueType @state

                    newEnv =
                        { inferredPattern.env with
                        , context = TryBranch (CA.patternPos pa)
                        }

                    typedExpression & expressionType =
                        inferExpression patternEnv exp @state

                    addEquality newEnv Why_TryExpression finalType expressionType @state

                    typedPattern & typedExpression

            Try pos { value = typedValue, type = valueType, patternsAndExpressions = typedPatternsAndExpressions } & finalType


        DestroyIn name expression:
            typedExpression & expressionType =
                inferExpression env expression @state

            DestroyIn name typedExpression & expressionType


checkExpression as Env: CanonicalType: Expression CanonicalType: State@: Expression UnificationType =
    env: expectedType: caExpression: @state:

    try caExpression & expectedType as

        LiteralNumber pos n & TypeOpaque _ CoreTypes.numberUsr:
            LiteralNumber pos n


        LiteralText pos text & TypeOpaque _ CoreTypes.textUsr:
            LiteralText pos text


        Variable pos ref & _:
            try getVariableByRef ref env as
                Nothing:
                    checkError env pos (ErrorVariableNotFound ref) @state

                Just var:
                    if typeIsCompatibleWith expectedType var.type then
                        Variable pos ref
                    else
                        checkError env pos (ErrorVariableTypeIncompatible ref var expectedType) @state


        Constructor pos usr & _:
            try getConstructorByUsr usr env as
                Nothing:
                    checkError env pos (ErrorConstructorNotFound usr) @state

                Just cons:
                    if typeIsCompatibleWith expectedType cons.type then
                        Constructor pos usr
                    else
                        checkError env pos (ErrorConstructorTypeIncompatible usr cons expectedType) @state


        Lambda pos pattern lambdaModifier body & TypeFunction _ in mod out:
            typedPattern & localEnv =
                checkPattern env in pattern @state

            typedBody =
                checkExpression localEnv out body @state

            checkModifiers lambdaModifier mod @state

            Lambda pos typedPattern mod typedBody


        Call pos reference argument unusedType & _:
            checkCallCo env expectedType pos reference [argument & unusedType] @state


        CallCo pos reference argsAndUnusedTypes & _:
            checkCallCo env expectedType pos reference argsAndUnusedTypes @state


        Record pos (Just ext) valueByName & TypeRecord _ typeByName:

            all values must exist in typeByName
            ext must have type expected type


        Record pos Nothing valueByName & TypeRecord _ typeByName:
            aOnly & both & bOnly =
                onlyBothOnly valueByName typeByName

            if aOnly /= Dict.empty then
                checkError pos "record has extra attributes that are not in annotation"

            else if bOnly /= Dict.empty then
                checkError pos "record is missing attributes that are in the annotation"

            else
                typedAttrs =
                    Dict.map both name: (value & type):
                        # TODO add attribute name to env!?
                        checkExpression env type value @state

            Record pos Nothing typedAttrs


        RecordAccess pos attrName exp & _:

            typedExpression & expressionType =
                inferExpression env exp @state

            newId =
                getNewId @state

            requiredType =
                TypeExtra << TypeRecordExt pos newId (Dict.singleton attrName expectedType)

            addEquality env Why_AttributeAccess expressionType requiredType @state

            RecordAccess pos attrName typedExpression


        LetIn { pattern, body } rest & _:

            inferredPattern =
                inferPattern env @state

            newEnv =
                { inferredPattern.env with
                , context = LetInBody pos .context
                }

            checkExpression newEnv expectedType rest @state


        If pos { condition, true, false }:

            typedCondition =
                checkExpression { env with context = IfCondition pos } CoreTypes.bool condition @state

            typedTrue =
                checkExpression { env with context = IfTrue } expectedType true @state

            typedFalse =
                checkExpression { env with context = IfFalse } expectedType false @state

            If pos {
                , condition = typedCondition
                , true = typedTrue
                , false = typedFalse
                }


        Try pos { value, type = __unused __, patternsAndExpressions }:

            typedValue & valueType =
                inferExpression env value @state

            typedPatternsAndExpressions =
                patternsAndExpressions >> List.map (pa & exp):

                    inferredPattern =
                        inferPattern env pa @state

                    newEnv =
                        { inferredPattern.env with
                        , context = TryBranch (CA.patternPos pa)
                        }

                    addEquality env Why_TryPattern inferredPattern.type valueType @state

                    typedExpression =
                        checkExpression patternEnv expectedType exp @state

                    typedPattern & typedExpression


        DestroyIn name exp:
            DestroyIn name << checkExpression env expectedType exp @state


    _:
        checkError "Not compatible" @state




checkCallCo as Env: CanonicalType: Pos: Expression: [Argument & unusedType]: State@: Expression UnificationType =
    env: expectedType: pos: reference: givenArgs: @state:

    typedReference & referenceType =
        inferExpression env reference @state

    typedArgumentsAndArgumentTypes =
        givenArgs >> List.map (arg & unusedType):
            inferArgument env argument @state

    referenceArgs & referenceReturn =
        linearizeCurriedParameters referenceType []

    addEquality env Why_ReturnType referenceReturn expectedType @state

    if referenceArgs /= [] then

        rl =
            List.length referenceArgs

        gl =
            List.length givenArgs

        if rl > gl then
            checkError "not enough arguments"

        else if rl < gl then
            checkError "too many arguments"

        else
            typedArgs =
                List.map2 Tuple.pair referenceArgs typedArgumentsAndArgumentTypes
                >> List.indexedMap stuff index: ((refMod & refType) & (tyArg & argTy)):
                      addEquality env (Why_Argument index) refType argTy @state

                      refMod & tyArg

            CallCo pos typedReference typedArgs

    else
        try referenceType as
            TypeUnificationVariable id:

                referenceTypeFromArguments =
                    expectedType >> List.forReversed typedArgumentsAndArgumentTypes (tyArg & argTy): type:
                        TypeFunction argTy modifier type

                addEquality env Why_CalledAsFunction referenceType referenceTypeFromArguments @state

                typedArgs =
                    List.map Tuple.first typedArgumentsAndArgumentTypes

                CallCo pos typedReference typedArgs

            _:
                checkError "Trying to call as a function"



inferArgument as Env: Argument CanonicalType: State@: Argument UnificationType & UnificationType =
    env: arg: @state:

    try arg as
        ArgumentExpression exp:
            typedExp & expType =
                inferExpression env exp @state

            ArgumentExpression typedExt & expType

        ArgumentRecycle pos ref:
            try getVariableByRef ref env as

                Nothing:
                    inferenceError env (ErrorVariableNotFound ref pos) @state

                Just var:
                    ArgumentRecycle pos ref & var.type



#
#
# Patterns
#
#
alias PatternOut = {
    , patternType as UnificationType
    , typedPatern as Pattern UnificationType
    , env as Env
    }


inferPattern as Env: Pattern CanonicalType: State@: PatternOut =
    env: pattern: @state:

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
                            None
                        else
                            None

                        { env with variables = Dict.insert name type .variables }

            typedPattern =
                PatternAny pos { isUnique, maybeName, maybeAnnotation, type }

            { typedPattern, patternType, env = newEnv }


        PatternLiteralText pos text:
            {
            , typedPattern = PatternLiteralText pos text
            , patternType = CoreTypes.textType
            , env
            }


        PatternLiteralNumber pos n:
            {
            , typedPattern = PatternLiteralNumber pos n
            , patternType = CoreTypes.numberType
            , env
            }


        PatternConstructor pos usr arguments:

            typedArguments & argumentTypes & newEnv =
                [] & [] & env >> List.forReversed arguments arg: (typedPas & paTypes & envX):

                    out =
                        inferPattern envX arg @state

                    (out.typedPattern :: typedPas) & (out.patternType :: paTypes) & out.env

            finalType =
                try getConstructor usr env as

                    Nothing:
                        inferenceError env (ErrorVariableNotFound usr pos) @state

                    Just cons:
                        argModAndTypes & returnType =
                            linearizeCurriedParameters (generalize cons.type @state) []

                        check that arguments number matches

                        add equalities for each argument

                        if there is any unique then
                            TypeUnique returnType
                        else
                            returnType

            {
            , typedPattern = PatternConstructor pos usr typedArguments
            , patternType = finalType
            , env = newEnv
            }


        PatternRecord pos pasAndUnusedTypesByName:

            typedPatternsAndPatternTypesByName & newEnv =
                Dict.empty & env >> Dict.for pasAndUnusedTypesByName name: (pa & __unused__): (dict & envX):

                    out =
                        inferPattern envX arg @state

                    Dict.insert name (out.typedPattern & out.patternType) dict & out.env

            patternTypeByName =
                typedPatternsAndPatternTypesByName >> Dict.map name: Tuple.second

            PatternRecord pos typedPatternsAndPatternTypesByName & TypeExtra (TypeRecordExt pos (newId @state) patternTypeByName)



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

