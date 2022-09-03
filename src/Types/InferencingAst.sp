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




union CanonicalType_ =
    , TypeAlias Pos Meta.UniqueSymbolReference Type

alias CanonicalType =
    Type CanonicalType_



union UnificationType_ =
    , TypeUnificationVariable UnificationVariableId
    , TypeRecordExt Pos UnificationVariableId (Dict Name (Type extra))

alias UnificationType =
    Type UnificationType_



alias ResolvedType =
    Type Never





union Why =
  ???


union Equality =
    , Equality Why UnificationType UnificationType


union Constraint =
    , C_Nest [Constraint]
    , C_Class Why UnificationType TypeClasses
    , C_Equality Why UnificationType UnificationType
    # TODO I don't know how to use this one
    #, C_Implication Name Constraint Constraint






#blah as { someAttr as b }: b =
#    x:
#    x.someAttr








alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }



union Type extra =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type extra]
    , TypeVariable Pos Name
    , TypeFunction Pos (Type extra) LambdaModifier (Type extra)
    , TypeRecord Pos (Dict Name (Type extra))
    , TypeMutable Pos (Type extra)
    , TypeExtra extra






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
        # we use the if also to get lazy ops and compacted compops, so even if the syntax does
        # not support statement blocks inside if condition, it's useful that the AST can model it.
        , condition as Expression
        , true as Expression
        , false as Expression
        }
    , Try Pos Expression [Pattern & Expression]
    , DestroyIn Name Expression


union Pattern type =
    , PatternAny Pos Bool (Maybe Text) type
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern type]
    , PatternRecord Pos (Dict Name (Pattern & type))


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


#
#
# Expressions
#
# TODO I *could* use a monad for State, but I don't want to because I hope to have good mutability sooner than later.
# (I'm not using the current mutation impl because it's super slow).
#
inferExpression as Env: Expression CanonicalType: State: Expression UnificationType & State & UnificationType =
    env: caExpression: state:

    try caExpression as

        # TODO would be nice to be able to say that from here on, `caExpression as Expression any`
        LiteralNumber pos n:
            LiteralNumber pos n & state & CoreTypes.numberType


        LiteralText pos text:
            LiteralText pos text & state & CoreTypes.textType


        Variable pos ref:
            try getVariableByRef ref env as
                Nothing:
                    inferenceError env (ErrorVariableNotFound ref pos) state

                Just var:
                    ty & state1 = generalize var.type state
                    Variable pos ref & state1 & ty


        Constructor pos usr:
            try getConstructorByUsr usr env as
                Nothing:
                    inferenceError env (ErrorConstructorNotFound usr pos) state

                Just cons:
                    ty & state1 = generalize cons.type state
                    Constructor pos usr & state1 & ty


        Lambda pos pattern modifier body:

            inferredPattern =
                inferPattern { env with context = LambdaArgs pos .context }

            state1 =
                patternOut.state

            newEnv = { env with
                , context = LambdaBody pos .context
                , variables = insertVariables inferredPattern.variables variables .variables state
                }

            unificationBody & state2 & bodyType =
                inferExpression newEnv body state1

            type =
                TypeFunction Pos.G
                    inferredPattern.type
                    modifier
                    expressionType

            exp =
                Lambda pos inferredPattern.unificationPattern modifier unificationBody

            exp & state2 & type


        Call pos reference argument __unused__:

            uniRef & state1 & refType =
                inferExpression env reference state

            uniArg & state2 & argType =
                inferArgument env argument state1

            callType & state3 =
                newType state2

            state4 =
                try refType as
                    TypeFunction _ in modifier out:
                        state3
                        >> addEquality env CallArgument in argType
                        >> addEquality env CallReturns out callType

                    TypeUnificationVariable id:
                        modifier = ???
                        state3
                        >> addEquality env CalledAsFunction refType (TypeFunction argType modifier callType)

          Call pos uniRef uniArg callType & state4 & callType


        CallCo pos reference argsAndUnusedTypes:
            as above


        Record pos Nothing attrs:

            unificationAttrs & state1 & inferredAttrTypes =
                inferAttrs ...

            unificationExpression =
                Record pos Nothing unificationAttrs

            inferredType =
                TypeRecord Pos.G inferredAttrTypes

            unificationExpression & state1 & inferredType


        Record pos (Just ext) attrExpressions:

            unificationAttrs & state1 & inferredAttrTypes =
                inferAttrs ...

            unificationExtension & state1 & inferredExtensionType =
                inferExpression env ext state

            type & state2 =
                try inferredExtensionType as
                    TypeRecord _ attrTypes:
                        check that inferredAttrTypes are a subset of attrs and unify their types

                        type is inferredExtensionType

                    TypeRecordExt _ tyvarId extensionAttrTypes:
                        expressionOnly & both & extensionOnly =
                            onlyA_both_onlyB inferredAttrTypes extensionAttrTypes

                        unify `both`

                        # TODO: is it faster if I avoid creating a new tyvar when expressionOnly is empty?
                        newTyvarId = newTyvarId state

                        TypeRecordExt _ newTyvarId (Dict.join both expressionOnly extensionOnly)

                    TypeUnificationVariable id:
                        ...

            Record pos (Just unificationExtension) unificationAttrs & state2 & type



        RecordAccess pos attrName recordExpression:

            unificationExpr & state1 & inferredType =
                inferExpression env recordExpression state

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
                    addError "not a record!"


        LetIn ValueDef Expression
        If Pos {
        # we use the if also to get lazy ops and compacted compops, so even if the syntax does
        # not support statement blocks inside if condition, it's useful that the AST can model it.
        , condition as Expression
        , true as Expression
        , false as Expression
        }
        Try Pos Expression [Pattern & Expression]
        DestroyIn Name Expression


checkExpression as Env: Expression CanonicalType: Expression UnificationType =
    ...



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
                    replaceUnificationVariable why tyvarId t2 tail state

                t1 & TypeExtra (TypeUnificationVariable tyvarId):
                    replaceUnificationVariable why tyvarId t1 tail state

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

                TypeMutable _ m1 & TypeMutable _ m2:
                    solveEqualities (Equality why m1 m2 :: tail) state

                TypeVariable _ name1 & TypeVariable _ name2:
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


replaceUnificationVariable as Why: UnificationVariableId: UnificationType: [Equality]: State: State =
    why: uniVarId: replacingType: remainingEqualities: state:

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
    uniVarId: replacingType: targetType:

    rec = applySubstitutionToType uniVarId replacingType

    try targetType as
        TypeOpaque pos usr args:
            TypeOpaque pos usr (List.map rec args)

        TypeVariable Pos name:
            targetType

        TypeFunction pos in mod out:
            TypeFunction pos (rec in) mod (rec out)

        TypeRecord pos attrs:
            TypeFunction pos (Dict.map (k: rec) attrs)

        TypeRecordExt pos name attrs
            ??? name?
            TypeFunction pos (Dict.map (k: rec) attrs)

        TypeMutable pos t:
            TypeMutable pos (rec t)

        TypeExtra (TypeUnificationVariable id):
            if id == univarId then
                replacingType
            else
                targetType

