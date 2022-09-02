[#
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
#]




union CanonicalType_ =
    , TypeAlias Pos Meta.UniqueSymbolReference Type

alias CanonicalType =
    Type CanonicalType_



union UnificationType_ =
    , TypeUnificationVariable Int

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










alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }



union Type extra =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type extra]
    , TypeVariable Pos Name
    , TypeFunction Pos (Type extra) LambdaModifier (Type extra)
    , TypeRecord Pos (Dict Name (Type extra))
    , TypeRecordExt Pos Name (Dict Name (Type extra))
    , TypeMutable Pos (Type extra)
    , TypeExtra extra






union Expression type =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos VariableArgs
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos (Pattern type) LambdaModifier Expression
    , Call Pos Expression Argument type
    , CallCo Pos Expression [Argument & type]
    , Record Pos (Maybe VariableArgs) (Dict Name Expression)
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







inferExpression as Env: Expression CanonicalType: Expression UnificationType & Constraint =
    ...


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

    check that replacingType does not contain uniVarId

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

