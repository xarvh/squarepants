
# A reference to a defined variable
alias Ref = CA.Ref


alias LambdaModifier =
    CA.LambdaModifier


alias UnificationVariableId =
    Int


union Type =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type]
    , TypeAlias Pos Meta.UniqueSymbolReference [Type]
    , TypeFunction Pos Type LambdaModifier Type
    , TypeRecord Pos (Dict Name Type)
    , TypeUnique Pos Type
    , TypeUnificationVariable UnificationVariableId
    , TypeRecordExt UnificationVariableId (Dict Name Type)


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos (Pattern) LambdaModifier (Expression)
    , Call Pos (Expression) (Argument) Type
    , CallCo Pos (Expression) [Argument & Type]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe (Expression)) (Dict Name (Expression))
    , RecordAccess Pos Name (Expression)
    , LetIn (ValueDef) (Expression)
    , If Pos {
        , condition as (Expression)
        , true as (Expression)
        , false as (Expression)
        }
    , Try Pos {
        , value as (Expression)
        , type as Type
        , patternsAndExpressions as [Pattern & Expression]
        }
    , DestroyIn Name (Expression)


union Pattern =
    , PatternAny Pos {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CA.Type
        , type as Type
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern]
    , PatternRecord Pos (Dict Name (Pattern & type))


union Argument =
    , ArgumentExpression (Expression)
    , ArgumentRecycle Pos [Name] Ref


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }


alias ValueDef = {
    , pattern as Pattern
    , native as Bool
    , body as Expression
    , tyvars as Dict Name TypeClasses
    }




#
# Module
#

alias TypeDeps =
    Set Meta.UniqueSymbolReference

alias AliasDef = {
    , usr as Meta.UniqueSymbolReference
    , args as [At Name]
    , type as Type
    , directTypeDeps as TypeDeps
    }


alias UnionDef = {
    , usr as Meta.UniqueSymbolReference
    , args as [Name]
    , constructors as Dict Name (Constructor)
    , directTypeDeps as TypeDeps
    }


alias Constructor = {
    , pos as Pos

    # type and args are redundant
    , typeUsr as Meta.UniqueSymbolReference
    , type as Type
    , args as [Type]
    }


alias Module = {
    , umr as Meta.UniqueModuleReference
    , asText as Text

#    , aliasDefs as Dict Name AliasDef
#    , unionDefs as Dict Name (UnionDef)
#    , valueDefs as Dict (Pattern) (ValueDef)
    }


initModule as Text: Meta.UniqueModuleReference: Module =
    asText: umr:
    {
    , umr
    , asText
    , aliasDefs = Dict.empty
    , unionDefs = Dict.empty
    , valueDefs = Dict.empty
    }



#
#
#

skipLetIns as CA.Expression: CA.Expression =
    expr:
    try expr as
        LetIn def e: skipLetIns e
        _: expr





#
# helpers
#


#typePos as Type: Pos =
#    ty:
#    try ty as
#        TypeConstant p _ _: p
#        #TypeGeneratedVar _: Pos.I 3
#        #TypeAnnotatedVar p _: p
#        TypeVariable p _ _: p
#        TypeFunction p _ _ _: p
#        TypeRecord p _: p
#        TypeRecordExt p _ _ _: p
#        TypeAlias p _ _: p
#        TypeMutable p _: p
#
#
#typeContainsUniques as Type: Bool =
#    ty:
#    try ty as
#        TypeConstant _ _ _: False
#        TypeVariable _ _ _: False
#        TypeFunction _ _ _ _: False
#        TypeRecord _ attrs: Dict.any (k: typeContainsUniques) attrs
#        TypeRecordExt _ _ _ attrs: Dict.any (k: typeContainsUniques) attrs
#        TypeAlias _ _ t: typeContainsUniques t
#        TypeMutable _ _: True


#patternPos as Pattern: Pos =
#    pa:
#    try pa as
#        PatternAny p _ _ _: p
#        PatternLiteralText p _: p
#        PatternLiteralNumber p _: p
#        PatternConstructor p path ps: p
#        PatternRecord p ps: p
#
#
#patternIsMutable as Pattern: Bool =
#    pattern:
#
#    try pattern as
#        PatternAny pos isMutable maybeName maybeType: isMutable
#        PatternLiteralNumber pos _: False
#        PatternLiteralText pos _: False
#        PatternConstructor pos path ps: List.any patternIsMutable ps
#        PatternRecord pos ps: Dict.any (k: patternIsMutable) ps
#
#
#patternNames as Pattern: Dict Name Pos =
#    p:
#    try p as
#        PatternAny pos _ Nothing _: Dict.empty
#        PatternAny pos _ (Just n) _: Dict.singleton n pos
#        PatternLiteralNumber pos _: Dict.empty
#        PatternLiteralText pos _: Dict.empty
#        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
#        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNames >> Dict.join) Dict.empty
#
#
#patternMutabilityByName as Pattern: Dict Name (Bool & Pos) =
#    p:
#    try p as
#        PatternAny pos _ Nothing _: Dict.empty
#        PatternAny pos isMutable (Just n) _: Dict.singleton n (isMutable & pos)
#        PatternLiteralNumber pos _: Dict.empty
#        PatternLiteralText pos _: Dict.empty
#        PatternConstructor pos path ps: List.for ps (x: x >> patternMutabilityByName >> Dict.join) Dict.empty
#        PatternRecord pos ps: Dict.for ps (k: v: v >> patternMutabilityByName >> Dict.join) Dict.empty
#
#
#patternNamedTypes as Pattern: Dict Name (Pos & Maybe Type) =
#    p:
#    try p as
#        PatternAny pos _ Nothing maybeType: Dict.empty
#        PatternAny pos _ (Just n) maybeType: Dict.singleton n (pos & maybeType)
#        PatternLiteralNumber pos _: Dict.empty
#        PatternLiteralText pos _: Dict.empty
#        PatternConstructor pos path ps: List.for ps (x: x >> patternNamedTypes >> Dict.join) Dict.empty
#        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNamedTypes >> Dict.join) Dict.empty


#argumentPos as Argument: Pos =
#    arg:
#    try arg as
#        ArgumentExpression e: expressionPos e
#        ArgumentMutable pos _: pos


#expressionPos as Expression: Pos =
#    e:
#    try e as
#        LiteralText pos _: pos
#        LiteralNumber pos _: pos
#        Variable pos _: pos
#        Constructor pos _: pos
#        Lambda pos _ _ _: pos
#        Record pos _ _: pos
#        Call pos _ _: pos
#        CallCo pos _ _: pos
#        If pos _: pos
#        Try pos _ _: pos
#        LetIn valueDef _: patternPos valueDef.pattern


#
# Stuff that should live... somewhere else?
#

alias InstanceVariable = {
    , definedAt as Pos
    # TODO: ty -> type
    , ty as Type
    , freeTypeVariables as Dict Name TypeClasses
    # TODO: isMutable -> isUnique
    , isMutable as Bool
    }


alias Globals = {
    , types as CA.All CA.TypeDef
    , constructors as CA.All (CA.Constructor)
    , instanceVariables as ByUsr (InstanceVariable)
    }

