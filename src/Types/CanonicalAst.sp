
#
# All and TypeDef are not (yet) used for the AST, but just as intermediate types
# Probably will be used in the AST once we properly track dependencies
#
alias All a =
    Dict Meta.UniqueSymbolReference a


union TypeDef type =
    , TypeDefAlias AliasDef
    , TypeDefUnion (UnionDef type)


#
# AST
#


# A reference to a defined variable
union Ref =
    # This is for stuff defined inside the current function/block
    , RefLocal Name
    # This is for stuff defined at root level
    , RefGlobal Meta.UniqueSymbolReference


union LambdaModifier =
    , LambdaNormal
    , LambdaRecycle


alias UnificationVariableId =
    Int


union Type extra =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type extra]
    #, TypeAnnotationVariable Pos Name
    , TypeFunction Pos (Type extra) LambdaModifier (Type extra)
    , TypeRecord Pos (Dict Name (Type extra))
    , TypeUnique Pos (Type extra)
    , TypeExtra extra


union CanonicalTypeExtra =
    , TypeAnnotationVariable Pos Name
    , TypeAlias Pos Meta.UniqueSymbolReference


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
    , ArgumentRecycle Pos [Name] Ref


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }


alias ValueDef type = {
    , pattern as Pattern type
    , native as Bool
    , body as Expression type

    , tyvars as Dict Name TypeClasses

    # Do we need these here?
    , directTypeDeps as TypeDeps
    , directConsDeps as Set Meta.UniqueSymbolReference
    , directValueDeps as Set Meta.UniqueSymbolReference
    }




#
# Module
#

alias TypeDeps =
    Set Meta.UniqueSymbolReference

alias AliasDef = {
    , usr as Meta.UniqueSymbolReference
    , args as [At Name]
    , type as Type CanonicalType
    , directTypeDeps as TypeDeps
    }


alias UnionDef type = {
    , usr as Meta.UniqueSymbolReference
    , args as [Name]
    , constructors as Dict Name (Constructor type)
    , directTypeDeps as TypeDeps
    }


alias Constructor type = {
    , pos as Pos

    # type and args are redundant
    , typeUsr as Meta.UniqueSymbolReference
    , type as Type type
    , args as [Type type]
    }


alias Module type = {
    , umr as Meta.UniqueModuleReference
    , asText as Text

    , aliasDefs as Dict Name AliasDef
    , unionDefs as Dict Name (UnionDef type)
    , valueDefs as Dict (Pattern type) (ValueDef type)
    }


initModule as Text: Meta.UniqueModuleReference: Module type =
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

skipLetIns as Expression: Expression =
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

alias InstanceVariable type = {
    , definedAt as Pos
    # TODO: ty -> type
    , ty as Type type
    , freeTypeVariables as Dict Name TypeClasses
    # TODO: isMutable -> isUnique
    , isMutable as Bool
    }


alias Globals type = {
    , types as CA.All (CA.TypeDef type)
    , constructors as CA.All (CA.Constructor type)
    , instanceVariables as ByUsr (InstanceVariable type)
    }

