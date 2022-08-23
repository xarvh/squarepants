
#
# All and TypeDef are not (yet) used for the AST, but just as intermediate types
# Probably will be used in the AST once we properly track dependencies
#
alias All a =
    Dict Meta.UniqueSymbolReference a


union TypeDef =
    , TypeDefAlias AliasDef
    , TypeDefUnion UnionDef


#
# AST
#


union TyvarUniqueness =
    # an unique tyvar is expressed as (TypeMutable TypeVariable)
    # TODO: just turn it into a canBeMutable Bool in TyvarFlags?
    , TyvarImmutable
    , TyvarEither


alias TyvarFlags = {
    , nonFn as Bool
    , uniqueness as TyvarUniqueness
    }


union Type =
    , TypeConstant Pos Meta.UniqueSymbolReference [Type]
    # Tyvars with the same name must have the same Flags
    # Duplicating them in the constructor is not ideal, but for now seems handy
    , TypeVariable Pos Name TyvarFlags
    , TypeFunction Pos Type LambdaModifier Type
    , TypeRecord Pos (Dict Name Type)
    , TypeRecordExt Pos Name TyvarFlags (Dict Name Type)
    , TypeAlias Pos Meta.UniqueSymbolReference Type
    , TypeMutable Pos Type


union Pattern =
    , PatternAny Pos Bool (Maybe Text) (Maybe Type)
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern]
    , PatternRecord Pos (Dict Name Pattern)


#
# Expression
#


# A reference to a defined variable
union Ref =
    # This is for stuff defined inside the current function/block
    , RefBlock Name
    # This is for stuff defined at root level
    , RefRoot Meta.UniqueSymbolReference


alias VariableArgs = {
    , ref as Ref
    , attrPath as [Name]
    }


union Argument =
    , ArgumentExpression Expression
    # TODO should we distinguish between a locally declared mutable and a mutable parameter?
    , ArgumentMutable Pos VariableArgs


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos VariableArgs
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos Pattern LambdaModifier Expression
    , Call Pos Expression Argument
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

    # Does it make sense that this is part of the Canonical AST?
    , DestroyIn Name Expression


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
    , constructors as Dict Name Constructor
    , directTypeDeps as TypeDeps
    }


alias Constructor = {
    , pos as Pos

    # type and args are redundant
    , typeUsr as Meta.UniqueSymbolReference
    , type as Type
    , args as [Type]
    }


alias ValueDef = {
    , pattern as Pattern
    , native as Bool
    , parentDefinitions as [Pattern]
    , body as Expression
    #
    , directTypeDeps as TypeDeps
    , directConsDeps as Set Meta.UniqueSymbolReference
    , directValueDeps as Set Meta.UniqueSymbolReference
    }


alias Annotation = {
    , type as Type
    , nonFn as Dict Name None
    }


alias Module = {
    , umr as Meta.UniqueModuleReference
    , asText as Text

    , aliasDefs as Dict Name AliasDef
    , unionDefs as Dict Name UnionDef
    , valueDefs as Dict Pattern ValueDef
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

skipLetIns as Expression: Expression =
    expr:
    try expr as
        LetIn def e: skipLetIns e
        _: expr





#
# helpers
#


typePos as Type: Pos =
    ty:
    try ty as
        TypeConstant p _ _: p
        #TypeGeneratedVar _: Pos.I 3
        #TypeAnnotatedVar p _: p
        TypeVariable p _ _: p
        TypeFunction p _ _ _: p
        TypeRecord p _: p
        TypeRecordExt p _ _ _: p
        TypeAlias p _ _: p
        TypeMutable p _: p


typeIsMutable as Type: Bool =
    ty:
    try ty as
        TypeConstant _ _ _: False
        TypeVariable _ _ _: False
        TypeFunction _ _ _ _: False
        TypeRecord _ attrs: Dict.any (k: typeIsMutable) attrs
        TypeRecordExt _ _ _ attrs: Dict.any (k: typeIsMutable) attrs
        TypeAlias _ _ t: typeIsMutable t
        TypeMutable _ _: True


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p _ _ _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p path ps: p
        PatternRecord p ps: p


patternIsMutable as Pattern: Bool =
    pattern:

    try pattern as
        PatternAny pos isMutable maybeName maybeType: isMutable
        PatternLiteralNumber pos _: False
        PatternLiteralText pos _: False
        PatternConstructor pos path ps: List.any patternIsMutable ps
        PatternRecord pos ps: Dict.any (k: patternIsMutable) ps


patternNames as Pattern: Dict Name Pos =
    p:
    try p as
        PatternAny pos _ Nothing _: Dict.empty
        PatternAny pos _ (Just n) _: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNames >> Dict.join) Dict.empty


patternMutabilityByName as Pattern: Dict Name (Bool & Pos) =
    p:
    try p as
        PatternAny pos _ Nothing _: Dict.empty
        PatternAny pos isMutable (Just n) _: Dict.singleton n (isMutable & pos)
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternMutabilityByName >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: v: v >> patternMutabilityByName >> Dict.join) Dict.empty





patternNamedTypes as Pattern: Dict Name (Pos & Maybe Type) =
    p:
    try p as
        PatternAny pos _ Nothing maybeType: Dict.empty
        PatternAny pos _ (Just n) maybeType: Dict.singleton n (pos & maybeType)
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNamedTypes >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNamedTypes >> Dict.join) Dict.empty


argumentPos as Argument: Pos =
    arg:
    try arg as
        ArgumentExpression e: expressionPos e
        ArgumentMutable pos _: pos


expressionPos as Expression: Pos =
    e:
    try e as
        LiteralText pos _: pos
        LiteralNumber pos _: pos
        Variable pos _: pos
        Constructor pos _: pos
        Lambda pos _ _ _: pos
        Record pos _ _: pos
        Call pos _ _: pos
        If pos _: pos
        Try pos _ _: pos
        LetIn valueDef _: patternPos valueDef.pattern


#
# Stuff that should live... somewhere else?
#

alias InstanceVariable =
    { definedAt as Pos
    # TODO: ty -> type
    , ty as Type
    , freeTypeVariables as Dict Name CA.TyvarFlags
    , isMutable as Bool
    }


alias Globals = {
    , types as CA.All CA.TypeDef
    , constructors as CA.All CA.Constructor
    , instanceVariables as ByUsr InstanceVariable
    }

