
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


union Type =
    , TypeConstant Pos Meta.UniqueSymbolReference [Type]
    #
    # TODO before I can use this, I probably need to find out how to model the TypeRecord extension
    #
    # Maybe the way to go is to have a
    #
    #   type TyVarRef = Generated TyVarId, Annotated Pos Name
    #
    #, TypeGeneratedVar TyVarId
    #, TypeAnnotatedVar Pos Name
    , TypeVariable Pos Name
    , TypeFunction Pos Type Bool Type
    , TypeRecord Pos (Maybe Name) (Dict Name Type)
    , TypeAlias Pos Meta.UniqueSymbolReference Type
    , TypeMutable Pos Type


union Pattern =
    , PatternDiscard Pos (Maybe Type)
    , PatternNamed Pos Bool Text (Maybe Type)
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


union Parameter =
    , ParameterPattern Pattern
    , ParameterMutable Pos Name


union Argument =
    , ArgumentExpression Expression
    # TODO should we distinguish between a locally declared mutable and a mutable parameter?
    , ArgumentMutable Pos VariableArgs


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos VariableArgs
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos Parameter Expression
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
    , nonFn as Set Name
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
# Pos helpers
#


typePos as Type: Pos =
    ty:
    try ty as
        TypeConstant p _ _: p
        #TypeGeneratedVar _: Pos.I 3
        #TypeAnnotatedVar p _: p
        TypeVariable p _: p
        TypeFunction p _ _ _: p
        TypeRecord p _ _: p
        TypeAlias p _ _: p
        TypeMutable p _: p


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternDiscard p _: p
        PatternNamed p _ _ _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p path ps: p
        PatternRecord p ps: p


patternNames as Pattern: Dict Name Pos =
    p:
    try p as
        PatternDiscard pos _: Dict.empty
        PatternNamed pos _ n _: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNames >> Dict.join) Dict.empty


patternNamedTypes as Pattern: Dict Name (Pos & Maybe Type) =
    p:
    try p as
        PatternDiscard pos _: Dict.empty
        PatternNamed pos _ n maybeType: Dict.singleton n (pos & maybeType)
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
        Lambda pos _ _: pos
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
    , freeTypeVariables as Dict Name { nonFn as Bool }
    , isMutable as Bool
    }


alias Globals = {
    , types as CA.All CA.TypeDef
    , constructors as CA.All CA.Constructor
    , instanceVariables as ByUsr InstanceVariable
    }

