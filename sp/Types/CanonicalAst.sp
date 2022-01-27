
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


alias TyVarId =
    Int


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


union Pattern =
    , PatternAny Pos (Maybe Text) (Maybe Type)
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern]
    , PatternRecord Pos (Dict Name Pattern)


union Statement =
    , Definition ValueDef
    # Evaluations are needed for return, mutation and debug
    , Evaluation Expression


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
    , Lambda Pos Parameter [Statement]
    , Record Pos (Maybe VariableArgs) (Dict Name Expression)
    , Call Pos Expression Argument
    , If Pos {
        # we use the if also to get lazy ops and compacted compops, so even if the syntax does
        # not support statement blocks inside if condition, it's useful that the AST can model it.
        , condition as [Statement]
        , true as [Statement]
        , false as [Statement]
        }
    , Try Pos Expression [Pattern & [Statement]]


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
    , type as Type
    , args as [Type]
    }


alias ValueDef = {
    , pattern as Pattern
    , native as Bool
    , mutable as Bool
    , parentDefinitions as [Pattern]
    , nonFn as Set Name
    , body as [Statement]
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


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p n _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p path ps: p
        PatternRecord p ps: p


patternNames as Pattern: Dict Name Pos =
    p:
    try p as
        PatternAny pos Nothing _: Dict.empty
        PatternAny pos (Just n) _: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.foldl (x: x >> patternNames >> Dict.join) ps Dict.empty
        PatternRecord pos ps: Dict.foldl (k: v: v >> patternNames >> Dict.join) ps Dict.empty


patternNamedTypes as Pattern: Dict Name (Pos & Maybe Type) =
    p:
    try p as
        PatternAny pos Nothing _: Dict.empty
        PatternAny pos (Just n) maybeType: Dict.singleton n (pos & maybeType)
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.foldl (x: x >> patternNamedTypes >> Dict.join) ps Dict.empty
        PatternRecord pos ps: Dict.foldl (k: v: v >> patternNamedTypes >> Dict.join) ps Dict.empty


statementPos as Statement: Pos =
    stat:
    try stat as
        Definition def:
            try List.reverse def.body as
                []: patternPos def.pattern
                last :: _: Pos.range (patternPos def.pattern) (statementPos last)

        Evaluation expr:
            expressionPos expr


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


#
# Stuff that should live in TypeCheck but I moved here so I don't have to compile TypeCheck
#


alias InstanceVariable =
    { definedAt as Pos
    , ty as Type
    , freeTypeVariables as Dict Name { nonFn as Bool }
    , isMutable as Bool
    }


alias InstanceVariablesByRef =
    Dict CA.Ref InstanceVariable


getFreeTypeVars as Dict Name Pos: Dict Name a: Type: Dict Name { nonFn as Bool } =
    nonFreeTyvars: nonFn: ty:

    posToTyvar =
        name: pos:
        # as Name: Pos: TypeVariable
        { nonFn = Dict.member name nonFn }

    Dict.diff (typeTyvars ty) nonFreeTyvars
        >> Dict.map posToTyvar


typeTyvars as Type: Dict Name Pos =
    ty:
    try ty as
        CA.TypeVariable pos name:
            # TODO is pos equivalent to definedAt?
            Dict.singleton name pos

        CA.TypeFunction _ from fromIsMutable to:
            Dict.join (typeTyvars from) (typeTyvars to)

        CA.TypeConstant pos ref args:
            List.foldl (a: Dict.join (typeTyvars a)) args Dict.empty

        CA.TypeAlias _ path t:
            typeTyvars t

        CA.TypeRecord pos extensible attrs:
            init =
                try extensible as
                    Nothing:
                        Dict.empty

                    Just name:
                        Dict.singleton name pos

            Dict.foldl (n: t: Dict.join (typeTyvars t)) attrs init


#
# Stuff that should live in Pipeline?
#


alias Globals = {
    , types as CA.All CA.TypeDef
    , constructors as CA.All CA.Constructor
    , instanceVariables as CA.InstanceVariablesByRef
    }

