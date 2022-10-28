
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


# A reference to a defined variable
union Ref =
    # This is for stuff defined inside the current function/block
    , RefLocal Name
    # This is for stuff defined at root level
    , RefGlobal Meta.UniqueSymbolReference


union LambdaModifier =
    , LambdaNormal
    , LambdaRecycle


union Type =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type]
    , TypeAlias Pos Meta.UniqueSymbolReference [Type]
    , TypeFunction Pos Type LambdaModifier Type
    , TypeRecord Pos (Dict Name Type)
    , TypeUnique Pos Type
    , TypeAnnotationVariable Pos Name


alias CanonicalType =
    Type


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos Pattern LambdaModifier Expression
    , Call Pos Expression Argument
    , CallCo Pos Expression [Argument]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe Expression) (Dict Name Expression)
    , RecordAccess Pos Name Expression
    , LetIn ValueDef Expression
    , If Pos {
        , condition as Expression
        , true as Expression
        , false as Expression
        }
    , Try Pos {
        , value as Expression
        , patternsAndExpressions as [Pattern & Expression]
        }
    , DestroyIn Name Expression


union Pattern =
    , PatternAny Pos {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CanonicalType
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern]
    , PatternRecord Pos (Dict Name Pattern)


union Argument =
    , ArgumentExpression Expression
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


typeTyvars as Type: Dict Name Pos =
    ty:

    fromList as [Type]: Dict Name Pos =
        list:
        List.for list (item: acc: Dict.join acc (typeTyvars item)) Dict.empty

    try ty as
        TypeOpaque _ _ args: fromList args
        TypeAlias _ _ args: fromList args
        TypeFunction _ from _ to: fromList [from, to]
        TypeRecord _ attrs: fromList (Dict.values attrs)
        TypeUnique _ t: typeTyvars t
        TypeAnnotationVariable pos name: Dict.singleton name pos


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p _ _: p
        PatternRecord p _: p


patternContainsUnique as Pattern: Bool =
    pattern:
    try pattern as
        PatternAny _ { isUnique, maybeName = _, maybeAnnotation = _ }: isUnique
        PatternLiteralText _ _: False
        PatternLiteralNumber _ _: False
        PatternConstructor _ _ args: List.any patternContainsUnique args
        PatternRecord _ attrs: Dict.any (k: patternContainsUnique) attrs


patternTyvars as Pattern: Dict Name Pos =
    pa:
    try pa as
        PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Just t }: typeTyvars t
        PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Nothing }: Dict.empty
        PatternLiteralText _ _: Dict.empty
        PatternLiteralNumber _ _: Dict.empty
        PatternConstructor _ _ args: List.for args (arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty
        PatternRecord _ attrs: Dict.for attrs (k: arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty


patternNames as Pattern: Dict Name { pos as Pos, isUnique as Bool, maybeAnnotation as Maybe Type } =
    p:
    try p as
        PatternAny pos { isUnique = _, maybeName = Nothing, maybeAnnotation = _ }: Dict.empty
        PatternAny pos { isUnique, maybeName = Just n, maybeAnnotation }: Dict.singleton n { pos, isUnique, maybeAnnotation }
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: v: v >> patternNames >> Dict.join) Dict.empty



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
    , type as Type
    , isUnique as Bool
    }


alias Globals = {
    , types as CA.All (CA.TypeDef)
    , constructors as CA.All (CA.Constructor)
    , instanceVariables as ByUsr InstanceVariable
    }

