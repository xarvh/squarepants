
union RawType =
    # alias, opaque or union
    , TypeNamed Pos USR [RawType]
    , TypeFn Pos [ParType] FullType
    , TypeRecord Pos (Dict Name RawType)
    , TypeAnnotationVariable Pos Name
    # This is used as a placeholder when there is an error and a type can't be determined
    # It's useful to avoid piling up errors (I think)
    , TypeError Pos


union ParType =
    , ParRe RawType
    , ParSp FullType


alias FullType =
    {
    , uni as Uniqueness
    , raw as RawType
    }


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos USR
    , Fn Pos [Parameter] Expression
    , Call Pos Expression [Argument]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe Expression) (Dict Name Expression)
    , RecordAccess Pos Name Expression
    , LetIn ValueDef Expression
    , If Pos
        {
        , condition as Expression
        , true as Expression
        , false as Expression
        }
    , Try Pos
        {
        , value as Expression
        , patternsAndExpressions as [Uniqueness & Pattern & Expression]
        }
    , DestroyIn Name Expression


union Argument =
    , ArgumentExpression Expression
    , ArgumentRecycle Pos Name [Name]


union Parameter =
    , ParameterPattern Uniqueness Pattern
    , ParameterRecycle Pos Name
    , ParameterPlaceholder Name Int


union Pattern =
    , PatternAny Pos
        {
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe RawType
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]
    , PatternRecord Pos PatternCompleteness (Dict Name Pattern)


union PatternCompleteness =
    , Partial
    , Complete


alias Tyvar =
    {
    #, annotatedAt as Pos
    , allowFunctions as Bool
    }


alias ValueDef =
    {
    , uni as Uniqueness
    , pattern as Pattern

    # TODO: have maybeBody instead of native?
    , native as Bool
    , body as Expression

    , tyvars as Dict Name Tyvar
    , univars as Dict UnivarId None

    # Do we need these here?
    , directTypeDeps as TypeDeps
    , directConsDeps as Set USR
    , directValueDeps as Set USR
    }



#
# Module
#

alias TypeDeps =
    Set USR


alias AliasDef =
    {
    , usr as USR
    , pars as [At Name]
    , type as RawType
    , directTypeDeps as TypeDeps
    }


alias UnionDef =
    {
    , usr as USR
    , pars as [At Name]
    , constructors as Dict Name Constructor
    , directTypeDeps as TypeDeps
    }


alias Constructor =
    {
    , pos as Pos
    , typeUsr as USR
    , ins as [RawType]
    , out as RawType
    }


alias Module =
    {
    , umr as UMR
    , asText as Text

    , aliasDefs as Dict Name AliasDef
    , unionDefs as Dict Name UnionDef
    , valueDefs as Dict Pattern ValueDef
    }


initModule as Text: UMR: Module =
    asText: umr:
    {
    , umr
    , asText
    , aliasDefs = Dict.empty
    , unionDefs = Dict.empty
    , valueDefs = Dict.empty
    }



#
# helpers
#


parTypeToRaw as ParType: RawType =
    p:
    try p as
        ParRe raw: raw
        ParSp full: full.raw


typeTyvars as RawType: Dict Name Pos =
    raw:

    fromList as [RawType]: Dict Name Pos =
        list:
        List.for list (item: acc: Dict.join acc (typeTyvars item)) Dict.empty

    try raw as
        TypeNamed _ _ args: fromList args
        TypeFn _ pars to: fromList (to.raw :: List.map parTypeToRaw pars)
        TypeRecord _ attrs: fromList (Dict.values attrs)
        TypeAnnotationVariable pos name: Dict.singleton name pos
        TypeError _: Dict.empty


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p _ _: p
        PatternRecord p _ _: p


patternTyvars as Pattern: Dict Name Pos =
    pa:
    try pa as
        PatternAny _ { maybeName = _, maybeAnnotation = Just t }: typeTyvars t
        PatternAny _ { maybeName = _, maybeAnnotation = Nothing }: Dict.empty
        PatternLiteralText _ _: Dict.empty
        PatternLiteralNumber _ _: Dict.empty
        PatternConstructor _ _ args: List.for args (arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty
        PatternRecord _ _ attrs: Dict.for attrs (k: arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty


patternNames as Pattern: Dict Name { pos as Pos, maybeAnnotation as Maybe RawType } =
    p:
    try p as
        PatternAny pos { maybeName = Nothing, maybeAnnotation = _ }: Dict.empty
        PatternAny pos { maybeName = Just n, maybeAnnotation }: Dict.singleton n { pos, maybeAnnotation }
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos _ ps: Dict.for ps (k: v: v >> patternNames >> Dict.join) Dict.empty


expressionPos as Expression: Pos =
    exp:
    try exp as
        LiteralNumber p _: p
        LiteralText p _: p
        Variable p _: p
        Constructor p _: p
        Fn p _ _: p
        Call p _ _: p
        Record p _ _: p
        RecordAccess p _ _: p
        LetIn def exp: patternPos def.pattern
        If p _: p
        Try p _: p
        DestroyIn _ _: Pos.G

