
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


alias Block =
    # TODO a Block is an Expression that also allows let..in
    , Expression


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos USR
    , Fn Pos [Parameter] Block
    , Call Pos Expression [Argument]
      #
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
      #
      # Also, attribute values could be Blocks
      #
    , Record Pos (Maybe Expression) (Dict Name Expression)
    , RecordAccess Pos Name Expression
    # TODO remove this once we implement Blocks
    , LetIn ValueDef Block
    , If Pos
        {
        , condition as Expression
        , true as Block
        , false as Block
        }
    , Try Pos
        {
        , value as Expression
        , patternsAndExpressions as [Uniqueness & Pattern & Block]
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
    , body as Block

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
    , fsPath as Text
    , umr as UMR
    , asText as Text

    , aliasDefs as Dict Name AliasDef
    , unionDefs as Dict Name UnionDef
    , valueDefs as Dict Pattern ValueDef
    }


initModule as fn Text, UMR, Text: Module =
    fn fsPath, umr, asText:
    {
    , umr
    , fsPath
    , asText
    , aliasDefs = Dict.empty
    , unionDefs = Dict.empty
    , valueDefs = Dict.empty
    }



#
# helpers
#


parTypeToRaw as fn ParType: RawType =
    fn p:
    try p as
        , ParRe raw: raw
        , ParSp full: full.raw


typeTyvars as fn RawType: Dict Name Pos =
    fn raw:

    fromList as fn [RawType]: Dict Name Pos =
        fn list:
        List.for Dict.empty list (fn item, acc: Dict.join acc (typeTyvars item))

    try raw as
        , TypeNamed _ _ args: fromList args
        , TypeFn _ pars to: fromList (to.raw :: List.map parTypeToRaw pars)
        , TypeRecord _ attrs: fromList (Dict.values attrs)
        , TypeAnnotationVariable pos name: Dict.ofOne name pos
        , TypeError _: Dict.empty


typeUnivars as fn RawType: Dict UnivarId None =
    fn raw:

    fromList as fn [RawType]: Dict UnivarId None =
        fn list:
        List.for Dict.empty list (fn item, acc: Dict.join acc (typeUnivars item))

    insertUni as fn Uniqueness, Dict UnivarId None: Dict UnivarId None =
        fn uni, acc:
        try uni as
            , Depends uid: Dict.insert uid None acc
            , _: acc

    parUnivars as fn ParType, Dict UnivarId None: Dict UnivarId None =
        fn par, acc:
        try par as
            , ParSp full: insertUni full.uni (typeUnivars full.raw)
            , ParRe _: acc

    try raw as
        , TypeNamed _ _ args: fromList args
        , TypeRecord _ attrs: fromList (Dict.values attrs)
        , TypeAnnotationVariable pos name: Dict.empty
        , TypeError _: Dict.empty
        , TypeFn _ pars to:
            Dict.empty
            >> insertUni to.uni __
            >> List.for __ pars parUnivars


patternPos as fn Pattern: Pos =
    fn pa:
    try pa as
        , PatternAny p _: p
        , PatternLiteralText p _: p
        , PatternLiteralNumber p _: p
        , PatternConstructor p _ _: p
        , PatternRecord p _ _: p


patternTyvars as fn Pattern: Dict Name Pos =
    fn pa:
    try pa as
        , PatternAny _ { maybeName = _, maybeAnnotation = Just t }: typeTyvars t
        , PatternAny _ { maybeName = _, maybeAnnotation = Nothing }: Dict.empty
        , PatternLiteralText _ _: Dict.empty
        , PatternLiteralNumber _ _: Dict.empty
        , PatternConstructor _ _ args: List.for Dict.empty args (fn arg, acc: Dict.join acc (patternTyvars arg))
        , PatternRecord _ _ attrs: Dict.for Dict.empty attrs (fn k, arg, acc: Dict.join acc (patternTyvars arg))


patternNames as fn Pattern: Dict Name { pos as Pos, maybeAnnotation as Maybe RawType } =
    fn p:
    try p as
        , PatternAny pos { maybeName = Nothing, maybeAnnotation = _ }: Dict.empty
        , PatternAny pos { maybeName = Just n, maybeAnnotation }: Dict.ofOne n { pos, maybeAnnotation }
        , PatternLiteralNumber pos _: Dict.empty
        , PatternLiteralText pos _: Dict.empty
        , PatternConstructor pos path ps: List.for Dict.empty ps (fn x, a: x >> patternNames >> Dict.join a __)
        , PatternRecord pos _ ps: Dict.for Dict.empty ps (fn k, v, a: v >> patternNames >> Dict.join a __)


expressionPos as fn Expression: Pos =
    fn exp:
    try exp as
        , LiteralNumber p _: p
        , LiteralText p _: p
        , Variable p _: p
        , Constructor p _: p
        , Fn p _ _: p
        , Call p _ _: p
        , Record p _ _: p
        , RecordAccess p _ _: p
        , LetIn def e: expressionPos e
        , If p _: p
        , Try p _: p
        , DestroyIn _ _: Pos.G

