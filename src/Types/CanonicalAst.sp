
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
      #
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
      #
      # Also, attribute values could be Blocks
      #
    , Record Pos (Maybe Expression) (Dict Name Expression)
    , RecordAccess Pos Name Expression
    # TODO remove this once we implement Blocks
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


alias Annotation = {
    , raw as RawType
    , tyvars as Dict Name { nonFn as Maybe Pos }
    , univars as Dict UnivarId None
    }


union Pattern =
    , PatternAny Pos (Maybe Name) (Maybe Annotation)
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]
    , PatternRecord Pos PatternCompleteness (Dict Name Pattern)


union PatternCompleteness =
    , Partial
    , Complete


alias ValueDef =
    {
    , uni as Uniqueness
    , pattern as Pattern

    # TODO: have maybeBody instead of native?
    , native as Bool
    , body as Expression

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
    , pars as [Name & Pos]
    , type as RawType
    , directTypeDeps as TypeDeps
    }


alias UnionDef =
    {
    , usr as USR
    , pars as [Name & Pos]
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
            , ParRe _: acc
            , ParSp full:
                acc
                >> Dict.join __ (typeUnivars full.raw)
                >> insertUni full.uni __

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
        , PatternAny p _ _: p
        , PatternLiteralText p _: p
        , PatternLiteralNumber p _: p
        , PatternConstructor p _ _: p
        , PatternRecord p _ _: p


patternTyvars as fn Pattern: Dict Name { nonFn as Maybe Pos } =
    fn pa:
    try pa as
        , PatternAny _ _ (Just ann): ann.tyvars
        , PatternAny _ _ Nothing: Dict.empty
        , PatternLiteralText _ _: Dict.empty
        , PatternLiteralNumber _ _: Dict.empty
        , PatternConstructor _ _ args: List.for Dict.empty args (fn arg, acc: Dict.join acc (patternTyvars arg))
        , PatternRecord _ _ attrs: Dict.for Dict.empty attrs (fn k, arg, acc: Dict.join acc (patternTyvars arg))


patternUnivars as fn Pattern: Dict UnivarId None =
    fn pa:
    try pa as
        , PatternAny _ _ (Just ann): ann.univars
        , PatternAny _ _ Nothing: Dict.empty
        , PatternLiteralText _ _: Dict.empty
        , PatternLiteralNumber _ _: Dict.empty
        , PatternConstructor _ _ args: List.for Dict.empty args (fn arg, acc: Dict.join acc (patternUnivars arg))
        , PatternRecord _ _ attrs: Dict.for Dict.empty attrs (fn k, arg, acc: Dict.join acc (patternUnivars arg))


patternNames as fn Pattern: [{ name as Name, pos as Pos, maybeAnnotation as Maybe Annotation }] =
    rec =
        fn p, acc:
        try p as
            , PatternAny pos Nothing _: acc
            , PatternAny pos (Just name) maybeAnnotation: [ { name, pos, maybeAnnotation }, ...acc]
            , PatternLiteralNumber pos _: acc
            , PatternLiteralText pos _: acc
            , PatternConstructor pos path ps: List.for acc ps rec
            , PatternRecord pos _ ps: Dict.for acc ps (fn k, v, a: rec v a)

    rec __ []


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

