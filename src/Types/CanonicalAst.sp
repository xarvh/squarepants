
union Type =
    Type Pos Type_


union UniFromPars =
    , UniIsFromPars
    , UniIsFixed UniqueOrImmutable


union Type_ =
    , TypeNamed USR UniFromPars [Type]
    , TypeFn [RecycleOrSpend & Type] Type
    , TypeRecord UniqueOrImmutable (Dict Name Type)
    , TypeAnnotationVariable UniqueOrImmutable Name
    # This is used as a placeholder when there is an error and a type can't be determined
    # It's useful to avoid piling up errors (I think)
    , TypeError


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


union Argument =
    , ArgumentExpression Expression
    , ArgumentRecycle Pos Name [Name]


union Parameter =
    , ParameterPattern Pattern
    , ParameterRecycle Pos Name


union Pattern =
    , PatternAny Pos {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe Type
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]
    , PatternRecord Pos PatternCompleteness (Dict Name Pattern)


union PatternCompleteness =
    , Partial
    , Complete


alias TypeClasses = {
    , allowFunctions as Bool
    , allowUniques as Bool
    }


alias ValueDef = {
    , pattern as Pattern

    # TODO: have maybeBody instead of native?
    , native as Bool
    , body as Expression

    , tyvars as Dict Name TypeClasses

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

alias AliasDef = {
    , usr as USR
    , pars as [At Name]
    , type as Type
    , directTypeDeps as TypeDeps
    }


alias UnionDef = {
    , usr as USR
    , pars as [At Name]
    , constructors as Dict Name Constructor
    , directTypeDeps as TypeDeps
    }


alias Constructor = {
    , pos as Pos

    # type and args are redundant
    , typeUsr as USR
    , type as Type

    , pars as [Type]
    }


alias Module = {
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
#
#

skipLetIns as CA.Expression: CA.Expression =
    expr:
    try expr as
        LetIn def e: skipLetIns e
        _: expr


makeUnique as CA.Type: CA.Type =
    (CA.Type pos _ type_):
    CA.Type pos Uni type_



#
# helpers
#


unmod as [mod & param]: [param] =
    List.map Tuple.second

mapmod as (a: b): [mod & a]: [mod & b] =
    f:
    List.map (Tuple.mapSecond f)


typeTyvars as Type: Dict Name Pos =
    (Type pos _ ty_):

    fromList as [Type]: Dict Name Pos =
        list:
        List.for list (item: acc: Dict.join acc (typeTyvars item)) Dict.empty

    try ty_ as
        TypeNamed _ args: fromList args
        TypeFn pars to: fromList << to :: unmod pars
        TypeRecord attrs: fromList (Dict.values attrs)
        TypeAnnotationVariable name: Dict.singleton name pos
        TypeError: Dict.empty


patternPos as Pattern: Pos =
    pa:
    try pa as
        PatternAny p _: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p _ _: p
        PatternRecord p _ _: p


patternContainsUnique as Pattern: Bool =
    pattern:
    try pattern as
        PatternAny _ { isUnique, maybeName = _, maybeAnnotation = _ }: isUnique
        PatternLiteralText _ _: False
        PatternLiteralNumber _ _: False
        PatternConstructor _ _ args: List.any patternContainsUnique args
        PatternRecord _ _ attrs: Dict.any (k: patternContainsUnique) attrs


patternTyvars as Pattern: Dict Name Pos =
    pa:
    try pa as
        PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Just t }: typeTyvars t
        PatternAny _ { isUnique = _, maybeName = _, maybeAnnotation = Nothing }: Dict.empty
        PatternLiteralText _ _: Dict.empty
        PatternLiteralNumber _ _: Dict.empty
        PatternConstructor _ _ args: List.for args (arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty
        PatternRecord _ _ attrs: Dict.for attrs (k: arg: acc: Dict.join acc (patternTyvars arg)) Dict.empty


patternNames as Pattern: Dict Name { pos as Pos, isUnique as Bool, maybeAnnotation as Maybe Type } =
    p:
    try p as
        PatternAny pos { isUnique = _, maybeName = Nothing, maybeAnnotation = _ }: Dict.empty
        PatternAny pos { isUnique, maybeName = Just n, maybeAnnotation }: Dict.singleton n { pos, isUnique, maybeAnnotation }
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

