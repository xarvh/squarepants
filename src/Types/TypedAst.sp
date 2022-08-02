
alias TyvarId =
    Int


union RawType =
    , TypeExact USR [RawType]
    , TypeFn [ParType] FullType
    , TypeVar TyvarId
    , TypeRecord (Maybe TyvarId) (Dict Name RawType)
    , TypeError


union ParType =
    , ParRe RawType
    , ParSp FullType


# Aliases (and opaques!) can't be FullType, they must be RawType!
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
        , valueType as FullType
        , patternsAndExpressions as [Pattern & Expression]
        }
    , DestroyIn Name Expression
    , Error Pos


union Pattern =
    , PatternAny Pos
        {
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CA.RawType
        , type as FullType
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]
    , PatternRecord Pos (Dict Name (Pattern & RawType))


union Argument =
    , ArgumentExpression FullType Expression
    , ArgumentRecycle Pos RawType [Name] Name


union Parameter =
    , ParameterPattern FullType Pattern
    , ParameterRecycle Pos RawType Name


alias Tyvar =
    {
    #, annotatedAt as Pos
    , generalizedAt as Pos
    , generalizedFor as Ref
    , originalName as Name
    , allowFunctions as Bool
    }


alias Univar =
    {
    , originalId as UnivarId
    }


alias ValueDef =
    {
    , type as FullType
    , pattern as Pattern
    , native as Bool
    , body as Expression
    , freeTyvars as Dict TyvarId Tyvar
    , freeUnivars as Dict UnivarId Univar
    , directValueDeps as Set USR
    , isFullyAnnotated as Bool
    }


#
# Module
#
alias Substitutions =
    {
    , tyvars as Dict TyvarId RawType
    , univars as Dict UnivarId Uniqueness
    }


alias Module =
    {
    , umr as UMR
    , asText as Text
    , valueDefs as Dict CA.Pattern ValueDef
    }


initModule as Text: UMR: Module =
    asText: umr:
    {
    , umr
    , asText
    , valueDefs = Dict.empty
    }


#
# Substitutions application
#
alias SubsAsFns =
    {
    , ty as TyvarId: Maybe RawType
    , uni as UnivarId: Maybe Uniqueness
    }


resolveUni as (UnivarId: Maybe Uniqueness): Uniqueness: Uniqueness =
    uniSub: uni:

    try uni as
        Depends id:
            try uniSub id as
                Nothing: uni
                Just u: u
        _:
            uni


resolveParType as SubsAsFns: ParType: ParType =
    saf: par:

    try par as
        ParRe raw: ParRe (resolveRaw saf raw)
        ParSp full: ParSp (resolveFull saf full)


resolveFull as SubsAsFns: FullType: FullType =
    saf: ({ uni, raw }):

    {
    , uni = resolveUni saf.uni uni
    , raw = resolveRaw saf raw
    }


resolveRaw as SubsAsFns: RawType: RawType =
    saf: raw:

    rec as RawType: RawType =
        resolveRaw saf

    try raw as
        TypeVar id:
            try saf.ty id as
                Nothing: raw
                Just replacement: replacement

        TypeExact usr pars:
            TypeExact usr (List.map rec pars)

        TypeFn pars out:
            TypeFn
                (List.map (resolveParType saf) pars)
                (resolveFull saf out)

        TypeRecord Nothing attrs:
            TypeRecord Nothing (Dict.map (k: rec) attrs)

        TypeRecord (Just id) attrs:
            try saf.ty id as
                Just replacement: replacement
                Nothing: TypeRecord (Just id) (Dict.map (k: rec) attrs)

        TypeError:
            TypeError


resolveArg as SubsAsFns: Argument: Argument =
    saf: arg:
    try arg as
        ArgumentExpression full expr:
            ArgumentExpression (resolveFull saf full) (resolveExpression saf expr)

        ArgumentRecycle p raw attrPath name:
            ArgumentRecycle p (resolveRaw saf raw) attrPath name


resolvePar as SubsAsFns: Parameter: Parameter =
    saf: par:
    try par as
        ParameterPattern full pa:
            ParameterPattern (resolveFull saf full) (resolvePattern saf pa)

        ParameterRecycle p raw name:
            ParameterRecycle p (resolveRaw saf raw) name


resolveExpression as SubsAsFns: Expression: Expression =
    saf: expression:

    rec = resolveExpression saf

    try expression as
        LiteralNumber _ _: expression
        LiteralText _ _: expression
        Variable _ _: expression
        Constructor _ _: expression

        Fn p pars body:
          Fn p (List.map (resolvePar saf) pars) (rec body)

        Call p ref args:
            Call p (rec ref) (List.map (resolveArg saf) args)

        Record p maybeExt attrs:
            Record p (Maybe.map rec maybeExt) (Dict.map (k: rec) attrs)

        RecordAccess p name exp:
            RecordAccess p name (rec exp)

        LetIn def body:
            LetIn (resolveValueDef saf def) (rec body)

        If p { condition, true, false }:
            If p { condition = rec condition, true = rec true, false = rec false }

        Try p { value, valueType, patternsAndExpressions }:
            Try p
                {
                , value = rec value
                , valueType = resolveFull saf valueType
                , patternsAndExpressions = List.map (Tuple.mapBoth (resolvePattern saf) rec) patternsAndExpressions
                }

        DestroyIn n e:
            DestroyIn n (rec e)

        Error p:
            expression


resolvePattern as SubsAsFns: Pattern: Pattern =
    saf: pattern:

    try pattern as
        PatternLiteralNumber pos _:
            pattern

        PatternLiteralText pos _:
            pattern

        PatternAny pos stuff:
            PatternAny pos { stuff with type = resolveFull saf .type }

        PatternConstructor pos usr ps:
            PatternConstructor pos usr (List.map (resolvePattern saf) ps)

        PatternRecord pos ps:
            PatternRecord pos (Dict.map (k: (p & t): resolvePattern saf p & resolveRaw saf t) ps)


resolveValueDef as SubsAsFns: ValueDef: ValueDef =
    saf: def:

    { def with
    , type = resolveFull saf .type
    , pattern = resolvePattern saf .pattern
    , body = resolveExpression saf .body
    # TODO?, freeTyvars
    # TODO?, freeUnivars
    }



#
# helpers
#
toRaw as ParType: RawType =
    par:
    try par as
        ParRe raw: raw
        ParSp full: full.raw


mapPars as (RawType: RawType): [ParType]: [ParType] =
    f: pars:

    pars >> List.map par:
        try par as
            ParRe raw: ParRe (f raw)
            ParSp full: ParSp { full with raw = f .raw }


patternNames as Pattern: Dict Name { pos as Pos, type as FullType } =
    p:
    try p as
        PatternAny pos { maybeName = Nothing, maybeAnnotation = _, type = _ }: Dict.empty
        PatternAny pos { maybeName = Just n, maybeAnnotation = _, type }: Dict.singleton n { pos, type }
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos usr ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: (pa & ty): pa >> patternNames >> Dict.join) Dict.empty


typeTyvars as RawType: Dict TyvarId None =
    type:
    try type as
        TypeExact usr args: Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeVar id: Dict.singleton id None
        TypeRecord Nothing attrs: Dict.empty >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeRecord (Just id) attrs: Dict.singleton id None >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeError: Dict.empty
        TypeFn ins out:
            typeTyvars out.raw
            >> List.for ins in: Dict.join (in >> toRaw >> typeTyvars)


typeAllowsFunctions as (TyvarId: Bool): RawType: Bool =
    testId: type:
    try type as
        TypeFn ins out: True
        TypeVar id: testId id
        TypeExact usr args: List.any (typeAllowsFunctions testId) args
        TypeRecord _ attrs: Dict.any (k: typeAllowsFunctions testId) attrs
        TypeError: True

