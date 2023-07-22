
alias TyvarId =
    Int


union RawType =
    , TypeOpaque USR [RawType]
    , TypeFn [ParType] FullType
    , TypeVar TyvarId
    # TODO Rename to TypeVariant
    , TypeUnion (Maybe TyvarId) (Dict Name [RawType])
    , TypeRecord (Maybe TyvarId) (Dict Name RawType)
    , TypeRecursive USR [RawType]
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
    , Constructor Pos Name [Expression]
    , Fn Pos [Parameter] Expression FullType
    , Call Pos Expression [Argument]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe Expression) (Dict Name Expression)
    , RecordAccess Pos Name Expression

    # TODO Adding the FullType here increases compile time by 10% by itself.
    # This is because there are a lot of LetIns and each requires its resolution.
    # At the same time, most of them are repeated, because nested LetIns have the same value.
    # So maybe there is a way to optimize this?
    , LetIn ValueDef Expression FullType
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
        , type as FullType
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Name [Pattern]
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
    #, generalizedAt as Pos
    #, generalizedFor as Ref
    , maybeAnnotated as Maybe { name as Name, allowFunctions as Bool }
    }


alias Univar =
    {
    , annotatedId as UnivarId
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
    , fsPath as Text
    , asText as Text
    , valueDefs as Dict CA.Pattern ValueDef
    }


initModule as fn Text, Text, UMR: Module =
    fn fsPath, asText, umr:
    {
    , umr
    , fsPath
    , asText
    , valueDefs = Dict.empty
    }


#
# Substitutions application
#
alias SubsAsFns =
    {
    , ty as fn TyvarId: Maybe RawType
    , uni as fn UnivarId: Maybe Uniqueness
    }


resolveUni as fn (fn UnivarId: Maybe Uniqueness), Uniqueness: Uniqueness =
    fn uniSub, uni:

    try uni as
        , Depends id:
            try uniSub id as
                , Nothing: uni
                , Just u: u
        , _:
            uni


resolveParType as fn SubsAsFns, ParType: ParType =
    fn saf, par:

    try par as
        , ParRe raw: ParRe (resolveRaw saf raw)
        , ParSp full: ParSp (resolveFull saf full)


resolveFull as fn SubsAsFns, FullType: FullType =
    fn saf, ({ uni, raw }):

    {
    , uni = resolveUni saf.uni uni
    , raw = resolveRaw saf raw
    }


resolveRaw as fn SubsAsFns, RawType: RawType =
    fn saf, raw:

    rec as fn RawType: RawType =
        resolveRaw saf __

    try raw as
        , TypeVar id:
            try saf.ty id as
                , Nothing: raw
                , Just replacement: replacement

        , TypeOpaque usr pars:
            TypeOpaque usr (List.map rec pars)

        , TypeFn pars out:
            TypeFn
                (List.map (resolveParType saf __) pars)
                (resolveFull saf out)

        , TypeRecord Nothing attrs:
            TypeRecord Nothing (Dict.map (fn k, v: rec v) attrs)

        , TypeRecord (Just id) attrs:
            try saf.ty id as
                , Just replacement: replacement
                , Nothing: TypeRecord (Just id) (Dict.map (fn k, v: rec v) attrs)

        , TypeUnion Nothing attrs:
            TypeUnion Nothing (Dict.map (fn k, v: List.map rec v) attrs)

        , TypeUnion (Just id) attrs:
            try saf.ty id as
                , Just replacement: replacement
                , Nothing: TypeUnion (Just id) (Dict.map (fn k, v: List.map rec v) attrs)

        , TypeRecursive usr args:
            TypeRecursive usr (List.map rec args)

        , TypeError:
            TypeError


resolveArg as fn SubsAsFns, Argument: Argument =
    fn saf, arg:
    try arg as
        , ArgumentExpression full expr:
            ArgumentExpression (resolveFull saf full) (resolveExpression saf expr)

        , ArgumentRecycle p raw attrPath name:
            ArgumentRecycle p (resolveRaw saf raw) attrPath name


resolvePar as fn SubsAsFns, Parameter: Parameter =
    fn saf, par:
    try par as
        , ParameterPattern full pa:
            ParameterPattern (resolveFull saf full) (resolvePattern saf pa)

        , ParameterRecycle p raw name:
            ParameterRecycle p (resolveRaw saf raw) name


resolveExpression as fn SubsAsFns, Expression: Expression =
    fn saf, expression:

    rec = resolveExpression saf __

    try expression as
        , LiteralNumber _ _: expression
        , LiteralText _ _: expression
        , Variable _ _: expression

        , Constructor p name args:
            Constructor p name (List.map (resolveExpression saf __) args)

        , Fn p pars body bodyType:
            Fn p (List.map (resolvePar saf __) pars) (rec body) (resolveFull saf bodyType)

        , Call p ref args:
            Call p (rec ref) (List.map (resolveArg saf __) args)

        , Record p maybeExt attrs:
            Record p (Maybe.map rec maybeExt) (Dict.map (fn k, v: rec v) attrs)

        , RecordAccess p name exp:
            RecordAccess p name (rec exp)

        , LetIn def rest restType:
            LetIn (resolveValueDef saf def) (rec rest) (resolveFull saf restType)

        , If p { condition, true, false }:
            If p { condition = rec condition, true = rec true, false = rec false }

        , Try p { value, valueType, patternsAndExpressions }:
            Try p
                {
                , value = rec value
                , valueType = resolveFull saf valueType
                , patternsAndExpressions = List.map (Tuple.mapBoth (resolvePattern saf __) rec __) patternsAndExpressions
                }

        , DestroyIn n e:
            DestroyIn n (rec e)

        , Error p:
            expression


resolvePattern as fn SubsAsFns, Pattern: Pattern =
    fn saf, pattern:

    try pattern as
        , PatternLiteralNumber pos _:
            pattern

        , PatternLiteralText pos _:
            pattern

        , PatternAny pos stuff:
            PatternAny pos { stuff with type = resolveFull saf .type }

        , PatternConstructor pos usr ps:
            PatternConstructor pos usr (List.map (resolvePattern saf __) ps)

        , PatternRecord pos ps:
            PatternRecord pos (Dict.map (fn k, (p & t): resolvePattern saf p & resolveRaw saf t) ps)


resolveValueDef as fn SubsAsFns, ValueDef: ValueDef =
    fn saf, def:

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
toRaw as fn ParType: RawType =
    fn par:
    try par as
        , ParRe raw: raw
        , ParSp full: full.raw


mapPars as fn (fn RawType: RawType), [ParType]: [ParType] =
    fn f, pars:

    zzz =
        fn par:
        try par as
            , ParRe raw: ParRe (f raw)
            , ParSp full: ParSp { full with raw = f .raw }

    List.map zzz pars

patternNames as fn Pattern: Dict Name { pos as Pos, type as FullType } =
    fn p:
    try p as
        , PatternAny pos { maybeName = Nothing, type = _ }: Dict.empty
        , PatternAny pos { maybeName = Just n, type }: Dict.ofOne n { pos, type }
        , PatternLiteralNumber pos _: Dict.empty
        , PatternLiteralText pos _: Dict.empty
        , PatternConstructor pos usr ps: List.for Dict.empty ps (fn x, a: x >> patternNames >> Dict.join __ a)
        , PatternRecord pos ps: Dict.for Dict.empty ps (fn k, (pa & ty), a: pa >> patternNames >> Dict.join a __)


typeTyvars as fn RawType: Dict TyvarId None =
    fn type:

    addConsTyvars =
        Dict.for __ __ fn consName, consArgs, acc0:
            List.for acc0 consArgs fn argType, acc1:
                Dict.join (typeTyvars argType) acc1

    try type as
        , TypeOpaque usr args: List.for Dict.empty args (fn a, acc: Dict.join (typeTyvars a) acc)
        , TypeVar id: Dict.ofOne id None
        , TypeRecord Nothing attrs: Dict.for Dict.empty attrs (fn k, a, d: Dict.join (typeTyvars a) d)
        , TypeRecord (Just id) attrs: Dict.ofOne id None >> Dict.for __ attrs (fn k, a, d: Dict.join (typeTyvars a) d)
        , TypeUnion Nothing consByName: addConsTyvars Dict.empty consByName
        , TypeUnion (Just id) consByName: addConsTyvars (Dict.ofOne id None) consByName
        , TypeRecursive usr args: Dict.empty # It's recursive, so we can assume it was done already?
        , TypeError: Dict.empty
        , TypeFn ins out:
            typeTyvars out.raw
            >> List.for __ ins fn in, a: Dict.join (in >> toRaw >> typeTyvars) a


typeAllowsFunctions as fn (fn TyvarId: Bool), RawType: Bool =
    fn testId, type:
    try type as
        , TypeFn ins out: True
        , TypeVar id: testId id
        , TypeOpaque usr args: List.any (typeAllowsFunctions testId __) args
        , TypeUnion _ consByName: Dict.any (fn k, v: List.any (typeAllowsFunctions testId __) v) consByName
        , TypeRecord _ attrs: Dict.any (fn k, v: typeAllowsFunctions testId v) attrs
        , TypeRecursive _ _: False # It's recursive, so we can assume it was done already?
        , TypeError: True


normalizeTyvarId as fn @Hash TyvarId TyvarId, TyvarId: TyvarId =
    fn @hash, id:

    try Hash.get @hash id as
        , Just nid: nid
        , Nothing:
          !maxId = 0
          Hash.each @hash fn k, v:
            if v > cloneUni @maxId then
                @maxId := cloneImm v
            else
                None

          nid = maxId + 1
          Hash.insert @hash id nid
          nid


normalizeType as fn @Hash TyvarId TyvarId, RawType: RawType =
    fn @hash, type:

    rec =
        normalizeType @hash __

    recId =
        normalizeTyvarId @hash __

    try type as
        , TypeOpaque usr args:
            TypeOpaque usr (List.map rec args)

        , TypeFn pars out:
            TypeFn
                (mapPars rec pars)
                { out with raw = rec .raw }

        , TypeRecord maybeId attrs:
            TypeRecord
                (Maybe.map recId maybeId)
                (Dict.map (fn k, v: rec v) attrs)

        , TypeUnion maybeId consByName:
            TypeUnion
                (Maybe.map recId maybeId)
                (Dict.map (fn k, v: List.map rec v) consByName)

        , TypeVar id:
            TypeVar (recId id)

        , TypeRecursive usr args:
            TypeRecursive usr (List.map rec args)

        , TypeError:
            TypeError

