TyvarId =
    Int


# This reference can uniquely reference lambdas nested inside a definition
LambdaRef =
    USR & Int


var LambdaSet =
    , 'lSet [ LambdaRef ]
    , 'lVar Int
#, 'lOther ???



var RawType =
    , 'typeExact Pos USR [ RawType ]
    , 'typeFn Pos LambdaSet [ ParType ] FullType
    , 'typeVar Pos TyvarId
    , 'typeRecord Pos (Maybe TyvarId) (Dict Name RawType)
    , 'typeError


var ParType =
    , 'parRe RawType
    , 'parSp FullType


# Aliases (and opaques!) can't be FullType, they must be RawType!
FullType =
    {
    , raw as RawType
    , uni as Uniqueness
    }


var Expression =
    , 'literalNumber Pos Number
    , 'literalText Pos Text
    , 'variable Pos Ref
    , 'constructor Pos USR
    , # We have LambdaSet here so that during specialization we can replace the lambda with a constructor of that set
      'fn Pos LambdaSet LambdaRef [ Parameter ] Expression FullType
    , 'call Pos LambdaSet Expression [ Argument ]
    , # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
      'record Pos (Maybe Expression) (Dict Name Expression)
    , 'recordAccess Pos Name Expression
    , # TODO Adding the FullType here increases compile time by 10% by itself.
      # This is because there are a lot of LetIns and each requires its resolution.
      # At the same time, most of them are repeated, because nested LetIns have the same value.
      # So maybe there is a way to optimize this?
      'letIn ValueDef Expression FullType
    , 'if
          Pos
          {
          , condition as Expression
          , false as Expression
          , true as Expression
          }
    , 'try
          Pos
          {
          , patternsAndExpressions as [ Pattern & Expression ]
          , value as Expression
          , valueType as FullType
          }
    , 'destroyIn Name Expression
    , 'error Pos
    , 'introspect Self.Self


var Pattern =
    , 'patternAny
          Pos
          {
          , maybeName as Maybe Text
          , type as FullType
          }
    , 'patternLiteralText Pos Text
    , 'patternLiteralNumber Pos Number
    , 'patternConstructor Pos USR [ Pattern ]
    , 'patternRecord Pos (Dict Name (Pattern & RawType))


var Argument =
    , 'argumentExpression FullType Expression
    , 'argumentRecycle Pos RawType [ Name ] Name


var Parameter =
    , 'parameterPattern FullType Pattern
    , 'parameterRecycle Pos RawType Name
    , 'parameterPlaceholder FullType Int


Tyvar =
    {
    , maybeAnnotated as Maybe { allowFunctions as Bool, name as Name }
    }


Univar =
    {
    , annotatedId as UnivarId
    }


ValueDef =
    {
    , body as Maybe Expression
    , directDeps as CA.Deps
    , freeTyvars as Dict TyvarId Tyvar
    , freeUnivars as Dict UnivarId Univar
    , isFullyAnnotated as Bool
    , pattern as Pattern
    , type as FullType
    }


#
# Module
#
Substitutions =
    {
    , tyvars as Dict TyvarId RawType
    , univars as Dict UnivarId Uniqueness
    }


Module =
    {
    , asText as Text
    , fsPath as Text
    , umr as UMR
    , valueDefs as Dict Name ValueDef
    }


initModule as fn Text, Text, UMR: Module =
    fn fsPath, asText, umr:
    {
    , asText
    , fsPath
    , umr
    , valueDefs = Dict.empty
    }


#
# Substitutions application
#
SubsAsFns =
    {
    , ty as fn TyvarId: Maybe RawType
    , uni as fn UnivarId: Maybe Uniqueness
    }


resolveUni as fn fn UnivarId: Maybe Uniqueness, Uniqueness: Uniqueness =
    fn uniSub, uni:
    try uni as

        'depends id:
            try uniSub id as
                'nothing: uni
                'just u: u

        _:
            uni


resolveParType as fn SubsAsFns, ParType: ParType =
    fn saf, par:
    try par as
        'parRe raw: 'parRe (resolveRaw saf raw)
        'parSp full: 'parSp (resolveFull saf full)


resolveFull as fn SubsAsFns, FullType: FullType =
    fn saf, { raw, uni }:
    {
    , raw = resolveRaw saf raw
    , uni = resolveUni saf.uni uni
    }


resolveRaw as fn SubsAsFns, RawType: RawType =
    fn saf, raw:
    rec as fn RawType: RawType =
        resolveRaw saf __

    try raw as

        'typeVar _ id:
            try saf.ty id as
                'nothing: raw
                'just replacement: replacement

        'typeExact p usr pars:
            'typeExact p usr (List.map rec pars)

        'typeFn p instances pars out:
            'typeFn p instances (List.map (resolveParType saf __) pars) (resolveFull saf out)

        'typeRecord p maybeId attrs0:
            attrs1 =
                Dict.map (fn k, v: rec v) attrs0

            try maybeId as

                'nothing:
                    'typeRecord p 'nothing attrs1

                'just id:
                    try saf.ty id as

                        'nothing:
                            'typeRecord p ('just id) attrs1

                        'just ('typeRecord _ maybeNewId newAttrs):
                            # TODO not sure joining the attrs is the correct thing to do
                            'typeRecord p maybeNewId (Dict.join newAttrs attrs1)

                        'just ('typeVar _ newId):
                            'typeRecord p ('just newId) attrs1

                        _:
                            'typeError

        'typeError:
            'typeError


resolveArg as fn SubsAsFns, Argument: Argument =
    fn saf, arg:
    try arg as
        'argumentExpression full expr: 'argumentExpression (resolveFull saf full) (resolveExpression saf expr)
        'argumentRecycle p raw attrPath name: 'argumentRecycle p (resolveRaw saf raw) attrPath name


resolvePar as fn SubsAsFns, Parameter: Parameter =
    fn saf, par:
    try par as
        'parameterPattern full pa: 'parameterPattern (resolveFull saf full) (resolvePattern saf pa)
        'parameterPlaceholder full n: 'parameterPlaceholder (resolveFull saf full) n
        'parameterRecycle p raw name: 'parameterRecycle p (resolveRaw saf raw) name


resolveExpression as fn SubsAsFns, Expression: Expression =
    fn saf, expression:
    rec =
        resolveExpression saf __

    try expression as

        'literalNumber _ _:
            expression

        'literalText _ _:
            expression

        'variable _ _:
            expression

        'constructor _ _:
            expression

        'fn p lSet lambdaRef pars body bodyType:
            'fn p lSet lambdaRef (List.map (resolvePar saf __) pars) (rec body) (resolveFull saf bodyType)

        'call p lSet ref args:
            'call p lSet (rec ref) (List.map (resolveArg saf __) args)

        'record p maybeExt attrs:
            'record p (Maybe.map rec maybeExt) (Dict.map (fn k, v: rec v) attrs)

        'recordAccess p name exp:
            'recordAccess p name (rec exp)

        'letIn def rest restType:
            'letIn (resolveValueDef saf def) (rec rest) (resolveFull saf restType)

        'if p { condition, false, true }:
            'if p { condition = rec condition, false = rec false, true = rec true }

        'try p { patternsAndExpressions, value, valueType }:
            'try
                p
                {
                , patternsAndExpressions = List.map (Tuple.mapBoth (resolvePattern saf __) rec __) patternsAndExpressions
                , value = rec value
                , valueType = resolveFull saf valueType
                }

        'destroyIn n e:
            'destroyIn n (rec e)

        'error p:
            expression

        'introspect _:
            expression


resolvePattern as fn SubsAsFns, Pattern: Pattern =
    fn saf, pattern:
    try pattern as
        'patternLiteralNumber pos _: pattern
        'patternLiteralText pos _: pattern
        'patternAny pos stuff: 'patternAny pos { stuff with type = resolveFull saf .type }
        'patternConstructor pos usr ps: 'patternConstructor pos usr (List.map (resolvePattern saf __) ps)
        'patternRecord pos ps: 'patternRecord pos (Dict.map (fn k, p & t: resolvePattern saf p & resolveRaw saf t) ps)


resolveValueDef as fn SubsAsFns, ValueDef: ValueDef =
    fn saf, def:
    { def with
    , body = Maybe.map (resolveExpression saf __) .body
    , pattern = resolvePattern saf .pattern
    , type = resolveFull saf .type
    }


# TODO?, freeTyvars
# TODO?, freeUnivars
#
# helpers
#
toRaw as fn ParType: RawType =
    fn par:
    try par as
        'parRe raw: raw
        'parSp full: full.raw


mapPars as fn fn RawType: RawType, [ ParType ]: [ ParType ] =
    fn f, pars:
    zzz =
        fn par:
        try par as
            'parRe raw: 'parRe (f raw)
            'parSp full: 'parSp { full with raw = f .raw }

    List.map zzz pars


patternPos as fn Pattern: Pos =
    fn p:
    try p as
        'patternAny pos _: pos
        'patternLiteralNumber pos _: pos
        'patternLiteralText pos _: pos
        'patternConstructor pos usr ps: pos
        'patternRecord pos ps: pos


patternNames as fn Pattern: Dict Name { pos as Pos, type as FullType } =
    fn p:
    try p as
        'patternAny pos { maybeName = 'nothing, type = _ }: Dict.empty
        'patternAny pos { maybeName = 'just n, type }: Dict.ofOne n { pos, type }
        'patternLiteralNumber pos _: Dict.empty
        'patternLiteralText pos _: Dict.empty
        'patternConstructor pos usr ps: List.for Dict.empty ps (fn x, a: x >> patternNames >> Dict.join __ a)
        'patternRecord pos ps: Dict.for Dict.empty ps (fn k, pa & ty, a: pa >> patternNames >> Dict.join a __)


typeTyvars as fn RawType: Dict TyvarId None =
    fn type:
    try type as
        'typeExact _ usr args: List.for Dict.empty args (fn a, acc: Dict.join (typeTyvars a) acc)
        'typeVar _ id: Dict.ofOne id 'none
        'typeRecord _ 'nothing attrs: Dict.for Dict.empty attrs (fn k, a, d: Dict.join (typeTyvars a) d)
        'typeRecord _ ('just id) attrs: Dict.ofOne id 'none >> Dict.for __ attrs (fn k, a, d: Dict.join (typeTyvars a) d)
        'typeFn _ _ ins out: typeTyvars out.raw >> List.for __ ins (fn in, a: Dict.join (in >> toRaw >> typeTyvars) a)
        'typeError: Dict.empty


typeAllowsFunctions as fn fn TyvarId: Bool, RawType: Bool =
    fn testId, type:
    try type as
        'typeFn _ _ ins out: 'true
        'typeVar _ id: testId id
        'typeExact _ usr args: List.any (typeAllowsFunctions testId __) args
        'typeRecord _ _ attrs: Dict.any (fn k, v: typeAllowsFunctions testId v) attrs
        'typeError: 'true


normalizeTyvarId as fn @Hash TyvarId TyvarId, TyvarId: TyvarId =
    fn @hash, id:
    try Hash.get @hash id as

        'just nid:
            nid

        'nothing:
            !maxId =
                0

            Hash.each @hash fn k, v:
                if v > cloneUni @maxId then
                    @maxId := cloneImm v
                else
                    'none

            nid =
                maxId + 1

            Hash.insert @hash id nid

            nid


normalizeType as fn @Hash TyvarId TyvarId, RawType: RawType =
    fn @hash, type:
    try type as
        'typeExact p usr args: 'typeExact p usr (List.map (normalizeType @hash __) args)
        'typeFn p instances pars out: 'typeFn p instances (mapPars (normalizeType @hash __) pars) { out with raw = normalizeType @hash .raw }
        'typeRecord p 'nothing attrs: 'typeRecord p 'nothing (Dict.map (fn k, v: normalizeType @hash v) attrs)
        'typeRecord p ('just id) attrs: 'typeRecord p ('just << normalizeTyvarId @hash id) (Dict.map (fn k, v: normalizeType @hash v) attrs)
        'typeVar p id: 'typeVar p (normalizeTyvarId @hash id)
        'typeError: 'typeError


stripTypePos as fn RawType: RawType =
    fn raw:
    rec as fn RawType: RawType =
        stripTypePos

    pos =
        Pos.'t

    try raw as
        'typeVar _ id: 'typeVar pos id
        'typeExact _ usr pars: 'typeExact pos usr (List.map rec pars)
        'typeFn _ instances pars out: 'typeFn pos instances (mapPars rec pars) { out with raw = rec .raw }
        'typeRecord _ maybeId attrs0: 'typeRecord pos maybeId (Dict.map (fn k, v: rec v) attrs0)
        'typeError: 'typeError
