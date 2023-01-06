
alias TyvarId =
    Int


union Uniqueness =
    # We don't care to distinguish between unique and immutable
    # In theory, all expressions that work with a unique will work with an immutable, but not vice-versa
    # So we track only what MUST be immutable, and what instead could work for either.
    #
    # This is very useful because if we don't know anything about a type, then there is also nothing against
    # using it as unique, so we can set it to AllowUni.
    #
    # When we unify AllowUni and ForceImm, the result is always ForceImm
    # Then we set all non-unique declarations to ForceImm and we let the uniqueness check step actually check it
    , AllowUni
    , ForceImm


# TODO find a better naming. This is so confusing
# TODO merge it with the one in CA?
union UniFromPars =
    , UniIsFromPars
    , UniIsFixed Uniqueness





union TypeEither = TU TypeUni, TI TypeImm

union TypeUni =
    , TypeUnionUni USR [TypeUni]
    , TypeRecordUni (Maybe TyvarId) (Dict Name TypeUni)
    , TypeVarUni TyvarId

    , TypeErrorUni

union TypeImm =
    , TypeFn [RecycleOrSpend & TypeUni] TypeUni
    , TypeUnionImm USR [TypeImm]
    , TypeRecordImm (Maybe TyvarId) (Dict Name TypeImm)
    , TypeVarImm TyvarId
    , TypeErrorImm

uniToImm as TypeUni: TypeImm =
    todo "uniToImm"



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
        , type as TypeEither
        , patternsAndExpressions as [Pattern & Expression]
        }
    , DestroyIn Name Expression
    , Error Pos


union Pattern =
    , PatternAny Pos
        {
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CA.Type
        , type as TypeEither
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]

    , PatternRecord Pos (Dict Name (Pattern & TypeEither))


union Argument =
    , ArgumentExpression TypeEither Expression
    , ArgumentRecycle TypeUni Pos [Name] Name


union Parameter =
    , ParameterPattern TypeEither Pattern
    , ParameterRecycle TypeUni Pos Name


alias Tyvar = {
    , originalName as Name
    , allowFunctions as Bool
    , allowUniques as Bool
    }


alias ValueDef =
    {
    , pattern as Pattern
    , native as Bool
    , body as Expression
    , freeTyvars as Dict TyvarId Tyvar
    , directValueDeps as Set USR
    , isFullyAnnotated as Bool
    }


#
# Module
#

alias Module =
    {
    , umr as UMR
    , asText as Text
    , valueDefs as Dict CA.Pattern ValueDef
    , substitutions as Dict TyvarId TypeEither
    }


initModule as Text: UMR: Module =
    asText: umr:
    {
    , umr
    , asText
    , valueDefs = Dict.empty
    }


#
# helpers
#
patternNames as Pattern: Dict Name { pos as Pos, isUnique as Bool, type as TypeEither } =
    p:
    try p as
        PatternAny pos { isUnique = _, maybeName = Nothing, maybeAnnotation = _, type = _ }: Dict.empty
        PatternAny pos { isUnique, maybeName = Just n, maybeAnnotation = _, type }: Dict.singleton n { pos, isUnique, type }
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: (pa & ty): pa >> patternNames >> Dict.join) Dict.empty


typeTyvars as Type: Dict TyvarId None =
    type:
    try type as
        TypeExact _ usr args: Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeFn ins out: typeTyvars out >> List.for ins (_ & in): Dict.join (typeTyvars in)
        TypeRecord _ attrs: Dict.empty >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeVar _ id: Dict.singleton id None
        TypeRecordExt _ id attrs: Dict.singleton id None >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeError: Dict.empty


typeAllowsFunctions as Type: Bool =
    type:
    try type as
        TypeFn ins out: True
        TypeVar _ id: True
        TypeExact _ usr args: List.any typeAllowsFunctions args
        TypeRecord _ attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeRecordExt _ id attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeError: True


setUni as Uniqueness: Type: Type =
    uni: type:
    try type as
        TypeFn ins out:
            if uni == TA.AllowUni then
                todo "Compiler bug: functions should always be ForceImm"
            else
                type

        TypeVar _ id:
            TypeVar uni id

        TypeExact _ usr args:
            newArgs = if uni == AllowUni then args else List.map (setUni uni) args
            TypeExact (UniIsFixed uni) usr newArgs

        TypeRecord _ attrs:
            newAttrs = if uni == AllowUni then attrs else Dict.map (k: setUni uni) attrs
            TypeRecord uni newAttrs

        TypeRecordExt _ id attrs:
            newAttrs = if uni == AllowUni then attrs else Dict.map (k: setUni uni) attrs
            TypeRecordExt uni id newAttrs

        TypeError:
            TypeError


getUni as Type: Uniqueness =
    type:
    try type as
        TypeFn ins out: ForceImm
        TypeVar uni id: uni
        TypeRecord uni attrs: uni
        TypeRecordExt uni id attrs: uni
        TypeError: AllowUni
        TypeExact uniFromPars usr args:
            try uniFromPars as
                UniIsFixed uni: uni
                UniIsFromPars: if List.any (a: getUni a == AllowUni) args then AllowUni else ForceImm

