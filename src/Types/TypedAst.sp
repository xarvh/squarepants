

alias TyvarId =
    Int


alias UnivarId =
    Int


union UniquenessType =
    , UniF   # Must be immutable
    , UniT   # Is compatible with uniqueness
    , UniOr UniquenessType UniquenessType
    , UniAnd UniquenessType UniquenessType
    , UniNot UniquenessType
    , UniVar UnivarId


union BaseType =
    , TypeExact USR [Type]
    , TypeFn [RecycleOrSpend & Type] Type
    , TypeVar TyvarId

    , TypeError
    , TypeRecord (Dict Name Type)
    , TypeRecordExt TyvarId (Dict Name Type)


union Type =
    , Type UniquenessType BaseType


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos USR
    , Fn Pos [Parameter & Type] Expression
    , Call Pos Expression [Argument & Type]
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
        , type as Type
        , patternsAndExpressions as [Pattern & Expression]
        }
    # TODO this should have a type too!
    , DestroyIn Name Expression
    , Error Pos


union Pattern =
    , PatternAny Pos
        {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CA.Type
        , type as Type
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos USR [Pattern]
    , PatternRecord Pos (Dict Name (Pattern & Type))


union Argument =
    , ArgumentExpression Expression
    , ArgumentRecycle Pos [Name] Name


union Parameter =
    , ParameterPattern Pattern
    , ParameterRecycle Pos Name


alias Tyvar = {
    , originalName as Name
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
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
    , substitutions as Dict TyvarId Type
    }


initModule as Text: UMR: Module =
    asText: umr:
    {
    , umr
    , asText
    , valueDefs = Dict.empty
    }


#
# Helpers
#
patternNames as Pattern: Dict Name Pos =
    p:
    try p as
        PatternAny pos { isUnique = _, maybeName = Nothing, maybeAnnotation = _, type = _ }: Dict.empty
        PatternAny pos { isUnique = _, maybeName = Just n, maybeAnnotation = _, type = _ }: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: (pa & ty): pa >> patternNames >> Dict.join) Dict.empty


typeTyvars as Type: Dict TyvarId None =
    (Type uniType baseType):
    try baseType as
        TypeExact _ usr args: Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeFn _ ins out: typeTyvars out >> List.for ins (_ & in): Dict.join (typeTyvars in)
        TypeRecord _ attrs: Dict.empty >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeVar _ id: Dict.singleton id None
        TypeRecordExt _ id attrs: Dict.singleton id None >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeError: Dict.empty


typeAllowsFunctions as Type: Bool =
    (Type uniType baseType):
    try baseType as
        TypeFn _ ins out: True
        TypeVar _ id: True
        TypeExact _ usr args: List.any typeAllowsFunctions args
        TypeRecord _ attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeRecordExt _ id attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeError: True


getUni as Type: UniquenessType =
    Type uni base:
    uni

