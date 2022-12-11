
alias UnificationVariableId =
    Int


union Type =
    , TypeExact UniqueOrImmutable USR [Type]
    , TypeFn UniqueOrImmutable [RecycleOrSpend & Type] Type
    , TypeRecord UniqueOrImmutable (Dict Name Type)
    , TypeUnificationVariable (Maybe UniqueOrImmutable) UnificationVariableId
    , TypeRecordExt UniqueOrImmutable UnificationVariableId (Dict Name Type)
    , TypeError


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
    , freeTyvars as Dict UnificationVariableId Tyvar
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
    , substitutions as Dict UnificationVariableId Type
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
patternNames as Pattern: Dict Name Pos =
    p:
    try p as
        PatternAny pos { isUnique = _, maybeName = Nothing, maybeAnnotation = _, type = _ }: Dict.empty
        PatternAny pos { isUnique = _, maybeName = Just n, maybeAnnotation = _, type = _ }: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.for ps (x: x >> patternNames >> Dict.join) Dict.empty
        PatternRecord pos ps: Dict.for ps (k: (pa & ty): pa >> patternNames >> Dict.join) Dict.empty


typeTyvars as Type: Dict UnificationVariableId None =
    type:
    try type as
        TypeExact _ usr args: Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeFn _ ins out: typeTyvars out >> List.for ins (_ & in): Dict.join (typeTyvars in)
        TypeRecord _ attrs: Dict.empty >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeUnificationVariable _ id: Dict.singleton id None
        TypeRecordExt _ id attrs: Dict.singleton id None >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        TypeError: Dict.empty


typeAllowsFunctions as Type: Bool =
    type:
    try type as
        TypeFn _ ins out: True
        TypeUnificationVariable _ id: True
        TypeExact _ usr args: List.any typeAllowsFunctions args
        TypeRecord _ attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeRecordExt _ id attrs: Dict.any (k: typeAllowsFunctions) attrs
        TypeError: True


setUni as UniqueOrImmutable: Type: Type =
    uni: type:
    try type as
        TypeFn _ ins out:
            TypeFn uni ins out

        TypeUnificationVariable _ id:
            TypeUnificationVariable (Just uni) id

        TypeExact _ usr args:
            TypeExact uni usr args

        TypeRecord _ attrs:
            TypeRecord uni attrs

        TypeRecordExt _ id attrs:
            TypeRecordExt uni id attrs

        TypeError:
            TypeError

getUni as Type: Maybe UniqueOrImmutable =
    type:
    try type as
        TypeFn uni ins out: Just uni
        TypeUnificationVariable maybeUni id: maybeUni
        TypeExact uni usr args: Just uni
        TypeRecord uni attrs: Just uni
        TypeRecordExt uni id attrs: Just uni
        TypeError: Nothing
