
# A reference to a defined variable
alias Ref = CA.Ref


alias UnificationVariableId =
    Int


union Type =
    , TypeOpaque Pos USR [Type]
    , TypeAlias Pos USR [Type]
    , TypeFn Pos [Bool & Type] Type
    , TypeRecord Pos (Dict Name Type)
    , TypeUnique Pos Type
    , TypeUnificationVariable UnificationVariableId
    , TypeRecordExt UnificationVariableId (Dict Name Type)
    , TypeError Pos


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
    , ArgumentRecycle Pos [Name] Ref


union Parameter =
    , ParameterPattern Pattern
    , ParameterRecycle Pos Name


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }


alias ValueDef =
    {
    , pattern as Pattern
    , native as Bool
    , body as Expression
    , tyvars as Dict Name TypeClasses
    , directValueDeps as Set USR
    }




#
# Module
#

alias Constructor =
    {
    , pos as Pos
    , typeUsr as USR
    , type as Type
#    , args as [Type]
    }


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
        TypeOpaque pos usr args: Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeAlias pos usr args:  Dict.empty >> List.for args (a: Dict.join (typeTyvars a))
        TypeFn pos ins out: typeTyvars out >> List.for ins (_ & in): Dict.join (typeTyvars in)
        TypeRecord pos attrs: Dict.empty >> Dict.for attrs (k: a: Dict.join (typeTyvars a))
        #TODO Should we say here that the var must allow uniqueness?
        TypeUnique pos ty: typeTyvars ty
        TypeUnificationVariable id: Dict.singleton id None
        TypeRecordExt id attrs: Dict.singleton id None >> Dict.for attrs (k: a: Dict.join (typeTyvars a))


#
# Stuff that should live... somewhere else?
#

alias InstanceVariable = {
    , definedAt as Pos
    , type as Type
    , tyvars as Dict Name TypeClasses
    , isUnique as Bool
    }


alias Globals = {
    , types as ByUsr TypeDef
    , constructors as ByUsr Constructor
    , instanceVariables as ByUsr InstanceVariable
    , tyvarIdCounter as Int
    }



#
# ....?
#
union TypeDef =
    , TypeDefAlias AliasDef
    , TypeDefUnion UnionDef


alias AliasDef = {
    , usr as USR
    , pars as [At Name]
    , type as Type
    #, directTypeDeps as TypeDeps
    }


alias UnionDef = {
    , usr as USR
    , pars as [At Name]
    #, constructors as Dict Name Constructor
    #, directTypeDeps as TypeDeps
    }

