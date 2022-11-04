
# A reference to a defined variable
alias Ref = CA.Ref


alias LambdaModifier =
    CA.LambdaModifier


alias UnificationVariableId =
    Int


union Type =
    , TypeOpaque Pos Meta.UniqueSymbolReference [Type]
    , TypeAlias Pos Meta.UniqueSymbolReference [Type]
    , TypeFunction Pos Type LambdaModifier Type
    , TypeRecord Pos (Dict Name Type)
    , TypeUnique Pos Type
    , TypeUnificationVariable UnificationVariableId
    , TypeRecordExt UnificationVariableId (Dict Name Type)


union Expression =
    , LiteralNumber Pos Number
    , LiteralText Pos Text
    , Variable Pos Ref
    , Constructor Pos Meta.UniqueSymbolReference
    , Lambda Pos (Pattern) LambdaModifier (Expression)
    , Call Pos (Expression) (Argument) Type
    , CallCo Pos (Expression) [Argument & Type]
      # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
    , Record Pos (Maybe (Expression)) (Dict Name (Expression))
    , RecordAccess Pos Name (Expression)
    , LetIn ValueDef (Expression)
    , If Pos {
        , condition as (Expression)
        , true as (Expression)
        , false as (Expression)
        }
    , Try Pos {
        , value as (Expression)
        , type as Type
        , patternsAndExpressions as [Pattern & Expression]
        }
    , DestroyIn Name (Expression)


union Pattern =
    , PatternAny Pos {
        , isUnique as Bool
        , maybeName as Maybe Text
        , maybeAnnotation as Maybe CA.Type
        , type as Type
        }
    , PatternLiteralText Pos Text
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Meta.UniqueSymbolReference [Pattern]
    , PatternRecord Pos (Dict Name (Pattern & Type))


union Argument =
    , ArgumentExpression (Expression)
    , ArgumentRecycle Pos [Name] Ref


alias TypeClasses = {
    , allowFunctions as Maybe Bool
    , allowUniques as Maybe Bool
    }


alias ValueDef = {
    , pattern as Pattern
    , native as Bool
    , body as Expression
    , tyvars as Dict Name TypeClasses
    , directValueDeps as Set Meta.UniqueSymbolReference
    }




#
# Module
#

alias Constructor = {
    , pos as Pos

    # type and args are redundant
    , typeUsr as Meta.UniqueSymbolReference
    , type as Type
    , args as [Type]
    }


alias Module = {
    , umr as Meta.UniqueModuleReference
    , asText as Text
    , valueDefs as Dict CA.Pattern ValueDef
    , substitutions as Dict UnificationVariableId Type
    }


initModule as Text: Meta.UniqueModuleReference: Module =
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



#
# Stuff that should live... somewhere else?
#

alias InstanceVariable = {
    , definedAt as Pos
    # TODO: ty -> type
    , ty as Type
    , freeTypeVariables as Dict Name TypeClasses
    # TODO: isMutable -> isUnique
    , isMutable as Bool
    }


alias Globals = {
#    , types as CA.All TypeDef
    , constructors as CA.All Constructor
    , instanceVariables as ByUsr InstanceVariable
    }

