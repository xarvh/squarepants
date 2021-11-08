module Types.CanonicalAst exposing (..)

{-| Canonical AST is meant for

  - Type inference
  - Optimization
  - Emission or interpretation

-}

import Dict exposing (Dict)
import StateMonad as M exposing (M, do, return)
import Types.Literal


type alias AllDefs =
    Dict String RootDef


{-| The position of a piece of code
-}
type Pos
    = -- actual position: module reference, start, end
      P String Int Int
    | -- stripped
      S
    | -- defined natively, usually in Core or Prelude
      N
    | -- defined as test
      T
    | -- inferred
      I Int
    | -- Something Went Wrong
      SWW
      {-
         TODO the following ones need to be removed
      -}
    | -- error todo
      E
    | -- Formattable to canonical todo
      F
    | -- ScopeCheck HACK
      G
      -- Union
    | U


range : Pos -> Pos -> Pos
range a b =
    case ( a, b ) of
        ( P ma sa ea, P mb sb eb ) ->
            if ma /= mb then
                SWW

            else
                P ma (min sa sb) (max ea eb)

        ( P _ _ _, _ ) ->
            a

        _ ->
            b



----
--- Root
--
--
-- TODO allow alias/union definitions within a block?
--


type RootDef
    = Union UnionDef
    | Alias AliasDef
    | Value RootValueDef


type alias AliasDef =
    { name : String
    , args : List String
    , ty : Type
    }


type alias UnionDef =
    -- TODO add pos?
    -- TODO add position for each constructor!
    -- TODO remove name and add resolvedName, userName
    { name : String
    , args : List String
    , constructors : Dict String (List Type)
    }


type alias RootValueDef =
    { name : String
    , localName : String
    , pos : Pos
    , maybeAnnotation : Maybe Annotation
    , isNative : Bool
    , body : List Statement
    }


type alias Annotation =
    { pos : Pos
    , ty : Type
    , nonFn : Dict String Pos
    }


type alias LocalValueDef =
    { pattern : Pattern
    , defsPath : List String
    , mutable : Bool
    , maybeAnnotation : Maybe Annotation
    , body : List Statement
    }


rootToLocalDef : RootValueDef -> LocalValueDef
rootToLocalDef r =
    { pattern = PatternAny r.pos r.name
    , defsPath = [ r.name ]
    , mutable = False
    , maybeAnnotation = r.maybeAnnotation
    , body = r.body
    }



----
--- Type
--


type Type
    = TypeConstant Pos String (List Type)
    | TypeVariable Pos String
    | TypeFunction Pos Type Bool Type
    | TypeRecord Pos (Maybe String) (Dict String Type)
    | TypeAlias Pos String Type


typePos : Type -> Pos
typePos ty =
    case ty of
        TypeConstant p _ _ ->
            p

        TypeVariable p _ ->
            p

        TypeFunction p _ _ _ ->
            p

        TypeRecord p _ _ ->
            p

        TypeAlias p _ _ ->
            p


{-| TODO do I need the Pos?
-}
type
    RejectFunction
    -- set by user
    = Us Pos
      -- parameter is mutable
    | Pa Pos



----
--- Expressions
--


type Statement
    = Definition LocalValueDef
      -- Evaluations are needed for return, mutation and debug
    | Evaluation Expression


statementPos : Statement -> Pos
statementPos stat =
    case stat of
        Definition def ->
            case List.reverse def.body of
                [] ->
                    patternPos def.pattern

                last :: _ ->
                    range (patternPos def.pattern) (statementPos last)

        Evaluation expr ->
            expressionPos expr


type Expression
    = Literal Pos Types.Literal.Value
    | Variable Pos VariableArgs
    | Lambda Pos Parameter (List Statement)
    | Record Pos (Maybe VariableArgs) (Dict String Expression)
    | Call Pos Expression Argument
    | If
        Pos
        -- we use the if also to get lazy ops and compacted compops, so even if the syntax does
        -- not support statement blocks inside if condition, it's useful that the AST can model it.
        { condition : List Statement
        , true : List Statement
        , false : List Statement
        }
    | Try Pos Expression (List ( Pattern, List Statement ))


expressionPos : Expression -> Pos
expressionPos e =
    case e of
        Literal pos _ ->
            pos

        Variable pos _ ->
            pos

        Lambda pos _ _ ->
            pos

        Record pos _ _ ->
            pos

        Call pos _ _ ->
            pos

        If pos _ ->
            pos

        Try pos _ _ ->
            pos


type Parameter
    = ParameterPattern Pattern
    | ParameterMutable Pos String


type Argument
    = ArgumentExpression Expression
    | ArgumentMutable Pos VariableArgs


argumentPos : Argument -> Pos
argumentPos arg =
    case arg of
        ArgumentExpression e ->
            expressionPos e

        ArgumentMutable pos _ ->
            pos



{- TODO
   union SymbolReference symbolName =
     #`value`
     #    -> locale
     #    -> globale in metafile
     , Internal symbolName
     , Root ModuleName symbolName
     , Global ModuleName symbolName

     #`Module.value`
     #    -> direct
     #    -> module alias
     , Direct ModuleName symbolName
     , Aliased ModuleAlias ModuleName symbolName
-}


type alias VariableArgs =
    { name : String
    , attrPath : List String

    -- True => declared in the root scope
    -- False => declared inside a function's scope
    , isRoot : Bool
    }



----
--- Pattern
--


type Pattern
    = PatternDiscard Pos
    | PatternAny Pos String
    | PatternLiteral Pos Types.Literal.Value
    | PatternConstructor Pos String (List Pattern)
    | PatternRecord Pos (Dict String Pattern)


patternNames : Pattern -> Dict String Pos
patternNames p =
    case p of
        PatternDiscard pos ->
            Dict.empty

        PatternAny pos n ->
            Dict.singleton n pos

        PatternLiteral pos _ ->
            Dict.empty

        PatternConstructor pos path ps ->
            List.foldl (patternNames >> Dict.union) Dict.empty ps

        PatternRecord pos ps ->
            Dict.foldl (\k -> patternNames >> Dict.union) Dict.empty ps


patternPos : Pattern -> Pos
patternPos pa =
    case pa of
        PatternDiscard p ->
            p

        PatternAny p n ->
            p

        PatternLiteral p _ ->
            p

        PatternConstructor p path ps ->
            p

        PatternRecord p ps ->
            p



----
--- RootDef splitters
--


asValue : RootDef -> Maybe RootValueDef
asValue r =
    case r of
        Value v ->
            Just v

        _ ->
            Nothing


asAlias : RootDef -> Maybe AliasDef
asAlias r =
    case r of
        Alias a ->
            Just a

        _ ->
            Nothing


asUnion : RootDef -> Maybe UnionDef
asUnion r =
    case r of
        Union u ->
            Just u

        _ ->
            Nothing


split : AllDefs -> ( Dict String AliasDef, Dict String UnionDef, Dict String RootValueDef )
split =
    let
        part3 n rootDef ( als, uns, vals ) =
            case rootDef of
                Alias a ->
                    ( Dict.insert n a als, uns, vals )

                Union u ->
                    ( als, Dict.insert n u uns, vals )

                Value v ->
                    ( als, uns, Dict.insert n v vals )
    in
    Dict.foldl part3 ( Dict.empty, Dict.empty, Dict.empty )


findValue : String -> AllDefs -> Maybe RootValueDef
findValue name mod =
    case Dict.get name mod of
        Just (Value def) ->
            Just def

        _ ->
            Nothing



----
--- Crawler
--


type PosMap
    = PosMap_Expr Expression
    | PosMap_Type Type
    | PosMap_Pattern Pattern
    | PosMap_MutParam String
    | PosMap_RootValueDef RootValueDef
    | PosMap_MutableArg VariableArgs
    | PosMap_Annotation Annotation
    | PosMap_NonFunction String


posMap_module : (PosMap -> Pos -> M acc Pos) -> AllDefs -> M acc AllDefs
posMap_module f a_defs =
    let
        fold name a_rootDef =
            case a_rootDef of
                Alias a_aliasDef ->
                    do (posMap_aliasDef f a_aliasDef) <| \b ->
                    return <| Alias b

                Union a_unionDef ->
                    do (posMap_unionDef f a_unionDef) <| \b ->
                    return <| Union b

                Value a_valueDef ->
                    do (posMap_rootValueDef f a_valueDef) <| \b ->
                    return <| Value b
    in
    M.dict_map fold a_defs


posMap_rootValueDef : (PosMap -> Pos -> M acc Pos) -> RootValueDef -> M acc RootValueDef
posMap_rootValueDef f def =
    do (posMap_block f def.body) <| \b_body ->
    do (posMap_annotation f def.maybeAnnotation) <| \b_ann ->
    do (f (PosMap_RootValueDef def) def.pos) <| \b_pos ->
    return
        { name = def.name
        , localName = def.localName
        , pos = b_pos
        , isNative = def.isNative
        , maybeAnnotation = b_ann
        , body = b_body
        }


posMap_annotation : (PosMap -> Pos -> M acc Pos) -> Maybe Annotation -> M acc (Maybe Annotation)
posMap_annotation f maybeAnnotation =
    case maybeAnnotation of
        Nothing ->
            return Nothing

        Just ann ->
            do (f (PosMap_Annotation ann) ann.pos) <| \pos ->
            do (posMap_type f ann.ty) <| \ty ->
            do (M.dict_map (\name -> f (PosMap_NonFunction name)) ann.nonFn) <| \nf ->
            { pos = pos
            , ty = ty
            , nonFn = nf
            }
                |> Just
                |> return


posMap_aliasDef : (PosMap -> Pos -> M acc Pos) -> AliasDef -> M acc AliasDef
posMap_aliasDef f def =
    do (posMap_type f def.ty) <| \b_ty ->
    return
        { name = def.name
        , args = def.args
        , ty = b_ty
        }


posMap_unionDef : (PosMap -> Pos -> M acc Pos) -> UnionDef -> M acc UnionDef
posMap_unionDef f def =
    do (M.dict_map (\k -> M.list_map (posMap_type f)) def.constructors) <| \cons ->
    return
        { name = def.name
        , args = def.args
        , constructors = cons
        }


posMap_valueDef : (PosMap -> Pos -> M acc Pos) -> LocalValueDef -> M acc LocalValueDef
posMap_valueDef f def =
    do (posMap_block f def.body) <| \b_body ->
    do (posMap_annotation f def.maybeAnnotation) <| \b_ann ->
    do (posMap_pattern f def.pattern) <| \b_pattern ->
    return
        { pattern = b_pattern
        , mutable = def.mutable
        , defsPath = def.defsPath
        , maybeAnnotation = b_ann
        , body = b_body
        }


posMap_type : (PosMap -> Pos -> M acc Pos) -> Type -> M acc Type
posMap_type f ty =
    let
        fty =
            f (PosMap_Type ty)
    in
    case ty of
        TypeConstant a_pos name a_args ->
            do (fty a_pos) <| \b_pos ->
            do (M.list_map (posMap_type f) a_args) <| \b_args ->
            return <| TypeConstant b_pos name b_args

        TypeVariable a_pos name ->
            do (fty a_pos) <| \b_pos ->
            return <| TypeVariable b_pos name

        TypeFunction a_pos a_from fromIsMut a_to ->
            do (fty a_pos) <| \b_pos ->
            do (posMap_type f a_from) <| \b_from ->
            do (posMap_type f a_to) <| \b_to ->
            return <| TypeFunction b_pos b_from fromIsMut b_to

        TypeRecord a_pos ext a_attrs ->
            do (fty a_pos) <| \b_pos ->
            do (M.dict_map (\k -> posMap_type f) a_attrs) <| \b_attrs ->
            return <| TypeRecord b_pos ext b_attrs

        TypeAlias a_pos name a_ty ->
            do (fty a_pos) <| \b_pos ->
            do (posMap_type f a_ty) <| \b_ty ->
            return <| TypeAlias b_pos name b_ty


posMap_expression : (PosMap -> Pos -> M acc Pos) -> Expression -> M acc Expression
posMap_expression fFold expr =
    let
        f =
            PosMap_Expr >> fFold
    in
    case expr of
        Literal a_pos ar ->
            do (f expr a_pos) <| \b_pos ->
            return <| Literal b_pos ar

        Variable a_pos args ->
            do (f expr a_pos) <| \b_pos ->
            return <| Variable b_pos args

        Lambda a_pos a_param a_body ->
            do (f expr a_pos) <| \b_pos ->
            do (posMap_block fFold a_body) <| \b_body ->
            do
                (case a_param of
                    ParameterMutable pos name ->
                        do (fFold (PosMap_MutParam name) pos) <| \p ->
                        return <| ParameterMutable p name

                    ParameterPattern a_pattern ->
                        do (posMap_pattern fFold a_pattern) <| \pa ->
                        return <| ParameterPattern pa
                )
            <| \b_param ->
            return <| Lambda b_pos b_param b_body

        Record a_pos a_ext a_attrs ->
            do (f expr a_pos) <| \b_pos ->
            do (M.dict_map (\k -> posMap_expression fFold) a_attrs) <| \b_attrs ->
            return <| Record b_pos a_ext b_attrs

        Call a_pos a_ref a_arg ->
            do (f expr a_pos) <| \b_pos ->
            do (posMap_expression fFold a_ref) <| \b_ref ->
            do (posMap_argument fFold a_arg) <| \b_arg ->
            return <| Call b_pos b_ref b_arg

        If a_pos ar ->
            do (f expr a_pos) <| \b_pos ->
            do (posMap_block fFold ar.condition) <| \b_cond ->
            do (posMap_block fFold ar.true) <| \b_true ->
            do (posMap_block fFold ar.false) <| \b_false ->
            return <| If b_pos { condition = b_cond, true = b_true, false = b_false }

        Try a_pos a_value a_tries ->
            do (f expr a_pos) <| \b_pos ->
            do (posMap_expression fFold a_value) <| \b_value ->
            let
                fold ( a_pattern, a_block ) =
                    do (posMap_block fFold a_block) <| \b_block ->
                    do (posMap_pattern fFold a_pattern) <| \b_pattern ->
                    return ( b_pattern, b_block )
            in
            do (M.list_map fold a_tries) <| \b_patterns ->
            return <| Try b_pos b_value b_patterns


posMap_pattern : (PosMap -> Pos -> M acc Pos) -> Pattern -> M acc Pattern
posMap_pattern fFold pattern =
    let
        f =
            PosMap_Pattern >> fFold
    in
    case pattern of
        PatternDiscard a_pos ->
            do (f pattern a_pos) <| \b_pos ->
            return <| PatternDiscard b_pos

        PatternAny a_pos name ->
            do (f pattern a_pos) <| \b_pos ->
            return <| PatternAny b_pos name

        PatternLiteral a_pos value ->
            do (f pattern a_pos) <| \b_pos ->
            return <| PatternLiteral b_pos value

        PatternConstructor a_pos name a_args ->
            do (f pattern a_pos) <| \b_pos ->
            do (M.list_map (posMap_pattern fFold) a_args) <| \b_args ->
            return <| PatternConstructor b_pos name b_args

        PatternRecord a_pos a_attrs ->
            do (f pattern a_pos) <| \b_pos ->
            do (M.dict_map (\k -> posMap_pattern fFold) a_attrs) <| \b_attrs ->
            return <| PatternRecord b_pos b_attrs


posMap_block : (PosMap -> Pos -> M acc Pos) -> List Statement -> M acc (List Statement)
posMap_block f block =
    M.list_map (posMap_statement f) block


posMap_argument : (PosMap -> Pos -> M acc Pos) -> Argument -> M acc Argument
posMap_argument f arg =
    case arg of
        ArgumentExpression expr ->
            do (posMap_expression f expr) <| \e ->
            return <| ArgumentExpression e

        ArgumentMutable pos ar ->
            do (f (PosMap_MutableArg ar) pos) <| \p ->
            return <| ArgumentMutable p ar


posMap_statement : (PosMap -> Pos -> M acc Pos) -> Statement -> M acc Statement
posMap_statement f stat =
    case stat of
        Definition ar ->
            do (posMap_valueDef f ar) <| \d ->
            return <| Definition d

        Evaluation expr ->
            do (posMap_expression f expr) <| \e ->
            return <| Evaluation e
