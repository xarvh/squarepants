

alias Name =
    Text


alias ModuleName =
    Text


#
# This tells us where a module comes from.
#
# It has two requirements:
#   1. It distinguishes two modules with the same name
#   2. It tells us how to actually load the module
#
# TODO: for the time being, this is just a placeholder
#
union Source =
    Source


# A reference to a definition (alias, union, constructor or value doesn't matter)
union Ref =
    # This is for stuff defined inside the current function
    , FunctionLocal Name
    # This is for stuff defined inside the current module
    , ModuleLocal Name
    # This is for stuff defined outside the current module
    , Foreign Source ModuleName Name


#
# Type
#

alias TyVarId =
    Int


# TODO I can't base this on defPath because patterns without names make it non-unique
# But probably I can use Pos?
union UniqueTyvarKey =
    UniqueTyvarKey Source ModuleName Int


union Type =
    , TypeConstant Pos Ref [Type]
    , TypeGeneratedVar TyVarId
    , TypeAnnotatedVar Pos UniqueTyvarKey Name
    , TypeFunction Pos Type Bool Type
    , TypeRecord Pos (Maybe Name) (Dict Name Type)
    , TypeAlias Pos Ref Type


typePos ty =
    as Type: Pos
    try ty as
        TypeConstant p _ _: p
        TypeVariable p _: p
        TypeFunction p _ _ _: p
        TypeRecord p _ _: p
        TypeAlias p _ _: p


#
# Pattern
#


union Pattern =
    , PatternDiscard Pos
    , PatternAny Pos Text
    , PatternLiteralText Pos Text
    # TODO: Should this be `IntLiteral`? What other Number types are allowed? Vecs?
    , PatternLiteralNumber Pos Number
    , PatternConstructor Pos Ref [Pattern]
    , PatternRecord Pos (Dict Name Pattern)


patternNames p =
    as Pattern: Dict Name Pos
    try p as
        PatternDiscard pos: Dict.empty
        PatternAny pos n: Dict.singleton n pos
        PatternLiteralNumber pos _: Dict.empty
        PatternLiteralText pos _: Dict.empty
        PatternConstructor pos path ps: List.foldl (fn x: x >> patternNames >> Dict.join) ps Dict.empty
        PatternRecord pos ps: Dict.foldl (fn k v: v >> patternNames >> Dict.join) ps Dict.empty


patternPos pa =
    as Pattern: Pos
    try pa as
        PatternDiscard p: p
        PatternAny p n: p
        PatternLiteralText p _: p
        PatternLiteralNumber p _: p
        PatternConstructor p path ps: p
        PatternRecord p ps: p


#
# Statements
#


union Statement =
    , Definition ValueDef
    # Evaluations are needed for return, mutation and debug
    , Evaluation Expression


statementPos stat =
    as Statement: Pos
    try stat as
        Definition def:
            try List.reverse def.body as
                []: patternPos def.pattern
                last :: _: Pos.range (patternPos def.pattern) (statementPos last)

        Evaluation expr:
            expressionPos expr


#
# Expressions
#


alias VariableArgs =
    {
    , ref as Ref
    , attrPath as [Name]
    }


union Parameter =
    , ParameterPattern Pattern
    , ParameterMutable Pos Name


union Argument =
    , ArgumentExpression Expression
    # TODO should we distinguish between a locally declared mutable and a mutable parameter?
    , ArgumentMutable Pos VariableArgs


argumentPos arg =
    as Argument: Pos
    try arg as
        ArgumentExpression e: expressionPos e
        ArgumentMutable pos _: pos


union Expression =
    , LiteralNumber Pos Text
    , LiteralText Pos Text
    , Variable Pos VariableArgs
    , Lambda Pos Parameter [Statement]
    , Record Pos (Maybe VariableArgs) (Dict Name Expression)
    , Call Pos Expression Argument
    , If Pos {
        # we use the if also to get lazy ops and compacted compops, so even if the syntax does
        # not support statement blocks inside if condition, it's useful that the AST can model it.
        , condition as [Statement]
        , true as [Statement]
        , false as [Statement]
        }
    , Try Pos Expression [Pattern & [Statement]]


expressionPos e =
    as Expression: Pos
    try e as
        LiteralText pos _: pos
        LiteralNumber pos _: pos
        Variable pos _: pos
        Lambda pos _ _: pos
        Record pos _ _: pos
        Call pos _ _: pos
        If pos _: pos
        Try pos _ _: pos


#
# Main definition stuff
#


alias AliasDef = {
    , name as At Name
    , args as [At Name]
    , type as Type
    }


alias UnionDef = {
    , name as At Name
    , args as [At Name]
    , constructors as Dict Name [Type]
    }


alias ValueDef = {
    , pattern as Pattern
    , native as Bool
    , mutable as Bool
    , parentDefinitions as [Name]
    , maybeAnnotation as Maybe Annotation
    , body as [Statement]
    }


alias Annotation = {
    , asPos as Pos
    , type as Type
    , nonFn as Dict Text Pos
    }


#
# Module stuff
#


union ForeignAddress =
    ForeignAddress Source ModuleName Name


alias Deps = {
    , values as Dict ForeignAddress None
    , types as Dict ForeignAddress None
    }


alias Module = {
    , source as Source
    , name as ModuleName
    , aliases as Dict Name ([#Deps &#] AliasDef)
    , unions as Dict Name ([#Deps &#] UnionDef)

    # this also contains constructors! =)
    # The pattern is used to find the definition in valueDef
    , exposedValues as Dict Name { [#deps as Deps,#] valueDefsKey as Pattern, type as Type }
    , valueDefs as Dict Pattern ([#Deps &#] ValueDef)
    , valueNames as Dict Name Pos
    }


#
# RootDef splitters
#

[#

asValue r =
    as RootDef: Maybe ValueDef
    try r as
        Value v: Just v
        _: Nothing


asAlias r =
    as RootDef: Maybe AliasDef
    try r as
        Alias a: Just a
        _: Nothing


asUnion r =
    as RootDef: Maybe UnionDef
    try r as
        Union u: Just u
        _: Nothing


split =
    as AllDefs: Dict String AliasDef & Dict String UnionDef & Dict String ValueDef

    part3 n rootDef ( als, uns, vals ) =
        case rootDef of
            Alias a:
                ( Dict.insert n a als, uns, vals )

            Union u:
                ( als, Dict.insert n u uns, vals )

            Value v:
                ( als, uns, Dict.insert n v vals )

    Dict.foldl part3 ( Dict.empty, Dict.empty, Dict.empty )


findValue name mod =
    as String: AllDefs: Maybe ValueDef
    try Dict.get name mod as
        Just (Value def): Just def
        _: Nothing

#]


#
# Crawler
#


union PosMap =
    , PosMap_ValueDef ValueDef Name
    , PosMap_AliasName Name
    , PosMap_AliasArgument Name
    , PosMap_UnionName Name
    , PosMap_UnionParam Name

    , PosMap_Type Type
    , PosMap_Expr Expression
    , PosMap_Pattern Pattern
    , PosMap_MutParam Name
    , PosMap_MutableArg VariableArgs
    , PosMap_Annotation Annotation
    , PosMap_NonFunction Name


[#
posMap_module as (PosMap: acc @: Pos: Pos): AllDefs: acc @: AllDefs
posMap_module f a_defs =
    let
        fold name a_rootDef =
            case a_rootDef of
                Alias a_aliasDef:
                    do (posMap_aliasDef f a_aliasDef) <| \b:
                    return <| Alias b

                Union a_unionDef:
                    do (posMap_unionDef f a_unionDef) <| \b:
                    return <| Union b

                Value a_valueDef:
                    do (posMap_rootValueDef f a_valueDef) <| \b:
                    return <| Value b
    in
    M.dict_map fold a_defs
#]


posMap_atName f makePosMap @acc (At pos a)=
    as (PosMap: acc @: Pos: Pos): (a: PosMap): acc @: At a: At a

    At (f (makePosMap a) @acc pos) a


posMap_valueDef f @acc def =
    as (PosMap: acc @: Pos: Pos): acc @: ValueDef: ValueDef

    { def with
    , primaryName = posMap_atName f (PosMap_ValueDef def) @acc def.primaryName
    , pattern = posMap_pattern f @acc def.pattern
    , maybeAnnotation = posMap_annotation f @acc def.maybeAnnotation
    , body = posMap_block f @acc def.body
    }


posMap_annotation f @acc maybeAnnotation =
    as (PosMap: acc @: Pos: Pos): acc @: Maybe Annotation: Maybe Annotation
    maybeAnnotation >> Maybe.map fn ann: {
        , asPos = f (PosMap_Annotation ann) @acc ann.asPos
        , ty = posMap_type f @acc ann.ty
        , nonFn = Dict.map (fn name: f (PosMap_NonFunction name) @acc) ann.nonFn
        }


posMap_aliasDef f @acc def =
    as (PosMap: acc @: Pos: Pos): acc @: AliasDef: AliasDef
    {
    , name = posMap_atName f PosMap_AliasName @acc def.name
    , args = List.map (posMap_atName f PosMap_AliasArgument @acc) def.args
    , ty = posMap_type f @acc def.ty
    }


posMap_unionDef f @acc def =
    as (PosMap: acc @: Pos: Pos): acc @: UnionDef: UnionDef
    {
    , name = posMap_atName f PosMap_UnionName @acc def.name
    , args = List.map (posMap_atName f PosMap_UnionParam @acc) def.args
    , constructors = Dict.map (fn name: List.map (posMap_type f @acc)) def.constructors
    }


posMap_type f @acc ty =
    as (PosMap: acc @: Pos: Pos): acc @: Type: Type

    fty =
        f (PosMap_Type ty) @acc

    try ty as
        TypeConstant a_pos name a_args:
          TypeConstant (fty a_pos) name (List.map (posMap_type f @acc) a_args)

        TypeVariable a_pos name:
            TypeVariable (fty a_pos) name

        TypeFunction a_pos a_from fromIsMut a_to:
            TypeFunction (fty a_pos) (posMap_type f @acc a_from) fromIsMut (posMap_type f @acc a_to)

        TypeRecord a_pos ext a_attrs:
            TypeRecord (fty a_pos) ext (Dict.map (fn k: posMap_type f @acc) a_attrs)

        TypeAlias a_pos name a_ty:
            TypeAlias (fty a_pos) name (posMap_type f @acc a_ty)




posMap_parameter f @acc param =
    as (PosMap: acc @: Pos: Pos): acc @: Parameter: Parameter
    try param as
        ParameterMutable pos name:
            ParameterMutable (f (PosMap_MutParam name) @acc pos) name

        ParameterPattern pattern:
            ParameterPattern (posMap_pattern f @acc pattern)


posMap_expression fFold @acc expr =
    as (PosMap: acc @: Pos: Pos): acc @: Expression: Expression

    f =
        fFold (PosMap_Expr expr) @acc

    try expr as
        LiteralText a_pos v:
            LiteralText (f a_pos) v

        LiteralNumber a_pos v:
            LiteralNumber (f a_pos) v

        Variable a_pos args:
            Variable (f a_pos) args

        Lambda a_pos a_param a_body:
            Lambda (f a_pos) (posMap_parameter fFold @acc a_param) (posMap_block fFold @acc a_body)

        Record a_pos a_ext a_attrs:
            Record (f a_pos) a_ext (Dict.map (fn k: posMap_expression fFold @acc) a_attrs)

        Call a_pos a_ref a_arg:
            Call (f a_pos) (posMap_expression fFold @acc a_ref) (posMap_argument fFold @acc a_arg)

        If a_pos ar:
            If (f a_pos) {
                , condition = posMap_block fFold @acc ar.condition
                , true = posMap_block fFold @acc ar.true
                , false = posMap_block fFold @acc ar.false
                }

        Try a_pos a_value a_tries:
            a_tries
                >> List.map (fn (pa & block): posMap_pattern fFold @acc pa & posMap_block fFold @acc block)
                >> Try (f a_pos) (posMap_expression fFold @acc a_value)


posMap_pattern fFold @acc pattern =
    as (PosMap: acc @: Pos: Pos): acc @: Pattern: Pattern
    todo "NI"
#    let
#        f =
#            PosMap_Pattern >> fFold
#    in
#    case pattern of
#        PatternDiscard a_pos:
#            do (f pattern a_pos) <| \b_pos:
#            return <| PatternDiscard b_pos
#
#        PatternAny a_pos name:
#            do (f pattern a_pos) <| \b_pos:
#            return <| PatternAny b_pos name
#
#        PatternLiteral a_pos value:
#            do (f pattern a_pos) <| \b_pos:
#            return <| PatternLiteral b_pos value
#
#        PatternConstructor a_pos name a_args:
#            do (f pattern a_pos) <| \b_pos:
#            do (M.list_map (posMap_pattern fFold) a_args) <| \b_args:
#            return <| PatternConstructor b_pos name b_args
#
#        PatternRecord a_pos a_attrs:
#            do (f pattern a_pos) <| \b_pos:
#            do (M.dict_map (\k: posMap_pattern fFold) a_attrs) <| \b_attrs:
#            return <| PatternRecord b_pos b_attrs


posMap_block f @acc block =
    as (PosMap: acc @: Pos: Pos): acc @: [Statement]: [Statement]
    todo ""
#    M.list_map (posMap_statement f) block


posMap_argument f @acc arg =
    as (PosMap: acc @: Pos: Pos): acc @: Argument: Argument
    todo ""
#    case arg of
#        ArgumentExpression expr:
#            do (posMap_expression f expr) <| \e:
#            return <| ArgumentExpression e
#
#        ArgumentMutable pos ar:
#            do (f (PosMap_MutableArg ar) pos) <| \p:
#            return <| ArgumentMutable p ar


posMap_statement f @acc stat =
    as (PosMap: acc @: Pos: Pos): acc @: Statement: Statement
    todo ""
#    case stat of
#        Definition ar:
#            do (posMap_valueDef f ar) <| \d:
#            return <| Definition d
#
#        Evaluation expr:
#            do (posMap_expression f expr) <| \e:
#            return <| Evaluation e
