[# This AST is meant to reflect more closely the cosmetic choices of the user.

It is meant for two purposes:

1.  Transform it into canonical AST for compiling
2.  Transform it back into human-readable, nicely formatted code

It is a lot more permissive than what the actual language syntax allows, because in this way we can give more helpful error messages to the user.
Instead than errors at parse time, we can produce more meaningful errors when translating into canonical.

#]


alias Module =
    List Statement


alias ValueDef =
    {
    , pattern as Pattern
    , mutable as Bool
    , maybeAnnotation as Maybe Annotation
    , body as List Statement
    }


alias Annotation =
    {
    , pos as Pos
    , ty as Type
    , nonFn as List String
    }


union Statement =
    , Evaluation Pos Expression
    , Definition Pos ValueDef
    , TypeAlias Pos {
        , name as String
        , args as List String
        , ty as Type
        }
    , UnionDef Pos {
        , name as String
        , args as List String

        # constructors are parsed into a TypePolymorphic
        , constructors as List Type
        }


union Type =
    , TypeName Pos String
    , TypePolymorphic Pos String (List Type)
    , TypeFunction Pos Type Bool Type
    , TypeTuple Pos (List Type)
    , TypeRecord Pos (RecordArgs Type)


union Expression =
    , Literal Pos Literal.Value
    , Variable Pos { isBinop as Bool } String
    , Mutable Pos String
    , Lambda Pos (List Pattern) (List Statement)
    , FunctionCall Pos Expression (List Expression)
    , Binop Pos Op.Precedence (SepList Binop Expression)
    , Unop Pos Unop Expression
    , If Pos {
        , isOneLine as Bool
        , condition as Expression
        , true as List Statement
        , false as List Statement
        }
    , Try Pos {
        , isOneLine as Bool
        , value as Expression
        , patterns as List ( Pattern & List Statement )
        , maybeElse as Maybe (List Statement)
        }
    , Record Pos (RecordArgs Expression)
    , List Pos (List Expression)


union Pattern =
    , PatternAny Pos Bool String
    , PatternLiteral Pos Literal.Value
    , PatternApplication Pos String (List Pattern)
    , PatternList Pos (List Pattern)
    , PatternRecord Pos (RecordArgs Pattern)
    , PatternCons Pos (List Pattern)
    , PatternTuple Pos (List Pattern)


alias RecordArgs expr =
    {
    , extends as Maybe expr
    , attrs as [ String & Maybe expr ]
    }


#
# Helpers
#


patternPos pa =
    as Pattern: Pos
    try pa as
        PatternAny p _ _:
            p

        PatternLiteral p _:
            p

        PatternApplication p _ _:
            p

        PatternList p _:
            p

        PatternRecord p _:
            p

        PatternCons p _:
            p

        PatternTuple p _:
            p


[#
recordArgs_map f ar =
    as (a: b): RecordArgs a: RecordArgs b
    {
    , extends = Maybe.map f ar.extends
    , attrs = List.map (Tuple.mapSecond (Maybe.map f)) ar.attrs
    }



posMap_statement f stat =
    as (Pos: Pos): Statement: Statement
    try stat as
        Evaluation e:
            Evaluation (posMap_expression f e)

        Definition def:
            Definition
                { pattern = posMap_pattern f def.pattern
                , mutable = def.mutable
                , maybeAnnotation = Maybe.map (posMap_annotation f) def.maybeAnnotation
                , body = List.map (posMap_statement f) def.body
                , pos = f def.pos
                }

        TypeAlias ar:
            TypeAlias
                { name = ar.name
                , args = ar.args
                , ty = posMap_type f ar.ty
                }

        UnionDef ar:
            UnionDef
                { name = ar.name
                , args = ar.args
                , constructors = List.map (posMap_type f) ar.constructors
                }


posMap_annotation f ann =
    as (Pos: Pos): Annotation: Annotation
    {
    , pos = f ann.pos
    , ty = posMap_type f ann.ty
    , nonFn = ann.nonFn
    }


posMap_expression f expr =
    as (Pos: Pos): Expression: Expression
    try expr as
        Literal pos value:
            Literal (f pos) value

        Variable pos isbin name:
            Variable (f pos) isbin name

        Mutable pos name:
            Mutable (f pos) name

        Lambda pos pas stats:
            Lambda (f pos)
                (List.map (posMap_pattern f) pas)
                (List.map (posMap_statement f) stats)

        FunctionCall pos ref args:
            FunctionCall (f pos)
                (posMap_expression f ref)
                (List.map (posMap_expression f) args)

        Binop pos group sepList:
            Binop (f pos) group (SepList.mapItem (posMap_expression f) sepList)

        Unop pos name right:
            Unop (f pos) name (posMap_expression f right)

        If pos ar:
            If (f pos)
                { isOneLine = ar.isOneLine
                , condition = posMap_expression f ar.condition
                , true = List.map (posMap_statement f) ar.true
                , false = List.map (posMap_statement f) ar.false
                }

        Try pos ar:
            Try (f pos)
                { isOneLine = ar.isOneLine
                , value = posMap_expression f ar.value
                , patterns = List.map (Tuple.mapBoth (posMap_pattern f) (List.map (posMap_statement f))) ar.patterns
                , maybeElse = Maybe.map (List.map (posMap_statement f)) ar.maybeElse
                }

        Record pos ar:
            Record (f pos) (recordArgs_map (posMap_expression f) ar)

        List pos exs:
            List (f pos) (List.map (posMap_expression f) exs)


posMap_pattern f pa =
    as (Pos: Pos): Pattern: Pattern
    try pa as
        PatternAny pos mutable name:
            PatternAny (f pos) mutable name

        PatternLiteral pos val:
            PatternLiteral (f pos) val

        PatternApplication pos cons pas:
            PatternApplication (f pos) cons (List.map (posMap_pattern f) pas)

        PatternList pos pas:
            PatternList (f pos) (List.map (posMap_pattern f) pas)

        PatternRecord pos ar:
            PatternRecord (f pos) (recordArgs_map (posMap_pattern f) ar)

        PatternCons pos pas:
            PatternCons (f pos) (List.map (posMap_pattern f) pas)

        PatternTuple pos pas:
            PatternTuple (f pos) (List.map (posMap_pattern f) pas)


posMap_type f ty =
    as (Pos: Pos): Type: Type
    try ty as
        TypeName pos name:
            TypeName (f pos) name

        TypePolymorphic pos name args:
            TypePolymorphic (f pos) name (List.map (posMap_type f) args)

        TypeFunction pos from mut to:
            TypeFunction (f pos) (posMap_type f from) mut (posMap_type f to)

        TypeTuple pos tys:
            TypeTuple (f pos) (List.map (posMap_type f) tys)

        TypeRecord pos ar:
            TypeRecord (f pos) (recordArgs_map (posMap_type f) ar)
#]
