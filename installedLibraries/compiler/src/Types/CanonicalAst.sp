var RawType =
    , # alias, opaque or varType
      'typeNamed Pos USR [ RawType ]
    , 'typeFn Pos [ ParType ] FullType
    , 'typeRecord Pos (Dict Name RawType)
    , 'typeAnnotationVariable Pos Name
    , # This is used as a placeholder when there is an error and a type can't be determined
      # It's useful to avoid piling up errors (I think)
      'typeError Pos


var ParType =
    , 'parRe RawType
    , 'parSp FullType


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
    , 'fn Pos [ Parameter ] Expression
    , 'call Pos Expression [ Argument ]
    , # maybeExpr can be, in principle, any expression, but in practice I should probably limit it
      # to nested RecordAccess? Maybe function calls too?
      'record Pos (Maybe Expression) (Dict Name Expression)
    , 'recordAccess Pos Name Expression
    , 'letIn LocalDef Expression
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
          , patternsAndExpressions as [ Uniqueness & Pattern & Expression ]
          , value as Expression
          }
    , 'introspect Pos Token.Introspect USR


var Argument =
    , 'argumentExpression Expression
    , 'argumentRecycle Pos Name [ Name ]


var Parameter =
    , 'parameterPattern Uniqueness Pattern
    , 'parameterRecycle Pos Name
    , 'parameterPlaceholder Int


Annotation =
    {
    , raw as RawType
    , tyvars as Dict Name { nonFn as Maybe Pos }
    , univars as Dict UnivarId None
    }


var Pattern =
    , 'patternAny Pos (Maybe Name) (Maybe Annotation)
    , 'patternLiteralText Pos Text
    , 'patternLiteralNumber Pos Number
    , 'patternConstructor Pos USR [ Pattern ]
    , 'patternRecord Pos PatternCompleteness (Dict Name Pattern)


var PatternCompleteness =
    , 'partial
    , 'complete


#
# Module
#

Deps =
    Dict USR DependencyType


LocalDef =
    {
    , body as Expression
    , pattern as Pattern
    , uni as Uniqueness
    }


ValueDef =
    {
    , directDeps as Dict USR DependencyType
    , maybeAnnotation as Maybe Annotation
    , maybeBody as Maybe Expression
    , name as Name
    , namePos as Pos
    }


AliasDef =
    {
    , directDeps as Dict USR DependencyType
    , pars as [ Name & Pos ]
    , type as RawType
    , usr as USR
    }


VariantTypeDef =
    {
    , constructors as Dict Name ConstructorDef
    , pars as [ Name & Pos ]
    , usr as USR
    }


ConstructorDef =
    {
    , constructorUsr as USR
    , directDeps as Dict USR DependencyType
    , ins as [ RawType ]
    , name as Name
    , out as RawType
    , pos as Pos
    , variantTypeUsr as USR
    }


Module =
    {
    , aliasDefs as Dict Name AliasDef
    , asText as Text
    , constructorDefs as Dict Name ConstructorDef
    , fsPath as Text
    , umr as UMR
    , umrToAlias as Dict UMR Name
    , usrToGlobal as Dict USR Name
    , valueDefs as Dict Name ValueDef
    , variantTypeDefs as Dict Name VariantTypeDef
    }


initModule as fn Text, UMR, Text: Module =
    fn fsPath, umr, asText:
    {
    , aliasDefs = Dict.empty
    , asText
    , constructorDefs = Dict.empty
    , fsPath
    , umr
    , umrToAlias = Dict.empty
    , usrToGlobal = Dict.empty
    , valueDefs = Dict.empty
    , variantTypeDefs = Dict.empty
    }


#
# helpers
#

parTypeToRaw as fn ParType: RawType =
    fn p:
    try p as
        'parRe raw: raw
        'parSp full: full.raw


typeTyvars as fn RawType: Dict Name Pos =
    fn raw:
    fromList as fn [ RawType ]: Dict Name Pos =
        fn list:
        List.for Dict.empty list (fn acc, item: Dict.join acc (typeTyvars item))

    try raw as
        'typeNamed _ _ args: fromList args
        'typeFn _ pars to: fromList (to.raw :: List.map pars parTypeToRaw)
        'typeRecord _ attrs: fromList (Dict.values attrs)
        'typeAnnotationVariable pos name: Dict.ofOne name pos
        'typeError _: Dict.empty


typePos as fn RawType: Pos =
    try __ as
        'typeNamed p _ _: p
        'typeFn p _ _: p
        'typeRecord p _: p
        'typeAnnotationVariable p _: p
        'typeError p: p


typeUnivars as fn RawType: Dict UnivarId None =
    fn raw:
    fromList as fn [ RawType ]: Dict UnivarId None =
        fn list:
        List.for Dict.empty list (fn acc, item: Dict.join acc (typeUnivars item))

    insertUni as fn Uniqueness, Dict UnivarId None: Dict UnivarId None =
        fn uni, acc:
        try uni as
            'depends uid: Dict.insert acc uid 'none
            _: acc

    parUnivars as fn Dict UnivarId None, ParType: Dict UnivarId None =
        fn acc, par:
        try par as

            'parRe _:
                acc

            'parSp full:
                acc
                >> Dict.join __ (typeUnivars full.raw)
                >> insertUni full.uni __

    try raw as

        'typeNamed _ _ args:
            fromList args

        'typeRecord _ attrs:
            fromList (Dict.values attrs)

        'typeAnnotationVariable pos name:
            Dict.empty

        'typeError _:
            Dict.empty

        'typeFn _ pars to:
            Dict.empty
            >> insertUni to.uni __
            >> List.for __ pars parUnivars


patternPos as fn Pattern: Pos =
    fn pa:
    try pa as
        'patternAny p _ _: p
        'patternLiteralText p _: p
        'patternLiteralNumber p _: p
        'patternConstructor p _ _: p
        'patternRecord p _ _: p


patternTyvars as fn Pattern: Dict Name { nonFn as Maybe Pos } =
    fn pa:
    try pa as
        'patternAny _ _ ('just ann): ann.tyvars
        'patternAny _ _ 'nothing: Dict.empty
        'patternLiteralText _ _: Dict.empty
        'patternLiteralNumber _ _: Dict.empty
        'patternConstructor _ _ args: List.for Dict.empty args (fn acc, arg: Dict.join acc (patternTyvars arg))
        'patternRecord _ _ attrs: Dict.for Dict.empty attrs (fn acc, k, arg: Dict.join acc (patternTyvars arg))


patternUnivars as fn Pattern: Dict UnivarId None =
    fn pa:
    try pa as
        'patternAny _ _ ('just ann): ann.univars
        'patternAny _ _ 'nothing: Dict.empty
        'patternLiteralText _ _: Dict.empty
        'patternLiteralNumber _ _: Dict.empty
        'patternConstructor _ _ args: List.for Dict.empty args (fn acc, arg: Dict.join acc (patternUnivars arg))
        'patternRecord _ _ attrs: Dict.for Dict.empty attrs (fn acc, k, arg: Dict.join acc (patternUnivars arg))


patternNames as fn Pattern: [ { maybeAnnotation as Maybe Annotation, name as Name, pos as Pos } ] =
    rec =
        fn acc, p:
        try p as
            'patternAny pos 'nothing _: acc
            'patternAny pos ('just name) maybeAnnotation: [ { maybeAnnotation, name, pos }, acc... ]
            'patternLiteralNumber pos _: acc
            'patternLiteralText pos _: acc
            'patternConstructor pos path ps: List.for acc ps rec
            'patternRecord pos _ ps: Dict.for acc ps (fn a, k, v: rec a v)

    rec [] __


expressionPos as fn Expression: Pos =
    fn exp:
    try exp as
        'literalNumber p _: p
        'literalText p _: p
        'variable p _: p
        'constructor p _: p
        'fn p _ _: p
        'call p _ _: p
        'record p _ _: p
        'recordAccess p _ _: p
        'letIn def e: expressionPos e
        'if p _: p
        'try p _: p
        'introspect p _ _: p


expressionName as fn Expression: Text =
    fn exp:
    try exp as
        'literalNumber p _: "literal number"
        'literalText p _: "literal text"
        'variable p _: "variable"
        'constructor p _: "constructor"
        'fn p _ _: "function definition"
        'call p _ _: "function call"
        'record p _ _: "record literal"
        'recordAccess p _ _: "record access"
        'letIn _ e: expressionName e
        'if p _: "if-expression"
        'try p _: "try-expression"
        'introspect p _ _: "introspection"
