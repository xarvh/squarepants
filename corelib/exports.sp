
module =
   path = Core
   exposes =
      None
      Bool
      Text
      List
      Number
      'none
      'true
      'false


module =
   path = Debug
   exposes =
      log
      todo
      toHuman
      benchStart
      benchStop


module =
    path = Array_Test
    exposes =
        tests


module =
   path = Array
   exposes =
      Array
      push
      pop
      get
      set
      sortBy
      fromList
      toList
      each


module =
    path = List_Test
    exposes =
        tests


module =
   path = List
   exposes =
      any
      all
      find
      findMap
      member
      sort
      sortBy
      indexBy
      for
      for2
      indexedFor
      indexedFor2
      forReversed
      forReversed2
      length
      map
      map2
      mapRes
      forRes
      range
      indexedMap
      indexedMap2
      append
      concat
      concatMap
      partition
      head
      last
      take
      takeFast
      takeTailRec
      takeReverse
      takeWhile
      filter
      filterMap
      mapFirst
      each
      indexedEach2
      reverse
      repeat
      drop
      minimum
      maximum
      circularPairs
      intersperse
      partitionWhile

module =
    path = Maybe
    exposes =
        Maybe
        'just
        'nothing
        onJust
        map
        toResult
        mapRes
        withDefault


module =
    path = Text
    exposes =
        forEach
        length
        slice
        fromNumber
        toNumber
        startsWith
        trimLeft
        trimRight
        toLower
        toUpper
        dropLeft
        dropRight
        padLeft
        padRight
        repeat
        replace
        startsWithRegex
        replaceRegex
        split
        contains
        join

module =
    path = Tuple
    exposes =
        first
        second
        mapFirst
        mapSecond
        mapBoth
        pair



module =
    path = Basics
    exposes =
        Int
        compare
        identity
        not
        applyIf
        btw
        cloneUni
        cloneImm
        max
        min
        clamp
        round
        modBy


module =
    path = Hash_Test
    exposes =
        tests


module =
    path = Hash
    exposes =
        Hash
        insert
        remove
        get
        for_
        for
        each
        fromList
        toList
        pop


module =
    path = Dict_Test
    exposes =
        tests


module =
    path = Dict
    exposes =
        Dict
        empty
        get
        member
        size
        insert
        remove
        update
        ofOne

        join
        intersect
        diff
        merge
        onlyBothOnly
        map
        mapRes
        mapKeys
        each
        for
        forRes
        forReversed
        filter
        partition
        any
        keys
        values
        toList
        fromList

module =
    path = Set
    exposes =
      Set
      empty
      member
      size
      isEmpty
      insert
      remove
      ofOne
      join
      intersect
      diff
      map
      for
      toList
      fromList

module =
    path = Result
    exposes =
      Result
      'ok
      'err
      onOk
      onErr
      map
      mapError
      fromMaybe
      withDefault

module =
    path = Self
    exposes =
        Value
        Self
        internalRepresentation
        LoadPars
        load
        toCaModules

module =
   path = Compiler/Ast
   exposes =
      Name
      Ref
      UnivarId
      Uniqueness
      'refLocal
      'refGlobal
      'refPlaceholder
      'uni
      'imm
      'depends
      toImm
      toUni

module =
   path = Compiler/TypedAst
   exposes =
        TyvarId
        RawType
        'typeExact
        'typeFn
        'typeVar
        'typeRecord
        'typeError

        ParType
        'parRe
        'parSp

        FullType

        Expression
        'literalNumber
        'literalText
        'variable
        'constructor
        'fn
        'call
        'record
        'recordAccess
        'letIn
        'if
        'try
        'destroyIn
        'error

        Pattern
        'patternAny
        'patternLiteralText
        'patternLiteralNumber
        'patternConstructor
        'patternRecord

        Argument
        'argumentExpression
        'argumentRecycle

        Parameter
        'parameterPattern
        'parameterRecycle
        'parameterPlaceholder

        Tyvar
        Univar
        ValueDef

        Substitutions
        Module
        initModule
        SubsAsFns
        resolveUni
        resolveParType
        resolveFull
        resolveRaw
        resolveArg
        resolvePar
        resolveExpression
        resolvePattern
        resolveValueDef

        toRaw
        mapPars
        patternNames
        typeTyvars
        typeAllowsFunctions
        normalizeTyvarId
        normalizeType

module =
   path = Compiler/CanonicalAst
   exposes =
        RawType
        'typeNamed
        'typeFn
        'typeRecord
        'typeAnnotationVariable
        'typeError
        ParType
        'parRe
        'parSp
        FullType
        Expression
        'literalNumber
        'literalText
        'variable
        'constructor
        'fn
        'call
        'record
        'recordAccess
        'letIn
        'if
        'try
        Argument
        'argumentExpression
        'argumentRecycle
        Parameter
        'parameterPattern
        'parameterRecycle
        'parameterPlaceholder
        Annotation
        Pattern
        'patternAny
        'patternLiteralText
        'patternLiteralNumber
        'patternConstructor
        'patternRecord
        PatternCompleteness
        'partial
        'complete
        Deps
        LocalDef
        ValueDef
        AliasDef
        VariantTypeDef
        ConstructorDef
        Module
        initModule
        parTypeToRaw
        typeTyvars
        typeUnivars
        patternPos
        patternTyvars
        patternUnivars
        patternNames
        expressionPos

module =
   path = Compiler/EmittableAst
   exposes =
        Expression
        'literalText
        'literalNumber
        'variable
        'call
        'fn
        'conditional
        'and
        'shallowEqual
        'letIn
        'literalArray
        'arrayAccess
        'constructor
        'constructorAccess
        'isConstructor
        'literalRecord
        'recordAccess
        'missingPattern
        Argument
        'argumentRecycle
        'argumentSpend
        GlobalDefinition

module =
   path = Compiler/FormattableAst
   exposes =
        Binop
        Module
        ValueDef
        AliasDef
        VariantTypeDef
        Comment
        Layout
        'inline
        'aligned
        'indented
        Statement
        'commentStatement
        'evaluation
        'valueDef
        'aliasDef
        'unionDef
        Expression
        'expression
        Expr_
        'literalText
        'literalNumber
        'argumentPlaceholder
        'resolvedArgumentPlaceholder
        'statements
        'list
        'record
        'lowercase
        'uppercase
        'constructor
        'recordShorthand
        'fn
        'unopCall
        'binopChain
        'call
        'poly
        'if
        'try
        'native
        RecordAttribute
        BinopChain
        binopChainExpressions
        binopChainReverse
        binopChainAllBinops
        statementPos

module =
    path = Compiler/Meta
    exposes =
        DependencyType
        'valueDependency
        'constructorDependency
        'typeDependency
        RootDirectory
        'core
        'user
        'installed
        RootPaths
        rootDirectoryToPath
        ImportsPath
        'importsPath
        SourceDir
        ModulePath
        UMR
        'UMR
        USR
        'USR
        ByUsr
        Location
        'locationSourceDir
        'locationLibrary
        Imports
        initImports
        ExportOptions
        Exports
        ResolvePars
        resolve
        resolveLocation

module =
    path = Compiler/Op
    exposes =
        UnopId
        'unopPlus
        'unopMinus
        'unopUnique
        'unopRecycle
        Unop
        Associativity
        'nonAssociative
        'left
        'right
        Binop
        precedence_function
        precedence_application
        precedence_multiplicative
        precedence_addittive
        precedence_comparison
        precedence_logical
        precedence_tuple
        precedence_cons
        precedence_pipe
        precedence_mutop

module =
   path = Compiler/Pos
   exposes =
        Pos
        'p
        'm
        'end
        's
        'n
        't
        'i
        'g
        At
        'at
        start
        end
        range
        drop

module =
    path = Compiler/Token
    exposes =
        LineNumber
        Token
        'token
        OpenOrClosed
        'open
        'closed
        SingleOrTriple
        'singleQuote
        'tripleQuote
        Kind
        'comment
        'newSiblingLine
        'blockStart
        'blockEnd
        'badIndent
        'textLiteral
        'numberLiteral
        'lowercase
        'constructor
        'uppercase
        'recordShorthand
        'argumentPlaceholder
        'uniquenessPolymorphismBinop
        'fn
        'if
        'then
        'else
        'try
        'as
        'with
        'introspectValue
        'introspectType
        'comma
        'colon
        'threeDots
        'defop
        'unop
        'binop
        'roundParen
        'squareBracket
        'curlyBrace
        'native
