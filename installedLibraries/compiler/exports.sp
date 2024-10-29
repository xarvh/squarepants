
module =
    path = Compiler/TypeCheck_Test
    exposes =
        tests

module =
    path = Compiler/MakeCanonical_Test
    exposes =
        tests

module =
    path = Compiler/Parser_Test
    exposes =
        tests

module =
    path = Compiler/Lexer_Test
    exposes =
        tests

module =
    path = Compiler/Uniqueness_Specs
    exposes =
        specs

module =
    path = Human/Format_Test
    exposes =
        tests


module =
   path = Types/Ast
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
   path = Types/TypedAst
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
        'introspect

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
        stripTypePos

        toRaw
        mapPars
        patternPos
        patternNames
        typeTyvars
        typeAllowsFunctions
        normalizeTyvarId
        normalizeType

module =
   path = Types/CanonicalAst
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
        'introspect
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
        typePos
        typeTyvars
        typeUnivars
        patternPos
        patternTyvars
        patternUnivars
        patternNames
        expressionPos


module =
   path = Types/FormattableAst
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
        'introspect
        RecordAttribute
        BinopChain
        binopChainExpressions
        binopChainReverse
        binopChainAllBinops
        statementPos

module =
    path = Types/Meta
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
        sourceDirectoryKey
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
    path = Types/Op
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
   path = Types/Pos
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
    path = Types/Token
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
        Introspect
        'type
        'typeOpen
        'value
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
        'this_is_sp_native
        'sp_introspect


module =
    path = Compiler/Error
    exposes =
          Module
          Error
          'raw
          'simple
          'nested
          Res
          res
          FormattedText
          'formattedText_Default
          'formattedText_Emphasys
          'formattedText_Warning
          'formattedText_Decoration
          toFormattedText
          count


module =
    path = Compiler/Parser
    exposes =
        textToFormattableModule


module =
    path = Compiler/CoreDefs
    exposes =
        coreModule
        umr
        makeUmr
        pathId
        importsDir
        sourceDir
        importsPath

        unaryPlus
        unaryMinus
        add
        multiply
        subtract
        mutableAssign
        mutableAdd
        mutableSubtract
        textConcat
        greaterThan
        lesserThan
        greaterOrEqualThan
        lesserOrEqualThan
        or_
        and_
        trueUsr
        falseUsr
        noneConsUsr
        divide
        listCons
        nilName
        consName
        equal
        notEqual


module =
    path = Compiler/MakeCanonical
    exposes =
        ReadOnly
        textToCanonicalModule


module =
   path = Compiler/MakeEmittable
   exposes =
      translateUsr
      translateName


module =
    path = Compiler/LazyBuild
    exposes =
        build


module =
    path = Human/Format
    exposes =
        formatStatements

