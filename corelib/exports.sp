
module =
   path = Core
   expose =
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

module =
   path = Array
   exposes =
      Array

module =
   path = List

module =
    path = Maybe
    exposes =
        Maybe
        'just
        'nothing

module =
    path = Text

module =
    path = Tuple

module =
    path = Basics
    exposes =
        Int
        clamp
        cloneImm
        cloneUni
        identity
        not
        applyIf
        modBy
        min
        max
        btw

module =
    path = Hash
    exposes = Hash

module =
    path = Dict
    exposes = Dict

module =
    path = Set
    exposes = Set

module =
    path = Result
    exposes =
      Result
      'ok
      'err
      onOk

module =
    path = Self

module =
   path = Compiler/Ast
   importAs = Ast
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
   importAs = TA

module =
   path = Compiler/CanonicalAst
   importAs = CA

module =
   path = Compiler/EmittableAst
   importAs = EA

module =
   path = Compiler/FormattableAst
   importAs = FA

module =
    path = Compiler/Meta
    importAs = Meta
    exposes =
        Imports
        Exports
        ByUsr
        USR
        UMR
        Source
        DependencyType
        'USR
        'UMR
        'valueDependency
        'constructorDependency
        'typeDependency

module =
    path = Compiler/Op
    importAs = Op

module =
   path = Compiler/Pos
   importAs = Pos
   exposes =
        Pos
        At
        'at

module =
    path = Compiler/Token
    importAs = Token
    exposes =
        Token
        'token

