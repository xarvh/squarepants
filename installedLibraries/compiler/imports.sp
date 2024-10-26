#
# Libraries
#
library =
    source = ":test"
    module =
       path = Test
       importAs = Test
       globals = Test


library =
    source = ":dependencyResolution"
    module =
       path = RefHierarchy


library =
    source = ":format"
    module =
       path = Format
       importAs = Fmt


library =
    source = ":parser"
    module =
       path = Parser


#
# Source
#
sourceDir =
    path = "src"

    module =
       path = Types/Ast
       importAs = Ast
       globals =
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
       importAs = TA

    module =
       path = Types/CanonicalAst
       importAs = CA

    module =
       path = Types/EmittableAst
       importAs = EA

    module =
       path = Types/FormattableAst
       importAs = FA

    module =
        path = Types/Meta
        importAs = Meta
        globals =
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
        path = Types/Op
        importAs = Op

    module =
       path = Types/Pos
       importAs = Pos
       globals =
            Pos
            At
            'at

    module =
        path = Types/Token
        importAs = Token
        globals =
            Token
            'token

    module =
        path = Compiler/CoreDefs
        importAs = CoreDefs

    module =
        path = Compiler/Error
        importAs = Error


#
# Core
#
library =
    source = "core"

    module =
       path = Compiler/EmittableAst
       importAs = EA

    module =
       path = Core
       globals =
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
       globals =
          log
          todo
          toHuman

    module =
       path = Array
       globals =
          Array

    module =
       path = List

    module =
        path = Maybe
        globals =
            Maybe
            'just
            'nothing

    module =
        path = Text

    module =
        path = Tuple

    module =
        path = Basics
        globals =
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
        globals = Hash

    module =
        path = Dict
        globals = Dict

    module =
        path = Set
        globals = Set

    module =
        path = Result
        globals =
          Result
          'ok
          'err
          onOk

    module =
        path = Self

