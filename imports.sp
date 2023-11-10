[# imports.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]


sourceDir =
    path = "specs"


sourceDir =
    path = "src"

    module =
        path = ImportsFile
        globals =
            ImportsFile

    module =
        path = Human/Type
        globals =
            usrToText

    module =
       path = Compiler/Error
       importAs = Error
       globals =
          Error
          Res

    module =
       path = Compiler/CoreDefs
       importAs = CoreDefs


    module =
       path = SPLib/Format
       importAs = Fmt

    module =
       path = SPLib/Parser
       importAs = Parser

    module =
       path = SPLib/RefHierarchy
       importAs = RefHierarchy

    module =
       path = SPLib/SPON
       importAs = SPON

    module =
       path = SPLib/Test
       importAs = Test
       globals = Test

    module =
       path = Targets/Javascript/Ast
       importAs = JA

    module =
       path = Compiler/TestHelpers
       importAs = TH

    module =
       path = Platform
       importAs = Platform
       globals =
          Platform


# This will be transformed into a platform
library =
    source = "core:posix"

    module =
       path = IO
       globals = IO

    module =
       path = Path


library =
    source = "core:prelude"

    # unlike sourceDirs, libraries don't automatically expose all available modules
    module =
        path = Array_Test
    module =
        path = Dict_Test
    module =
        path = Hash_Test
    module =
        path = List_Test

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

    module =
       path = Compiler/Ast
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
        globals =
            Imports
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
       globals =
            Pos
            At
            'at

    module =
        path = Compiler/Token
        importAs = Token
        globals =
            Token
            'token

