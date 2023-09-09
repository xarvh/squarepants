[# modules.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]


sourceDir =
    path = "specs"


sourceDir =
    path = "src"

    module =
        path = Human/Type
        globalValues =
            usrToText

    module =
       path = Compiler/Error
       importAs = Error
       globalTypes =
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
       globalTypes = Test

    module =
       path = Targets/Javascript/Ast
       importAs = JA

    module =
       path = Compiler/TestHelpers
       importAs = TH


# This will be transformed into a platform
library =
    source = "core:posix"

    module =
       path = IO
       globalTypes = IO

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
       globalTypes =
          None
          Bool
          Text
          List
          Number
       globalValues =
          'none
          'true
          'false

    module =
       path = Debug
       globalValues =
          log
          todo
          toHuman

    module =
       path = Array
       globalTypes =
          Array

    module =
       path = List

    module =
        path = Maybe
        globalTypes =
            Maybe
        globalValues =
            'just
            'nothing

    module =
        path = Text

    module =
        path = Tuple

    module =
        path = Basics
        globalTypes = Int
        globalValues =
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
        globalTypes = Hash

    module =
        path = Dict
        globalTypes = Dict

    module =
        path = Set
        globalTypes = Set

    module =
        path = Result
        globalTypes = Result
        globalValues =
          'ok
          'err
          onOk

    module =
        path = Self

    module =
       path = Compiler/Ast
       importAs = Ast
       globalTypes =
          Name
          Ref
          UnivarId
          Uniqueness
       globalValues =
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
       path = Compiler/Platform
       importAs = Platform
       globalTypes =
          Platform

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
        globalTypes =
            Meta
            ByUsr
            USR
            UMR
            LibrarySource
            DependencyType
        globalValues =
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
       globalTypes =
            Pos
            At
       globalValues =
            'at

    module =
        path = Compiler/Token
        importAs = Token
        globalTypes =
            Token
        globalValues =
            'token

