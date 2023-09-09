[# modules.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]


sourceDir =
    path = "specs"


sourceDir =
    path = "src"

    module =
       path = Compiler/Error
       importAs = Error
       globalTypes =
          Error
          Res

    module =
       path = Compiler/CoreTypes
       importAs = CoreTypes

    module =
       path = Types/Ast
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
       path = Targets/Javascript/Ast
       importAs = JA

    module =
       path = Compiler/TestHelpers
       importAs = TH

    module =
        path = Types/Meta
        importAs = Meta
        globalTypes =
            Meta
            ByUsr
            USR
            UMR
        globalValues =
            'USR
            'UMR

    module =
        path = Types/Op
        importAs = Op

    module =
       path = Types/Pos
       importAs = Pos
       globalTypes =
            Pos
            At
       globalValues =
            'at

    module =
        path = Types/Token
        importAs = Token
        globalTypes =
            Token
        globalValues =
            'token

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
