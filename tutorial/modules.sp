[# modules.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]


sourceDir =
    path = "."


sourceDir =
    # TODO remove this, it is here only because we don't have lazy module loading
    path = "../lib/posix"

    module =
       path = IO
       globalTypes = IO

    module =
       path = Path

sourceDir =
    path = "../src"

    module =
       path = Test
       importAs = Test
       globalTypes = Test

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
       path = Types/CanonicalAst
       importAs = CA

    module =
       path = Human/CanonicalAst
       importAs = HCA

    module =
       path = Types/EmittableAst
       importAs = EA

    module =
       path = Types/FormattableAst
       importAs = FA

    module =
       path = Types/JavascriptAst
       importAs = JA

    module =
       path = Compiler/TestHelpers
       importAs = TH

    module =
        path = Types/Meta
        importAs = Meta
        globalTypes =
            Meta
            Name
            ByUsr

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
            At

    module =
        path = Types/Token
        importAs = Token
        globalTypes =
            Token
        globalValues =
            Token

    module =
       path = SPLib/Buffer
       importAs = Buffer
       globalTypes = Buffer

    module =
       path = SPLib/Parser
       importAs = Parser


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
          None
          True
          False

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
            Just
            Nothing

    module =
        path = Text

    module =
        path = Tuple

    module =
        path = Basics
        globalTypes = Int
        globalValues =
            clamp
            identity
            not
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
          Ok
          Err
          onOk
