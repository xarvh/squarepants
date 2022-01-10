[# modules.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]

sourceDir =
    path = "sp"

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


# This will be transformed into a platform
sourceDir =
    path = "posix"

    module =
       path = IO
       importAs = IO
       globalTypes = IO


# This will be transformed into a lib
sourceDir =
    path = "corelib"

    module =
       path = List
       importAs = List

    module =
       path = Maybe
       importAs = Maybe
       globalTypes =
          Maybe
       globalValues =
          Just
          Nothing

    module =
       path = Text
       importAs = Text

    module =
       path = Tuple
       importAs = Tuple

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
       path = Dict
       importAs = Dict
       globalTypes = Dict

    module =
       path = Set
       importAs = Set
       globalTypes = Set

    module =
       path = Result
       importAs = Result
       globalTypes = Result
       globalValues =
          Ok
          Err
          onOk


library =
    # spcore" is a special value for the core library
    source = "spcore"

    module =
       path = SPCore
       importAs = SPCore
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
          log
          todo

