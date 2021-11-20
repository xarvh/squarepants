#
# modules.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work.
#
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
       path = Types/FormattableAst
       importAs = FA

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

    module =
       path = SPCore/List
       importAs = List
       globalTypes =
          Int

    module =
       path = SPCore/Maybe
       importAs = Maybe
       globalTypes =
          Maybe
       globalValues =
          Just
          Nothing

    module =
       path = SPCore/Text
       importAs = Text

    module =
       path = SPCore/Tuple
       importAs = Tuple

    module =
       path = SPCore/Debug
       importAs = Debug
       globalValues =
          log
          todo

    module =
       path = SPCore/Basics
       globalValues =
            assert
            clamp
            identity
            modBy
            min
            max

    module =
       path = SPLib/Buffer
       importAs = Buffer
       globalTypes = Buffer

    module =
       path = SPLib/Parser
       importAs = Parser

    module =
       path = SPCore/Dict
       importAs = Dict
       globalTypes = Dict

    module =
       path = SPCore/Set
       importAs = Set
       globalTypes = Set

    module =
       path = SPCore/Random
       importAs = Random

    module =
       path = SPCore/Result
       importAs = Result
       globalTypes = Result
       globalValues =
          Ok
          Err
