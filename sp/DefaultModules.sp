
#
# TODO get this from a decoded string once the troubles with SPON are fixed
#
asRecord =
    as ModulesFile.ModulesFile
    { libraries = [ { , modules = [ { , globalTypes = [ "None" , "Bool" , "Text" , "List" , "Number" ] , globalValues = [ "None" , "True" , "False" ] , visibleAs = "SPCore" , path = "SPCore" } , { , globalTypes = [ "Int" ] , globalValues = [] , visibleAs = "List" , path = "SPCore/List" } , { , globalTypes = [ "Maybe" ] , globalValues = [ "Just" , "Nothing" ] , visibleAs = "Maybe" , path = "SPCore/Maybe" } , { , globalTypes = [] , globalValues = [] , visibleAs = "Text" , path = "SPCore/Text" } , { , globalTypes = [] , globalValues = [] , visibleAs = "Tuple" , path = "SPCore/Tuple" } , { , globalTypes = [] , globalValues = [ "log" , "todo" ] , visibleAs = "Debug" , path = "SPCore/Debug" } , { , globalTypes = [] , globalValues = [ "assert" , "clamp" , "identity" , "modBy" , "min" , "max" ] , visibleAs = "SPCore/Basics" , path = "SPCore/Basics" } , { , globalTypes = [ "Buffer" ] , globalValues = [] , visibleAs = "Buffer" , path = "SPLib/Buffer" } , { , globalTypes = [] , globalValues = [] , visibleAs = "Parser" , path = "SPLib/Parser" } , { , globalTypes = [ "Dict" ] , globalValues = [] , visibleAs = "Dict" , path = "SPCore/Dict" } , { , globalTypes = [ "Set" ] , globalValues = [] , visibleAs = "Set" , path = "SPCore/Set" } , { , globalTypes = [] , globalValues = [] , visibleAs = "Random" , path = "SPCore/Random" } , { , globalTypes = [ "Result" ] , globalValues = [ "Ok" , "Err" ] , visibleAs = "Result" , path = "SPCore/Result" } ] , source = "spcore" } ] , sourceDirs = [] }


asText =
    """
library =
    # "spcore" is a special value for the core library
    source = "spcore"

    module =
       path = SPCore
       importAs = SPCore
       globalTypes =
          None
          Bool
          Text
          Number
       globalValues =
          None
          True
          False

    module =
       path = SPCore/List
       importAs = List

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
       path = SPCore/Dict
       importAs = Dict
       globalTypes = Dict

    module =
       path = SPCore/Set
       importAs = Set
       globalTypes = Set

    module =
       path = SPCore/Result
       importAs = Result
       globalTypes = Result
       globalValues =
          Ok
          Err
"""
