

asText as Text =
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
