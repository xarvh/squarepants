

asText as Text =
    """
library =
    source = "core:prelude"

    module =
        path = Core
        importAs = Core
        globalTypes =
            None
            Bool
            Text
            Number
        globalValues =
            'none
            'true
            'false
            mut

    module =
        path = List
        importAs = List

    module =
        path = Maybe
        importAs = Maybe
        globalTypes =
            Maybe
        globalValues =
           'just
           'nothing

    module =
        path = Text
        importAs = Text

    module =
        path = Tuple
        importAs = Tuple

    module =
        path = Debug
        importAs = Debug
        globalValues =
            log
            todo

    module =
        path = Basics
        globalTypes =
            Int
        globalValues =
            assert
            clamp
            identity
            modBy
            min
            max

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
            'ok
            'err
"""
