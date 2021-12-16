module Types.Meta exposing (..)

import Dict exposing (Dict)


type alias Meta =
    { globalValues : Dict String String
    , globalTypes : Dict String String

    {- TODO
    , originalToVisibleAs : Dict String String
    , visibleAsToOriginal : Dict String String
    -}
    , bynames : Dict String String
    }


init : Meta
init =
    { globalValues = Dict.empty
    , globalTypes = Dict.empty
    , bynames = Dict.empty
    }
