
# A platform should expose one of these inside a Platform.sp
# TODO: this should be moved to something like Core/Platform
alias Platform =
    {
    , name as Text
    , makeExecutable as fn Self.LoadPars: Text
    , defaultModules as Text
    , quickstart as Text
    , defaultOutputPath as Text
    }

