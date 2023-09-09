# A platform should expose one of these inside a Platform.sp
# TODO: this should be moved to something like Core/Platform
Platform =
    {
    , defaultModules as Text
    , defaultOutputPath as Text
    , makeExecutable as fn Self.LoadPars: Text
    , name as Text
    , quickstart as Text
    }
