# Each platform should expose one of these inside a Platform.sp
Platform =
    {
    , defaultModules as Text
    , defaultOutputPath as Text
    # TODO Text is a bad way to represet a binary, but we don't yet have a binary type. =|
    , makeExecutable as fn Self.LoadPars: Text
    , name as Text
    , quickstart as Text
    }
