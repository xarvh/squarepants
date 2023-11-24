# Each platform should expose one of these inside a Platform.sp
Platform =
    {
    # TODO at some point, we'll be able to generate a text file from Imports
    # to be used when the user wants to initialize a new project (or library?)
    , defaultImportsFile as ImportsFile
    , defaultOutputName as Text
    # TODO Text is a bad way to represet a binary, but we don't yet have a binary type. =|
    , makeExecutable as fn Meta.ImportsPath: fn Self.LoadPars: Text
    , name as Text
    , quickstart as Text
    }
