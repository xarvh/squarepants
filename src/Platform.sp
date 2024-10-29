GetPlatformsTranslatedUsr =
    fn Text, Text: EA.TranslatedUsr


# Each platform should expose one of these inside a Platform.sp
Platform =
    {
    # This is used to correctly add dependencies

    , compile as fn [ EA.TranslatedUsr & Text ], Self.LoadPars: Text
    # TODO at some point, we'll be able to generate a text file from Imports
    # to be used when the user wants to initialize a new project (or library?)
    , defaultImportsFile as ImportsFile
    , defaultOutputName as Text
    , extraRequiredSymbols as [ { modulePath as Text, symbolName as Name }  ]
    # TODO Text is a bad way to represet a binary, but we don't yet have a binary type. =|
    , makeExecutable as fn GetPlatformsTranslatedUsr, Self.LoadPars: Text
    , name as Text
    , quickstart as Text
    }
