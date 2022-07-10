
alias GetRidOfMe = {
    , constructors as [Meta.UniqueSymbolReference & CA.Constructor]
    , errorEnv as Error.Env
    }


# A platform should expose one of these inside a Platform.sp
# TODO: this should be moved to something like Core/Platform
alias Platform = {
    , name as Text
    , compile as GetRidOfMe: Meta.UniqueSymbolReference: [EA.GlobalDefinition]: Text
    , defaultModules as Text
    , quickstart as Text
    , defaultOutputPath as Text
    }

