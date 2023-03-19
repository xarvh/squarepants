
alias GetRidOfMe =
    {
    , constructors as [USR & TA.FullType]
    }

# A platform should expose one of these inside a Platform.sp
# TODO: this should be moved to something like Core/Platform
alias Platform =
    {
    , name as Text
    , compileStatements as fn GetRidOfMe, @Compiler/MakeEmittable.State, [EA.GlobalDefinition]: Text
    , makeExecutable as fn Compiler/Compiler.CompiledValue: Text
    , defaultModules as Text
    , quickstart as Text
    , defaultOutputPath as Text
    }

