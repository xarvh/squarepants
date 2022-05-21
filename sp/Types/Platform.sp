

#resolves `main` in whatever module is passed to the compiler


alias GetRidOfMe = {
    , constructors as CA.All CA.Constructor
    , errorEnv as Error.Env
    }


# A platform should expose one of these inside a Platform.sp
# TODO: this should be moved to something like SPCore/Platform
alias Platform = {
    , name as Text
    , compile as GetRidOfMe: [CA.ValueDef]: Dict Text Text
    , defaultModulesFile as ModulesFile.ModulesFile
    , quickstart as Text
    }





#in moduleFile:
#
#platform:
#
#    # spcore" is a special value for the core library
#    source = "./platforms/IO"
#
#    module =
#       path = SPCore
#       importAs = SPCore
#       globalTypes =
#          None
#          Bool
#          Text
#          List
#          Number
#       globalValues =
#          None
#          True
#          False
#          log
#          todo

