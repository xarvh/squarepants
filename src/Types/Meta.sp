#
# This tells us where a module comes from.
#
# It has two requirements:
#   1. It distinguishes two modules with the same name
#   2. It tells us how to actually load the module
#
# TODO: for the time being, this is just a placeholder
#
var Source =
    , 'core
    , 'posix
    , # This one is a HACK, until we have proper platform management
      'browser
    , # This one is a HACK, until we have proper platform management
      'sourceDirId Text


#
# Uniqueliy identifies a module within a source.
#
# It is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
#
# ex: "Core/List"
#
ModulePath =
    Text


var UMR =
    , 'UMR Source ModulePath


# TODO: have one for types, one for constructors and one for values?
var USR =
    , 'USR UMR Name


ByUsr a =
    Dict USR a


Meta =
    {
    , globalTypes as Dict Name USR
    # These resolve global symbol names
    , globalValues as Dict Name USR
    # These resolve module names
    , moduleVisibleAsToUmr as Dict Name UMR
    , sourceDirIdCounter as Int
    # The sourceDirId is the one eventually used to produce an internal unique name for the variable.
    # There are two main reasons for this:
    # 1) We don't want local path references to enter in the compiled output
    # 2) We don't want to have to deal with weird characters when we generate the internal names
    , sourceDirIdToPath as Dict Text Text
    # This is used by toHuman, to show symbols the same way the user would expect to read and write them
    # (only the main meta is used for this)
    , umrToModuleVisibleAs as Dict UMR Name
    }


init as Meta =
    {
    , globalTypes = Dict.empty
    , globalValues = Dict.empty
    , moduleVisibleAsToUmr = Dict.empty
    , sourceDirIdCounter = 0
    , sourceDirIdToPath = Dict.empty
    , umrToModuleVisibleAs = Dict.empty
    }


spCoreUmr as UMR =
    'UMR Meta.'core "Core"


spCoreUSR as fn Name: USR =
    'USR spCoreUmr __
