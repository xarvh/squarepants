#
# This tells us where a module comes from.
#
# It has two requirements:
#   1. It distinguishes two modules with the same name
#   2. It tells us how to actually load the module
#
# TODO: for the time being, this is just a placeholder
#
union Source =
    , Core
    , Posix # This one is a HACK, until we have proper platform management
    , Browser # This one is a HACK, until we have proper platform management
    , SourceDir Text


#
# Uniqueliy identifies a module within a source.
#
# It is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
#
# ex: "Core/List"
#
alias ModulePath =
    Text


union UMR =
    UMR Source ModulePath


# TODO: have one for types, one for constructors and one for values?
union USR =
    USR UMR Name


alias ByUsr a =
    Dict USR a


alias Meta = {

    # These resolve global symbol names
    , globalValues as Dict Name USR
    , globalTypes as Dict Name USR

    # These resolve module names
    , moduleVisibleAsToUmr as Dict Name UMR

    # This is used by toHuman, to show symbols the same way the user would expect to read and write them
    # (only the main meta is used for this)
    , umrToModuleVisibleAs as Dict UMR Name
    }


init as Meta = {
    , globalValues = Dict.empty
    , globalTypes = Dict.empty
    , moduleVisibleAsToUmr = Dict.empty
    , umrToModuleVisibleAs = Dict.empty
    }


spCorePath as Text =
    "Core"


spCoreUmr as UMR =
    UMR Core spCorePath


spCoreUSR as fn Name: USR =
    USR spCoreUmr __

