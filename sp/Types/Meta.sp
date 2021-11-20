
#
# The name of a variable, type, attribute or "visibleAs" module name
#
alias Name =
    Text


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
    , SourcePlaceholder


alias Path =
    [Text]


union UniqueModuleReference =
    UMR Source Path


union UniqueSymbolReference =
    USR Source Path Name


alias Meta = {

    # These resolve global symbol names
    , globalValues as Dict Name UniqueSymbolReference
    , globalTypes as Dict Name UniqueSymbolReference

    # These resolve module names
    , moduleVisibleAsToUmr as Dict Name UniqueModuleReference

    # This is used by toHuman, to show symbols the same way the user would expect to read and write them
    # (only the main meta is used for this)
    , umrToModuleVisibleAs as Dict UniqueModuleReference Name
    }


init =
    as Meta
    {
    , globalValues = Dict.empty
    , globalTypes = Dict.empty
    , moduleVisibleAsToUmr = Dict.empty
    , umrToModuleVisibleAs = Dict.empty
    }
