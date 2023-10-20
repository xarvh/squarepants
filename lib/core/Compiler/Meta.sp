
var DependencyType =
    , 'valueDependency
    , 'constructorDependency
    , 'typeDependency


#
# This type defines the possible directories where a module might be.
#
# It is designed to minimize the risk of including absolute paths in the output.
# The output code should not be different because the project root is located in a different place.
# At worst, the output should depend only on the directory structure of the project's directory.
#
var Source =
    , # `Core` definitions and the core modules.
      # They are the only ones (TODO for now?) who do not live in the current
      # project directory, but rather, they are wherever the executable is installed.
      'core
    , # This is an alias for whatever Source the *selected* Platform has.
      # This is needed because Platform must know its own Source in order to define overrides.
      # * The first argument is the userSourceDir path /within/ the library
      'platform Text
    , # This is the bulk of an application's specific code
      # All annotated definitions are publicly visible, and all modules use the project's Modules File.
      # * The argument is the directory path, relative to the project's root.
      'userSourceDir Text
    , # This is for self-contained code developed specifically for the app.
      # It exposes only selected definitions and has its own Modules File.
      # * The first argument is the library path, relative to the project's root.
      # * The second argument is the userSourceDir path /within/ the library
      'userLibrary Text Text
    , # This is for libraries installed and managed by the squarepants executable, most often third party libraries
      # They live in installedLibraries/ under the project's root directory.
      # * The first argument is the library path, relative to installedLibraries/.
      # * The second argument is the userSourceDir path /within/ the library
      'installedLibrary Text Text



#
# Uniqueliy identifies a module **within a given source**.
#
# It is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
#
# ex: "Core/List"
#
ModulePath =
    Text


#
# Unique Module Reference
#
# Uniquely identifies a module **within the whole project**.
#
var UMR =
    , 'UMR Source ModulePath


#
# Unique Symbol Reference
#
# Uniquely identifies a value, constructor or type (whether alias or variant) within the whole project
#
var USR =
    , 'USR UMR Name


ByUsr a =
    Dict USR a


#
# Every module uses exactly one ImportEnv that tells it how to translate module names and which globals are available.
#
Imports =
    {
    , globals as Dict Name USR
    # This is used by toHuman, to show symbols the same way the user would expect to read and write them
    # (only the main ImportEnv is used for this)
    , umrToVisibleAs as Dict UMR Name
    # These resolve module names
    , visibleAsToUmr as Dict Name UMR
    }


initImports as Imports =
    {
    , globals = Dict.empty
    , umrToVisibleAs = Dict.empty
    , visibleAsToUmr = Dict.empty
    }


ProjectEnvironment =
    {
    , libraryImportsBySource as Dict Source Imports
    , mainImports as Imports
    , platformSource as Source
    }


coreUmr as UMR =
    'UMR 'core "Core"


coreUsr as fn Name: USR =
    'USR spCoreUmr __
