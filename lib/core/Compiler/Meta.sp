#
# This module deals with modeling dependencies, source directories and references across modules
#
var DependencyType =
    , 'valueDependency
    , 'constructorDependency
    , 'typeDependency


[#

First, some terminology.
Let's assume we have this directory structure:

    /
        usr/
            local/
                squarepants/
                    corelib/
                        Result.sp
        home/
            someUser/
                myProjects/
                    thisProject/
                        src/
                            Main.sp
                        libs/
                            identifiers/
                                Id.sp
                        installedLibraries/
                            library1/
                                libs/
                                    library2/
                                        libs/
                                            library3/
                                                src/
                                                    Types/
                                                        Person.sp

  To uniquely identify each module, we define three values: root, importsDir, sourceDir, modulePath.
  The three modules in the example above have:

  *) Result.sp:
      root: 'core
      importsDir: ""
      sourceDir: "src"
      modulePath: "Result"

  *) Main.sp:
      root: 'user
      importsDir: ""
      sourceDir: "src"
      modulePath: "Main"

  *) Id.sp:
      root: 'user
      importsDir: "libs/identifies"
      sourceDir: ""
      modulePath: "Id"

  *) Types/Person.sp:
      root: 'installed
      importsDir: "library1/libs/library2/libs/library3"
      sourceDir: "src"
      modulePath: "Types/Person"

   The modulePath is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
   ex: "Core/List"

#]

#
# This type defines the possible root locations of our code.
#
# It is designed to leave out absolute paths in the output, so that the output is independent on the location of the repo.
# At worst, the output should depend only on the directory structure of the project's directory.
#
var RootDirectory =
    , # Path is relative to $(compiler's executable)/corelib
      'core
    , # Path is relative to $(project's root)
      'user
    , # Path is relative to $(project's root)/installedLibraries
      'installed


RootPaths =
    {
    , core as Text
    , installed as Text
    , project as Text
    }


rootDirectoryToPath as fn RootPaths, RootDirectory: Text =
    fn paths, rootDirectory:
    try rootDirectory as
        'core: paths.core
        'user: paths.project
        'installed: paths.installed


var ImportsPath =
    , 'importsPath RootDirectory Text


#
# Unique Module Reference
#
# Uniquely identifies a module within the whole project.
#
SourceDir =
    Text


ModulePath =
    Text


var UMR =
    , 'UMR ImportsPath SourceDir ModulePath


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
# Every module uses exactly one Imports that tells it how to translate module names and which globals are available.
#

var Location =
    , # When something is in a source dir, we know already its exact UMR
      'locationSourceDir UMR
    , # When something is in a library, we do NOT have the "sourceDir" part of the UMR!
      # We need to check the library's Imports to figure that out.
      'locationLibrary ImportsPath ModulePath


Imports =
    {
    # Tells us in which module a global is actually defined
    , globalNameToLocation as Dict Name Location
    , moduleAliasToLocation as Dict Name Location
    }


initImports as Imports =
    {
    , globalNameToModuleAlias = Dict.empty
    , moduleAliasToDirOrLibrary = Dict.empty
    }


var Exports =
    , 'all
    , 'modulesByAlias (Dict Name (Dict Name ExportOptions))


#
# This function assumes that the symbol is root: it will NOT check for values defined inside closures!
#
ResolvePars =
    {
    , currentImports as Imports
    , currentModule as UMR
    , loadExports as fn ImportsPath: Result Text (Imports & Exports)
    }


resolve as fn ResolvePars, Maybe Name, Name: Result [ Text ] USR =
    fn pars, maybeReferencedModuleAlias, referencedName:
    try maybeReferencedModuleAlias as

        'just alias:
            try Dict.get alias currentImports.moduleAliasToLocation as

                'nothing:
                    [
                    , "TODO currentImports" .. " says that `" .. referencedName .. "` is a global from module `" .. referencedAlias
                    , "However I cannot find that module!"
                    ]
                    >> 'err

                'just location:
                    resolveLocation pars location name

        'nothing:
            try Dict.get referencedName currentImports.globalNameToLocation as
                'nothing: 'USR pars.currentModule referencedName >> 'ok
                'just location: resolveLocation pars location name


resolveLocation as fn ResolvePars, Location, Maybe Name, Name: Result [ Text ] USR =
    fn pars, location, maybeReferencedModuleAlias, referencedName:
    try location as

        'locationSourceDir umr:
            'USR umr referencedName >> 'ok

        'locationLibrary importsPath modulePath:
            # We are missing the $sourceDir part of the UMR; this information is in the Imports of the library,
            # but is integrated in the Exports when we load it.
            #
            #    $importsDirOfLibrary/
            #        imports.sp
            #        $sourceDir/         <---------- We need this
            #            $modulePath

            pars.loadExports importsPath
            >> onOk fn libraryExports:
            try Dict.get modulePath libraryExports.exposedModulesByPath as

                'nothing:
                    try maybeReferencedModuleAlias as

                        'just referencedModuleAlias:
                            [
                            , "imports.sp translates `$referencedModuleAlias` as `$modulePath`"
                            , "However, library $directoryPathOfLibrary does not expose any $modulePath module"
                            ]
                            >> 'err

                        'nothing:
                            [
                            , "TODO ??????"
                            ]
                            >> 'err

                'just exposedModule:
                    try Dict.get referencedName exposedModule.exposedUsrByName as

                        'just usr:
                            'ok usr

                        'nothing:
                            try maybeReferencedModuleAlias as

                                'just referencedModuleAlias:
                                    [
                                    , "imports.sp translates `$referencedAlias` as `$modulePath`"
                                    , "However, $modulePath in library $directoryPathOfLibrary does not expose any $referencedName"
                                    ]
                                    >> 'err

                                'nothing:
                                    [
                                    , "TODO ?????!!!!"
                                    ]
                                    >> 'err
