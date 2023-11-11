#
# This module deals with modeling dependencies, source directories and references across modules
#
var DependencyType =
    , 'valueDependency
    , 'constructorDependency
    , 'typeDependency


[#

  To uniquely identify each module, we split its path in four parts.

  Let's see an example first, and assume we have this directory structure:

    /
        usr/
            local/
                squarepants/
                    corelib/
                        # No imports.sp here, it's hard-coded in CoreDefs
                        src/
                            Result.sp
        home/
            someUser/
                myProjects/
                    thisProject/
                        imports.sp
                        src/
                            Main.sp
                        libs/
                            identifiers/
                                imports.sp
                                exports.sp
                                Id.sp
                        installedLibraries/
                            library1/
                                libs/
                                    library2/
                                        libs/
                                            library3/
                                                imports.sp
                                                exports.sp
                                                src/
                                                    Types/
                                                        Person.sp

  The modules in the example above have:

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


  Now we can describe the four parts a bit more in the abstract:

    1) Root
          This is a variant type that tells us where to start.
          We use it so that we don't have to include any information about the local file system in the output.

    2) ImportsDir
          This tells us where `imports.sp` is.

    3) SourceDir
          This refers to a specific sourceDir entry in `imports.sp`.

    4) ModulePath
          The actual module, possibly with uppercased directory name.
          The modulePath is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.

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
    , modulePathToLocation as Dict Name Location
    }


initImports as Imports =
    {
    , globalNameToLocation = Dict.empty
    , moduleAliasToLocation = Dict.empty
    , modulePathToLocation = Dict.empty
    }


# TODO find a better name for this one
ExportOptions =
    {
    # isOpen is false for opaque types: aliases will hide their definition and varTypes will not expose their constructors
    , isOpen as Bool
    , usr as USR
    }


Exports =
    Dict Name (Dict Name ExportOptions)


#
# This function assumes that the symbol is root: it will NOT check for values defined inside closures!
#
# The code is here because this resolution system is a core feature of the language and should be
# consistent across platforms.
#
ResolvePars error =
    {
    , currentImports as Imports
    , currentModule as UMR
    , loadExports as fn ImportsPath: Result error Exports
    , makeError as fn [ Text ]: error
    }


resolve as fn ResolvePars error, Maybe Name, Name: Result error USR =
    fn pars, maybeReferencedModuleAlias, referencedName:
    try maybeReferencedModuleAlias as

        'just alias:
            try Dict.get alias pars.currentImports.moduleAliasToLocation as

                'nothing:
                    [
                    , "I cannot find the module: " .. alias
                    ]
                    >> pars.makeError
                    >> 'err

                'just location:
                    resolveLocation pars location maybeReferencedModuleAlias referencedName

        'nothing:
            try Dict.get referencedName pars.currentImports.globalNameToLocation as
                'nothing: 'USR pars.currentModule referencedName >> 'ok
                'just location: resolveLocation pars location maybeReferencedModuleAlias referencedName


resolveLocation as fn ResolvePars error, Location, Maybe Name, Name: Result error USR =
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
            #        $sourceDir/         <---------- We need to find out this
            #            $modulePath

            pars.loadExports importsPath
            >> Result.onOk fn modulesByPath:
                try Dict.get modulePath modulesByPath as

                    'nothing:
                        try maybeReferencedModuleAlias as

                            'just referencedModuleAlias:
                                [
                                , "imports.sp translates `$referencedModuleAlias` as `$modulePath`"
                                , "However, library $directoryPathOfLibrary does not expose any $modulePath module"
                                ]
                                >> pars.makeError
                                >> 'err

                            'nothing:
                                [
                                , "TODO ??????"
                                ]
                                >> pars.makeError
                                >> 'err

                    'just moduleUsrByName:
                        try Dict.get referencedName moduleUsrByName as

                            'just exportOptions:
                                'ok exportOptions.usr

                            'nothing:
                                try maybeReferencedModuleAlias as

                                    'just referencedModuleAlias:
                                        [
                                        , "imports.sp translates `$referencedAlias` as `$modulePath`"
                                        , "However, $modulePath in library $directoryPathOfLibrary does not expose any $referencedName"
                                        ]
                                        >> pars.makeError
                                        >> 'err

                                    'nothing:
                                        [
                                        , "TODO ?????!!!!"
                                        ]
                                        >> pars.makeError
                                        >> 'err
