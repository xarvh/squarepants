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


var ImportsDir =
    , 'importsDir RootDirectory Text


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
    , 'UMR ImportsDir SourceDir ModulePath


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

var ModuleLocation =
    , 'moduleLocationSourceDirectory UMR
    , 'moduleLocationLibrary ImportsDir ModulePath


Imports =
    {
    # Tells us in which module a global is actually defined
    , globalNameToModuleAlias as Dict Name Name
    , moduleAliasToDirOrLibrary as Dict Name ModuleLocation
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
Pars =
    {
    , currentImports as Imports
    , currentModule as UMR
    , loadExports as fn CodeSource: Result Text (Imports & Exports)
    , maybeReferencedModuleAlias as Maybe Name
    , referencedName as Name
    }


resolveSymbol as fn Pars: Result [ Text ] USR =
    fn pars:
    try pars.maybeReferencedModuleAlias as
        'just alias: 'just alias
        'nothing: Dict.get pars.referencedName currentImports.globalNameToModuleAlias
    >> try __ as

        'nothing:
            'USR pars.currentModule pars.referencedName >> 'ok

        'just referencedAlias:
            try Dict.get referencedAlias currentImports.moduleAliasToDirOrLibrary as

                'nothing:
                    try pars.maybeReferencedModuleAlias as

                        'nothing:
                            [
                            , "TODO currentImports" .. " says that `" .. pars.referencedName .. "` is a global from module `" .. referencedAlias
                            , "However I cannot find that module!"
                            ]
                            >> 'err

                        'just _:
                            [
                            , "Cannot find module `" .. referencedAlias .. "`"
                            ]
                            >> 'err

                'just ('moduleLocationSourceDirectory umr):
                    'USR umr pars.referencedName >> 'ok

                'just ('moduleLocationLibrary importsDirOfLibrary modulePath):
                    # We are missing the $sourceDir part of the UMR; this information is in the Imports of the library,
                    # but is integrated in the Exports when we load it.
                    #
                    #    $importsDirOfLibrary/
                    #        imports.sp
                    #        $sourceDir/
                    #            $modulePath

                    pars.loadExports importsDirOfLibrary
                    >> onOk fn libraryExports:
                    try Dict.get modulePath libraryExports.exposedModulesByPath as

                        'nothing:
                            [
                            , "imports.sp translates `$referencedAlias` as `$modulePath`"
                            , "However, library $directoryPathOfLibrary does not expose any $modulePath module"
                            ]
                            >> 'err

                        'just exposedModule:
                            try Dict.get referencedName exposedModule.exposedUsrByName as

                                'nothing:
                                    [
                                    , "imports.sp translates `$referencedAlias` as `$modulePath`"
                                    , "However, $modulePath in library $directoryPathOfLibrary does not expose any $referencedName"
                                    ]
                                    >> 'err

                                'just usr:
                                    'ok usr


usrToFullPath as fn { coreLib as Text, installed as Text, project as Text }, USR: [ Text ] =
    fn { coreLib, installed, project }, 'USR ('UMR ('importsDir rootDir importsDir) sourceDir modulePath) name:
    path =
        [ rootDir, importsDir, modulePath, name ]

    try rootDir as
        'core: [ coreLib, path... ]
        'user: [ project, path... ]
        'installed: [ installed, path... ]
