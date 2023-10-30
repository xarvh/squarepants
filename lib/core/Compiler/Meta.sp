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

  To uniquely identify each module, we define three values: root, sourcePath, modulePath.
  The three modules in the example above have:

  *) Result.sp:
      root: 'core
      sourcePath: "corelib"
      modulePath: "Result"

  *) Main.sp:
      root: 'user
      sourcePath: "src"
      modulePath: "Main"

  *) Types/Person.sp:
      root: 'installed
      sourcePath: "library1/libs/library2/libs/library3/src"
      modulePath: "Types/Person"
#]


#
# This type defines the possible root locations of our code.
#
# It is designed to leave out absolute paths in the output, so that the output is independent on the location of the repo.
# At worst, the output should depend only on the directory structure of the project's directory.
#
var RootDirectory =
    , # Path is relative to $(compiler's executable)/lib
      'core
    , # Path is relative to $(project's root)
      'user
    , # Path is relative to $(project's root)/installedLibraries
      'installed


SourcePath =
    Text


# The modulePath is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
# ex: "Core/List"
ModulePath =
    Text


var DirectoryPath =
    , 'directoryPath RootDirectory SourcePath


#
# Unique Module Reference
#
# Uniquely identifies a module within the whole project.
#
UMR =
    , 'UMR DirectoryPath ModulePath



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
Imports =
    {
    , directoryPath as DirectoryPath

    # Tells us in which module a global is actually defined
    , globalNameToModuleAlias as Dict Name Name

    # Translates from aliases to paths
    , moduleAliasToDirOrLibrary as Dict Name DirOrLibrary
    #, modulePathToModuleAlias as Dict Name Name
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
# TODO, not this function's responsibility, but somewhere we want to check that global values do not overlap local values!
#
Pars =
    {
    #, currentImports as Imports
    , currentModule as UMR
    , loadImportsAndExports as fn CodeSource: Result Text (Imports & Exports)

    , maybeReferencedModuleAlias as Maybe Name
    , referencedName as Name
    , joinPath as fn [Text]: Text
    }





[#



ProjectImports
--------------
      sourceDirectory =
          path = src/

          module =
              path = Types/User
              visibleAs = User


      library =
          source = libs/a/

          module =
              path = User
              visibleAs = AUser


      library =
          source = core

          module =
              path = Result
              visibleAs = Result
              globals = onOk


aLibImport
----------

      sourceDirectory =
          path = src/

          module =
              path = Types/User
              visibleAs = User


      library =
          source = git:somethingsomething.blah

          module =
              path = Blah
              visibleAs = Blah
              globals = meh


      library =
          source = core

          module =
              path = Result
              visibleAs = Result
              globals = onOk


project root
  /installedLibraries
    /someLibrary
      imports.sp
      /libs
        /someUserLibrary
          imports.sp
          /libs
            ..ad infinitum






project module
--------------


      library =
          source = libs/a/

          module =
              path = User
              visibleAs = AUser



        AUser ----> UMR?



        {
        , codeRoot =
            if imports is from an installed library then
                installed root
            else
                project root

        , sourcePath =
            if imports is from an installed library then
                ...




        # The modulePath is also what the user writes in the code to refer to a non-aliased sourceDir module, so needs to look nice.
        # ex: "Core/List"
        , modulePath as Text
        }






#]







resolveSymbol as fn Pars: Result [Text] USR =
    fn pars:

    try pars.maybeReferencedModuleAlias as
        'just alias:
            'just alias

        'nothing:
            Dict.get pars.referencedName currentImports.globalNameToModuleAlias

    >> try __ as
        'nothing:
            'USR pars.currentModule pars.referencedName
            >> 'ok

        'just referencedAlias:

            try Dict.get referencedAlias currentImports.moduleAliasToSourceAndPath as

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


                'just ('sourceDirectory umr):
                    'USR umr pars.referencedName
                    >> 'ok


                'just ('library libraryPath modulePath):

                    # We don't have the $sourceDirectory path within the library, so we need to load its imports.sp
                    #
                    #    $libraryPath/
                    #        imports.sp
                    #        $sourceDirectory/
                    #            $modulePath
                    #
                    #
                    # We also need to check exports.sp, to ensure that the requested symbol is actually exposed
                    #
                    # Note that a library cannot export modules from its own libraries!


                    else this is a library, we need to know the sourceDirectory within the library, so we need to load the library's imports.sp
                    also, we need to check the library's exports.sp

                    'USR { pars.currentModule with modulePath } pars.referencedName
                    >> 'ok


#                'just ('library librarySource):
#                    pars.loadImportsAndExports librarySource
#                    >> onOk fn libraryImports & libraryExports:
#
#                    todo "ensure that libraryExports contains module and name"
#
#                    resolveSymbol
#                        { pars with
#                        , currentImports = libraryImports
#                        , maybeReferencedModuleAlias = 'just referencedAlias
#                        }






usrToFullPath as fn { coreLib as Text, project as Text, installed as Text }, USR: [Text] =
    fn basePath, 'USR ('UMR codeSource modulePath) name:

    try codeSource as
        'core sourcePath: [ basePath.coreLib, sourcePath, modulePath, name ]
        'user sourcePath: [ basePath.project, sourcePath, modulePath, name ]
        'installed sourcePath: [ basePath.installed, sourcePath, modulePath, name ]



#ProjectEnvironment =
#    {
#    , libraryImportsBySource as Dict Source Imports
#    , mainImports as Imports
#    , platformSource as Source
#    }
