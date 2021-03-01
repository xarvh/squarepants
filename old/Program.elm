module Main exposing (..)

import Compiler.FindUndeclared exposing (..)
import Compiler.FormattableToCanonicalAst


{-| Something used in the Canonical AST to refer to a type or value
-}
type alias Reference =
    String


{-| A simple value or type name

map

True

Maybe

-}
type alias Name =
    String


{-| An absolute module name

Platform

List/Extra

-}
type alias ModuleName =
    String


{-| An absolute path to a type or value?
-}
type alias Path =
    ( ModuleName, Name )


{-| Can't contain `/`

List\_Extra

-}
type alias ModuleAlias =
    Name


type alias Meta =
    { moduleAlias : Dict ModuleAlias ModuleName
    , globals : Dict Name ModuleName
    }


pathToModule : Meta -> Path -> Maybe ModuleName
pathToModule meta ( moduleReference, baseName ) =
    if moduleReference == "" then
        -- it must be a global
        Dict.get baseName meta.globals

    else
        -- TODO: if it contains any `/` it can't be a module alias, so we can skip this attempt
        Dict.get moduleReference moduleAlias
            |> Maybe.withDefault moduleReference


pathToUniqueId : Meta -> Path -> String
pathToUniqueId meta path =
    let
        ( _, baseName ) =
            path

        moduleName =
            path
                |> pathToModule meta
                |> Maybe.withDefault ""
    in
    moduleName ++ baseName



{-

   missingModules
     missing = whatever module contains main

     pick first missing module

     once no more missing modules, for each module:
         - replace aliases
         - run type inference

         - replace internal root refs with absolute refs
         - track the individual dependencies of every value
         [future] cache

     start from `Module.Whatever.main`
       recursively collect all values and unions required

     emit

-}


type alias State =
    { modulesToLoad : Dict ModuleName WhoNeedsIt
    , loadedModules : Dict ModuleName Module
    , static :
        { targetValues : List Path
        , meta : Meta
        }
    }


type alias WhoNeedsIt =
    List ModuleName


{-|

         - load, tokenize, parse, canonicize
         - find and store exposed and undeclared
         - check that exposed contains all exposed globals via meta
         - for each undeclared, find which module exposes it
         - if there is no module or no global, produce an error
         - if there is a module and it's not loaded, add it to missing
         ? store the set of module, types and value dependencies?

-}
addMissingModule : Name -> String -> Meta -> State -> Res State
addMissingModule moduleName code state =
    let
        caModuleResult =
            code
                |> Compiler.StringToTokens.lexer
                |> Result.andThen Compiler.TokensToFormattableAst.parse
                |> Result.andThen Compiler.FormattableToCanonicalAst.translateModule
    in
    do caModuleResult <| \caModule ->
    do (Compiler.FindUndeclared.moduleUndeclared caModule) <| \undeclared ->
    -- This also tests that all referenced globals and modules are available in meta
    do (findRequiredModules meta undeclared) <| \requiredModules ->
    do (checkThatMetaReferencesActuallyExistInTheModule caModule) <| \_ ->
    let
        addMissingModule missingModuleName modulesToLoad =
            if Dict.member missingModuleName state.loadedModules then
                modulesToLoad

            else
                -- moduleName needs missingModuleName
                Dict.update missingModuleName (Maybe.withDefault [] >> (::) moduleName >> Just) modulesToLoad
    in
    -- TODO? store the set of module, types and value dependencies?
    Ok
        { modulesToLoad = Set.foldl addMissingModule state.modulesToLoad requiredModules
        , loadedModules = Dict.insert moduleName caModule state.loadedModules
        }


initState : Meta -> List ( ModuleName, Value ) -> State
initState meta targets =
    { modulesToLoad = List.foldl (\moduleName -> Dict.insert moduleName []) Dict.empty targets
    , loadedModules = Dict.empty
    , static =
        { targetValues = []
        , meta = meta
        }
    }


putEverythingTogether : State -> Res (Either ModuleName Program)
putEverythingTogether state =
    let
        maybeModuleToLoad =
            state.modulesToLoad
                |> Dict.keys
                |> List.head
    in
    case maybeModuleToLoad of
        Just moduleName ->
            Ok (Left moduleName)

        Nothing ->
            {-
               build a dict of all types
               resolve aliases in the dict
               replace aliases in modules
            -}
            todo


type alias TypesDict =
    Dict Path TypeEntry


type alias TypeEntry =
    { originalDefinition : TypeOrUnion
    , expandedDefinition : TypeOrUnion
    }



{-
   for every module

     gather all aliases
       normalize names?
       use a dict to translate local aliases to full path names?


     for every alias

-}


resolveAlias : Meta -> Set Path -> CA.AliasDef -> State -> ( NA.AliasDef, State )
resolveAlias meta pending caAlias state =
    xxx


expandType : Meta -> Set Path -> Path -> State -> ( Type, State )
expandType meta pending typePath state =
    let
        ( moduleName, typeName ) =
            typePath

        normalizedModuleNameResult =
            if moduleName == "" then
                case ( Dict.get typeName meta.globals, Dict.get typeName currentModule.aliases ) of
                    ( Nothing, Nothing ) ->
                        Err "TODO type not found"

                    ( Just global, Just local ) ->
                        Err "TODO name clash with global..."

                    ( Just global, Nothing ) ->
                        Ok global

                    ( Nothing, Just local ) ->
                        Ok local

            else
                case Dict.get moduleName meta.moduleAlias of
                    Nothing ->
                        Ok moduleName

                    Just actualModuleName ->
                        Ok actualModuleName
    in
    do normalizedModuleNameResult <| \normalizedModuleName ->
      -- has type been expanded already>




      if normalizedModuleName == "Core" then
        -- core type, no expansion needed
        ( (normalizedModuleName, typeName), state )
      else





    xxxx



{-

       undeclaredResult =
         caModuleResult
           -- we'll check that undeclared are annotated later
           |> Result.andThen
     in

     case Result.map2 Tuple.pair caModuleResult undeclaredResult of
       Err e ->
         Err e
       Ok (caModule, undeclared) ->
       let
           -- meta says this module should expose stuff it doesn't
           missingExposed = missingFromExposition meta moduleName exposed
       in
       if missingExposed /= Set.empty then
         -- also give the line where in meta that exposes the stuff
         Err <| MissingExposed missingExposed
       else
         let
             (requiredModules, unavailableModuleOrGlobal) =
               undeclared
                 |> List.partition (findModuleForSymbol meta)
         in
             if unavailable /= [] then
               Err <| UnavailableModuleOrGlobal unavailableModuleOrGlobal
             else













   - initState = { ... ,  missing = Set.singleton "SomeModule.main" }

   - iteration:
       - pick first `missing`
       - if in `available`
         - remove from `missing`
         else
         - getFile : Meta -> Path -> Maybe FileReference
         - if file not in `loadedModules`
             - load, tokenize, parse, canonicize
             - find undeclared, add them to `missing`
             - add exported to `available`
       # do I need both `available` and `missing`?

   - once `missing` is empty:
       - replace aliases
       - replace refs internal to the module with full paths
         (probably no need to replace global refs? What about global refs in libs?)
       - (also track all references contained each value so that we can tree shake?)
       - run type inference

   - build program


   - cacheable run: deals with *modules*
   - build run: deals in *values*





















   missingValues








-}
{-
   - take first missing

   - use meta to determine which module provides it
       [Future: if it's a library, merge the library meta with the current meta? ]

   - get module
       - if it's loaded in memory, return it
         else
           read file
           [Future] we CACHE all of:
             tokenize
             parse
             canonicize
             apply aliases
             find undeclared
             type inference
           normalize
           return


   CACHE
     -> It doesn't matter right now, because I don't know exactly *what* to cache.
     What are the bottlenecks that need caching?

     * This is crucial for build speed, and it can mess with everything else, so I don't want to skip it.

     * Cache exists per module
     * Cache is invalidated when
         - the module content changes
         - the compiler version changes
         - modules.toml changes
         - dependencies (ie, the "undeclareds") disappear or change type

     * The first three parts are trivial to figure out

       What about the last one?
         I will not know the dependencies until I actually compile the module, which kind of invalidates everything?

         ---> How do I know the dependencies before compiling the module?

         I need to save them together with the cache

         Does this mean that if I compile the same module with different dependencies I need to recompile it every time?

     * different approach
         - cache is loaded regardless of dependencies/undeclareds


         - First you load all modules, cached
         - Then you determine the types



     Main
       main (deps on: view)

     View
       view (deps on: button)

     Button
       button



     entry point
       > add `main` to missing

     main is cached, so
       > load cached version
       > insert `main` in loaded

     loaded main sez it needs `view`
     view is cached so
       > load cached version
       > insert `view` in loaded

     loaded view sez it needs `button`
     button has changed
       > recompile `button`

     button types has changed
       mark all modules that have dependencies on `button` as in need of type check

     once you loaded everything
       type check again all marked modules


     to do this I must
       1. be able to know which modules need what, and their type
       2. be able to run type inference on a cached module
           which means: cached module must be in a format that works with type inference

     What happens when a TYPE is redefined!?
       -> that DOES change the **interface** of a module
       -> the modules that expose that type should also be considered changed
       -> which in turn means I need to find all modules that depend on these changed modules?

     What if an /alias/ changes/?
     I need to recompile all modules that use that



     each cached module should come with
       - the external values and types it uses internally

       - the values it exposes, each with the external types it uses in its annotation (if any)

       type alias CachedModule
         externalValuesUsed : Dict Name Type
         externalTypesUsed : Set Name



-}


getFile : Meta -> Path -> Maybe FileReference
getFile meta path =
    Nothing


type alias Name =
    String


type alias Statement =
    CA.Statement ( Location, CA.Type )


type alias NormalizedValue =
    { name : Name
    , referencedValues : Set Name
    , body : List Statement
    }


type alias BuildState =
    { unionCons : Dict Path { typeName : Name, args : List CA.Type }
    , reversedDefs : List ( Name, List (CA.Statement Ext) )
    , defined : Set Path
    , missing : Set Path
    , loadedModules : Dict Name NormalizedModule
    , meta : Meta
    }


addMissingConstructor : Name -> Name -> List CA.Type -> ProgramState -> ProgramState
addMissingConstructor name ty args state =
    { state
        | unionCons = Dict.insert name { typeName = ty, args = args }
        , missing = Set.remove name state.missing
        , defined = Set.insert name state.defined
    }


addMissingValue : Path -> NormalizedValue -> ProgramState -> ProgramState
addMissingValue path value state =
    let
        missingHere =
            Set.diff value.referencedValues state.defined
    in
    { state
        | reversedDefs = ( path, value.body ) :: state.reversedDefs
        , defined = Set.insert path state.defined
        , missing =
            state.missing
                |> Set.union missingHere
                |> Set.remove path
    }
