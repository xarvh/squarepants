module Compiler.PutEverythingTogether exposing (..)

import Lib



{-
-- can I crawl when building ca ast?
    -> it's probably easy to normalize names
    -> it's probably a pain in the ass to collect references

-}


----
--- Lib
--


untilEmpty : (state -> List a) -> (List a -> state -> state) -> (a -> state -> Result error state) -> state -> Result error state
untilEmpty getList setList apply state =
    case getList state of
        [] ->
            Ok state

        head :: tail ->
            do (f head (setList tail state)) <| \newState ->
            untilListIsEmpty getList setList apply newState



----
---
--




type alias WhoNeedsIt =
    FullName


type Error
    = ErrorModuleNotFound WhoNeedsIt FullName
    | ErrorDefNotFoundInModule WhoNeedsIt FullName


type alias Res a =
    Result Error a



----
---
--


getNormalizedName : Meta -> ModuleName -> FullName -> FullName
getNormalizedName meta hostModName rawName =
    if rawName.mod == "" then
        case Dict.get rawName.def meta.globals of
            Nothing ->
                { mod = hostModName
                , def = rawName.def
                }

            Just globalFullName ->
                globalFullName

    else
        case Dict.get rawName.mod meta.shorthands of
            Nothing ->
                rawName

            Just modName ->
                { mod = modName
                , def = rawName.def
                }


getDefinition : WhoNeedsIt -> FullName -> State -> Res RootDefinition
getDefinition whoNeedsIt fullName state =
    case Dict.get fullName.mod state.loadedModules of
        Nothing ->
            ErrorModuleNotFound whoNeedsIt fullName

        Just mod ->
            case Dict.get fullName.def mod.defs of
                Nothing ->
                    ErrorDefNotFoundInModule whoNeedsIt fullName

                Just def ->
                    Ok def


flagAsMissing : State -> Blah
flagAsMissing state normalizedRef =
    if Dict.member normalizedRef state.done then
        Ok state

    else if cantBeCircular normalizedRef && Dict.member normalizedRef state.pending then
        Err "circular!"

    else
        Ok { state | missing = Dict.insert normalizedRef state.missing }



doOne : WhoNeedsIt -> FullName -> State -> Res State
doOne whoNeedsIt normalizedName state =
    do (getDefinition whoNeedsIt normalizedName state) <| \def ->
    do (crawlDefinition state.ro.meta def) <| \( normalizedRefs, normalizedDef ) ->
    Lib.list_foldlRes
        flagAsMissing
        normalizedRefs
        { state
            | done = Dict.insert normalizedName future_normalizedDef state.done
            , missing = Dict.remove normalizedName state.missing
        }


crawlDefinition : Meta -> RootDefinition -> (List FullName, RootDefinition)
crawlDefinition meta def =
  xxx




{-| given a list of entry points and a dict of modules, collect all visibles
collect : Meta -> Dict ModuleName (Dict LocalName Visible) -> List ( ModuleName, LocalName ) -> Res (Dict FullName) Visible
collect meta modules entryPoints =
let
xx =
x
in
xx

collectOne meta modules entryPoint collected =
do (getDefinition meta modules entryPoint) <| ( resolvedPath, def ) ->
Ok { collected | visibles = Dict.insert resolvedPath def collectedVisibles }

-}
