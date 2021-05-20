module MetaFile exposing (..)

-- import Compiler.StringToTokens
-- import Compiler.TokensToFormattableAst
-- import Types.FormattableAst as FA

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Types.Meta exposing (Meta)



-- TODO importAs -> visibleAs ?


type alias MetaFile =
    { sourceDirs : List SourceDir
    , libraries : List Library
    }


type alias SourceDir =
    { path : String
    , moduleExceptions : List Module
    }


type alias Library =
    { source : String
    , modules : List Module
    }


type alias Module =
    { path : String
    , importAs : String
    , globalValues : List String
    , globalTypes : List String
    }



----
---
--


stringToMeta : String -> Result String Meta
stringToMeta =
    stringToMetaFile >> Result.map toMeta


stringToMetaFile : String -> Result String MetaFile
stringToMetaFile json =
    json
        |> D.decodeString fileDecoder
        |> Result.mapError D.errorToString


toMeta : MetaFile -> Meta
toMeta metaFile =
    [ List.concatMap .modules metaFile.libraries
    , List.concatMap .moduleExceptions metaFile.sourceDirs
    ]
        |> List.concat
        |> List.foldl insertModule Types.Meta.init


insertModule : Module -> Meta -> Meta
insertModule mod meta =
    -- TODO fail if varName is used already
    { globalValues = List.foldl (\varName -> Dict.insert varName (mod.path ++ "." ++ varName)) meta.globalValues mod.globalValues
    , globalTypes = List.foldl (\varName -> Dict.insert varName (mod.path ++ "." ++ varName)) meta.globalTypes mod.globalTypes

    -- TODO fail if importAs is used already
    -- TODO ignore if importAs and path are the same
    -- TODO ensure that importAs is well-formed
    , bynames = Dict.insert mod.importAs mod.path meta.bynames
    }



----
--- JSON
--


do a b =
    D.andThen b a


fileDecoder : Decoder MetaFile
fileDecoder =
    do (D.field "sourceDirs" <| D.list sourceDirDecoder) <| \sds ->
    do (D.field "libraries" <| D.list libraryDecoder) <| \libs ->
    D.succeed
        { sourceDirs = sds
        , libraries = libs
        }


sourceDirDecoder : Decoder SourceDir
sourceDirDecoder =
    do (D.field "path" D.string) <| \path ->
    do (D.maybe <| D.field "moduleExceptions" <| D.list moduleDecoder) <| \moduleExceptions ->
    D.succeed
        { path = path
        , moduleExceptions = Maybe.withDefault [] moduleExceptions
        }


libraryDecoder : Decoder Library
libraryDecoder =
    do (D.field "source" D.string) <| \source ->
    do (D.field "modules" <| D.list moduleDecoder) <| \modules ->
    D.succeed
        { source = source
        , modules = modules
        }


moduleDecoder : Decoder Module
moduleDecoder =
    do (D.field "path" D.string) <| \path ->
    do (D.maybe <| D.field "importAs" D.string) <| \maybeImportAs ->
    do (D.maybe <| D.field "globalValues" <| D.list D.string) <| \maybeGlobalValues ->
    do (D.maybe <| D.field "globalTypes" <| D.list D.string) <| \maybeGlobalTypes ->
    D.succeed
        { path = path
        , importAs = Maybe.withDefault "" maybeImportAs
        , globalValues = Maybe.withDefault [] maybeGlobalValues
        , globalTypes = Maybe.withDefault [] maybeGlobalTypes
        }
