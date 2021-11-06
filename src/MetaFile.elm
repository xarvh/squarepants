module MetaFile exposing (..)

import Compiler.Lexer
import Compiler.TokensToFormattableAst
import Dict exposing (Dict)
import SPON as M exposing (do, return)
import Types.Error as Error exposing (Res)
import Types.FormattableAst as FA
import Types.Meta exposing (Meta)


type alias MetaFile =
    { sourceDirs : List SourceDir
    , libraries : List Library
    }


initMetaFile : MetaFile
initMetaFile =
    { sourceDirs = []
    , libraries = []
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


stringToMeta : String -> Res Meta
stringToMeta =
    stringToMetaFile "modules.sp" >> Result.map toMeta


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
--- Reader
--


type RootEntry
    = Lib Library
    | Dir SourceDir


moduleReader : M.Reader Module
moduleReader =
    M.do (M.field "path" M.varName) <| \path ->
    M.do (M.maybe <| M.field "importAs" M.varName) <| \importAs ->
    M.do (M.maybe <| M.field "globalTypes" (M.many M.varName)) <| \globalTypes ->
    M.do (M.maybe <| M.field "globalValues" (M.many M.varName)) <| \globalValues ->
    M.return
        { path = path
        , importAs = Maybe.withDefault path importAs
        , globalTypes = Maybe.withDefault [] globalTypes
        , globalValues = Maybe.withDefault [] globalValues
        }


libraryReader : M.Reader Library
libraryReader =
    M.do (M.field "source" M.string) <| \source ->
    M.do (M.many <| M.field "module" moduleReader) <| \modules ->
    M.return
        { source = source
        , modules = modules
        }


sourceDirectoryReader : M.Reader SourceDir
sourceDirectoryReader =
    M.do (M.field "path" M.string) <| \path ->
    M.do (M.many <| M.field "module" moduleReader) <| \modules ->
    M.return
        { path = path
        , moduleExceptions = modules
        }


metaFileReader : M.Reader (List RootEntry)
metaFileReader =
    [ M.do (M.field "library" libraryReader) <| \lib -> M.return <| Lib lib
    , M.do (M.field "sourceDir" sourceDirectoryReader) <| \dir -> M.return <| Dir dir
    ]
        |> M.oneOf
        |> M.many


stringToMetaFile : String -> String -> Res MetaFile
stringToMetaFile sponName sponContent =
    let
        insert : RootEntry -> MetaFile -> MetaFile
        insert rootEntry metaFile =
            case rootEntry of
                Lib lib ->
                    { metaFile | libraries = lib :: metaFile.libraries }

                Dir dir ->
                    { metaFile | sourceDirs = dir :: metaFile.sourceDirs }

        statementsToResMetaFile : List FA.Statement -> Res MetaFile
        statementsToResMetaFile statements =
            case M.run metaFileReader statements of
                Err message ->
                    Error.errorTodo message

                Ok rootEntries ->
                    Ok <| List.foldl insert initMetaFile rootEntries
    in
    sponContent
        |> Compiler.Lexer.lexer sponName
        |> Result.andThen (Compiler.TokensToFormattableAst.parse sponName sponContent)
        |> Result.andThen statementsToResMetaFile
