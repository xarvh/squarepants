module MainSpcc exposing (program)

import Compiler.ApplyAliases
import Compiler.CanonicalToJs
import Compiler.JsToString
import Compiler.Pipeline
import Compiler.TestHelpers
import Compiler.TypeInference as TI
import Dict exposing (Dict)
import Lib
import MetaFile exposing (MetaFile)
import Posix.IO exposing (IO, Process, do, return)
import Posix.IO.File
import Posix.IO.Process exposing (print)
import Prelude
import Types.CanonicalAst as CA exposing (Pos)
import Types.Error exposing (Res)
import Types.Meta exposing (Meta)



-- TODO this should go somewhere in Types and be used in ErrorEnv


type alias ModuleByName =
    Dict String { fsPath : String, content : String }



--


pathJoin : String -> String -> String
pathJoin a b =
    a ++ "/" ++ b


exit : String -> IO a
exit =
    Err >> return >> Posix.IO.exitOnError identity


loadFile : String -> IO String
loadFile =
    Posix.IO.File.contentsOf >> Posix.IO.exitOnError identity



-- Load every single file in a directory


loadDirectory : String -> Dict String String -> IO (Dict String String)
loadDirectory fullPath accum =
    do (Posix.IO.File.readDir fullPath |> Posix.IO.exitOnError identity) <| \entries ->
    loadAllEntries fullPath entries accum


loadAllEntries : String -> List Posix.IO.File.Entry -> Dict String String -> IO (Dict String String)
loadAllEntries path entries accum =
    case entries of
        [] ->
            return accum

        head :: tail ->
            do (loadEntry path head accum) (loadAllEntries path tail)


loadEntry : String -> Posix.IO.File.Entry -> Dict String String -> IO (Dict String String)
loadEntry path entry accum =
    case entry of
        Posix.IO.File.Other _ ->
            return accum

        Posix.IO.File.Directory dirName ->
            -- TODO also check that dirName starts with an uppercase
            loadDirectory (pathJoin path dirName) accum

        Posix.IO.File.File fileName ->
            -- TODO we should also check that length is > 3 and that the first char is uppercase
            if String.endsWith ".sp" fileName then
                let
                    fullPath =
                        pathJoin path fileName
                in
                do (loadFile fullPath) <| \content ->
                accum
                    |> Dict.insert fullPath content
                    |> return

            else
                return accum



--


fileToModuleName : String -> String -> String
fileToModuleName sourceDirPath fileNamePath =
    fileNamePath
        |> String.dropLeft (String.length sourceDirPath + 1)
        |> String.dropRight 3


loadSourceDir : String -> IO ModuleByName
loadSourceDir path =
    let
        insertFile fileName content =
            Dict.insert
                (fileToModuleName path fileName)
                { fsPath = fileName
                , content = content
                }
    in
    do (loadDirectory path Dict.empty) <| \filesByPath ->
    filesByPath
        |> Dict.foldl insertFile Dict.empty
        |> return


loadAllSourceDir : List String -> ModuleByName -> IO ModuleByName
loadAllSourceDir dirs accum =
    case dirs of
        [] ->
            return accum

        head :: tail ->
            do (loadSourceDir head) <| \modulesByName ->
            loadAllSourceDir tail (Dict.union modulesByName accum)



--


loadMetaFile : IO MetaFile
loadMetaFile =
    do (loadFile "sp.json") <| \string ->
    case MetaFile.stringToMetaFile string of
        Err err ->
            exit err

        Ok metaFile ->
            return metaFile



----
--- Pipeline
--


makeProgram : MetaFile -> ModuleByName -> Result String String
makeProgram metaFile files =
    let
        errorEnv : Types.Error.ErrorEnv
        errorEnv =
            { metaFile = metaFile
            , moduleByName = files
            }

        meta =
            MetaFile.toMeta metaFile

        do =
            Lib.result_do

        --compileAndInsert : String -> String -> CA.AllDefs -> Result String CA.AllDefs
        compileAndInsert moduleName { content } acc =
            content
                |> Compiler.TestHelpers.unindent
                |> (\fa -> Compiler.Pipeline.stringToCanonicalAst meta moduleName fa)
                |> Result.map (Dict.union acc)
                |> Result.mapError (Compiler.TestHelpers.errorToString errorEnv)
    in
    do (Lib.dict_foldRes compileAndInsert files Prelude.prelude) <| \allDefs ->
    let
        withAliases : Result String CA.AllDefs
        withAliases =
            allDefs
                |> Compiler.ApplyAliases.applyAliasesToModule
                |> Result.mapError (Compiler.TestHelpers.errorToString errorEnv)
    in
    do withAliases <| \alsDefs ->
    let
        blah : Result String ( CA.AllDefs, TI.Env, TI.Substitutions )
        blah =
            alsDefs
                |> TI.inspectModule Dict.empty
                |> Result.mapError (Compiler.TestHelpers.errorToString errorEnv)
    in
    do blah <| \( typedProgram, env, subs ) ->
    typedProgram
        |> Compiler.CanonicalToJs.translateAll subs
        |> List.map (Compiler.JsToString.emitStatement 0)
        |> (++) [ Compiler.CanonicalToJs.nativeDefinitions ]
        |> String.join "\n\n"
        |> (\s -> s ++ "console.log($Main$main())")
        |> Ok



----
--- Main
--


spcc : Process -> IO ()
spcc process =
    -- argv[0]: MainSpcc.elm
    -- argv[1]: --debug
    case List.drop 2 process.argv of
        outFile :: tail ->
            do loadMetaFile <| \metaFile ->
            do (loadAllSourceDir (List.map .path metaFile.sourceDirs) Dict.empty) <| \modsByName ->
            case makeProgram metaFile modsByName of
                Err err ->
                    exit err

                Ok js ->
                    Posix.IO.File.writeContentsTo (Debug.log "" outFile) js

        _ ->
            exit "no output file specified"


program : Posix.IO.PosixProgram
program =
    Posix.IO.program spcc
