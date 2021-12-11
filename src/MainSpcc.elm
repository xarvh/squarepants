module MainSpcc exposing (program)

import Compiler.ApplyAliases
import Compiler.CanonicalToJs
import Compiler.JsToString
import Compiler.Pipeline
import Compiler.TestHelpers
import Compiler.TypeCheck as TC
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


skippedModules =
    Debug.log "skippedModules"
        [ ""
        , "sp/Compiler/Lexer_Test.sp"
        , "sp/Compiler/MakeCanonical_Test.sp"
        , "sp/Compiler/Parser_Test.sp"
        , "sp/Compiler/TypeCheck_Test.sp"
        , "sp/SPCore/Dict_Test.sp"
        , "sp/SPCore/List_Test.sp"

        --         , "sp/Compiler/MakeCanonical.sp"
        , "sp/Compiler/TypeCheck.sp"

        --         ,  "sp/Compiler/Parser.sp"
        --         ,  "sp/Compiler/TestHelpers.sp"
        --         , "sp/Compiler/CoreTypes.sp"
        --         , "sp/Compiler/Error.sp"
        --         , "sp/Compiler/Lexer.sp"
        --         ,  "sp/DefaultModules.sp"
        --         , "sp/Human/CanonicalAst.sp"
        --         ,  "sp/Main.sp"
        --         ,  "sp/ModulesFile.sp"
        --         ,  "sp/Prelude.sp"
        --         ,  "sp/SPCore/Basics.sp"
        --         ,  "sp/SPCore/Dict.sp"
        --         ,  "sp/SPCore/List.sp"
        --         ,  "sp/SPCore/Maybe.sp"
        --         ,  "sp/SPCore/Result.sp"
        --         ,  "sp/SPCore/Set.sp"
        --         ,  "sp/SPCore/Text.sp"
        --         ,  "sp/SPCore/Tuple.sp"
        --         ,  "sp/SPLib/Buffer.sp"
        --         ,  "sp/SPLib/Parser.sp"
        --         ,  "sp/SPON.sp"
        --         ,  "sp/StateMonad.sp"
        --         ,  "sp/Test.sp"
        --         ,  "sp/Types/CanonicalAst.sp"
        --         ,  "sp/Types/FormattableAst.sp"
        --         ,  "sp/Types/Meta.sp"
        --         ,  "sp/Types/Op.sp"
        --         ,  "sp/Types/Pos.sp"
        --         ,  "sp/Types/Token.sp"
        ]


metafileName =
    "modules.sp"



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
    do (Posix.IO.File.readDir fullPath |> Posix.IO.exitOnError identity) <|
        \entries ->
            loadAllEntries fullPath entries accum


loadAllEntries : String -> List Posix.IO.File.Entry -> Dict String String -> IO (Dict String String)
loadAllEntries path entries accum =
    case entries of
        [] ->
            return accum

        head :: tail ->
            --
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
                if List.member fullPath skippedModules then
                    return accum

                else
                    do (loadFile fullPath) <|
                        \content ->
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
    do (loadDirectory path Dict.empty) <|
        \filesByPath ->
            filesByPath
                |> Dict.foldl insertFile Dict.empty
                |> return


loadAllSourceDir : List String -> ModuleByName -> IO ModuleByName
loadAllSourceDir dirs accum =
    case dirs of
        [] ->
            return accum

        head :: tail ->
            do (loadSourceDir head) <|
                \modulesByName ->
                    loadAllSourceDir tail (Dict.union modulesByName accum)



--


loadMetaFile : IO MetaFile
loadMetaFile =
    do (loadFile metafileName) <|
        \string ->
            case MetaFile.stringToMetaFile metafileName string of
                Err err ->
                    let
                        errorEnv : Types.Error.ErrorEnv
                        errorEnv =
                            { moduleByName = Dict.singleton metafileName { fsPath = metafileName, content = string }
                            }
                    in
                    err
                        |> Compiler.TestHelpers.errorToString errorEnv
                        |> exit

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
            { moduleByName = files
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
    do (Lib.dict_foldRes compileAndInsert files Prelude.prelude) <|
        \allDefs ->
            let
                withAliases : Result String CA.AllDefs
                withAliases =
                    allDefs
                        |> Compiler.ApplyAliases.applyAliasesToModule
                        |> Result.mapError (Compiler.TestHelpers.errorToString errorEnv)
            in
            do withAliases <|
                \alsDefs ->
                    let
                        blah : Result String TC.Env
                        blah =
                            let
                                _ =
                                    Debug.log "typechecking..." ()
                            in
                            alsDefs
                                |> TC.allDefsToEnvAndValues
                                |> (\( env, values ) -> TC.fromAllValueDefs env values)
                                |> Result.mapError (Compiler.TestHelpers.errorToString errorEnv)
                    in
                    do blah <|
                        \env ->
                            alsDefs
                                |> Compiler.CanonicalToJs.translateAll errorEnv
                                |> List.map (Compiler.JsToString.emitStatement 0)
                                |> (++) [ Compiler.CanonicalToJs.nativeDefinitions ]
                                |> String.join "\n\n"
                                |> (\s -> s ++ "const fs = require('fs');\n")
                                --|> (\s -> s ++ "fs.readFile(process.argv[2] || '', (err, file) => console.log($Main$main(err ? '' : file.toString())))")
                                |> (\s -> s ++ """
                                       const out = $Main$main({})(array_toList(process.argv.slice(1)))[1]('never');
                                       if (out[1]) console.error(out[1]);
                                       """)
                                |> Ok



----
--- Main
--


program : Process -> IO ()
program process =
    -- argv[0]: MainSpcc.elm
    -- argv[1]: --debug
    case List.drop 2 process.argv of
        outFile :: tail ->
            do loadMetaFile <|
                \metaFile ->
                    do (loadAllSourceDir (List.map .path metaFile.sourceDirs) Dict.empty) <|
                        \modsByName ->
                            case makeProgram metaFile modsByName of
                                Err err ->
                                    exit <| err ++ "\n"

                                Ok js ->
                                    Posix.IO.File.writeContentsTo (Debug.log "" outFile) js

        _ ->
            exit "no output file specified"
