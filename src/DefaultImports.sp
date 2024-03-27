defaultImportsFile as ImportsFile =
    {
    , libraries =
        [
        , {
        , modules =
            [
            , mod
                "Core"
                [
                , "None"
                , "Bool"
                , "Text"
                , "Number"
                , "'none"
                , "'true"
                , "'false"
                , "mut"
                ]
            , mod
                "Basics"
                [
                , "Int"
                , "assert"
                , "clamp"
                , "identity"
                , "modBy"
                , "min"
                , "max"
                , "cloneImm"
                , "cloneUni"
                ]
            , mod "Text" []
            , mod "Tuple" []
            , mod "Debug" [ "log", "todo" ]
            , mod "Self" []
            , mod "List" []
            , mod "Dict" [ "Dict" ]
            , mod "Array" [ "Array" ]
            , mod "Hash" [ "Hash" ]
            , mod "Set" [ "Set" ]
            , mod "Maybe" [ "Maybe", "'just", "'nothing" ]
            , mod "Result" [ "Result", "'ok", "'err" ]
            ]
        , platform = ""
        , source = "core"
        }
        ]
    , sourceDirs =
        [
        , {
        , modules = []
        , path = "."
        }
        , {
        , modules = []
        , path = "src/"
        }
        ]
    }


mod as fn Text, [ Text ]: ImportsFile.Module =
    fn path, globals:
    {
    , globals
    , path
    , visibleAs = path
    }


platformDefaultImportsFile as fn Text, [ Text & [ Text ] ]: ImportsFile =
    fn name, modules:
    platform as ImportsFile.Library =
        {
        , modules = List.map (fn path & globals: mod path globals) modules
        , platform = ""
        , source = ":" .. name
        }

    { defaultImportsFile with libraries = [ platform, .libraries... ] }


asText__ as Text =
    """
    library =
        source = "core:prelude"

        #
        # Compiler stuff
        #

        module =
           path = Compiler/Ast
           importAs = Ast
           globalTypes =
              Name
              Ref
              UnivarId
              Uniqueness
           globalValues =
              'refLocal
              'refGlobal
              'refPlaceholder
              'uni
              'imm
              'depends
              toImm
              toUni

        module =
           path = Compiler/TypedAst
           importAs = TA

        module =
           path = Compiler/Platform
           importAs = Platform
           globalTypes =
              Platform

        module =
           path = Compiler/CanonicalAst
           importAs = CA

        module =
           path = Compiler/EmittableAst
           importAs = EA

        module =
           path = Compiler/FormattableAst
           importAs = FA

        module =
            path = Compiler/Meta
            importAs = Meta
            globalTypes =
                Meta
                ByUsr
                USR
                UMR
                LibrarySource
                DependencyType
            globalValues =
                'USR
                'UMR
                'valueDependency
                'constructorDependency
                'typeDependency

        module =
            path = Compiler/Op
            importAs = Op

        module =
           path = Compiler/Pos
           importAs = Pos
           globalTypes =
                Pos
                At
           globalValues =
                'at

        module =
            path = Compiler/Token
            importAs = Token
            globalTypes =
                Token
            globalValues =
                'token

    """
