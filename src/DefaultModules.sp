# TODO using this also for Core libraries.
# This is wrong, we should have a separate modules as defaul and one for Core.
asText as Text =
    """
    library =
        source = "core:prelude"

        module =
            path = Core
            importAs = Core
            globalTypes =
                None
                Bool
                Text
                Number
            globalValues =
                'none
                'true
                'false
                mut

        module =
            path = List
            importAs = List

        module =
            path = Maybe
            importAs = Maybe
            globalTypes =
                Maybe
            globalValues =
               'just
               'nothing

        module =
            path = Text
            importAs = Text

        module =
            path = Tuple
            importAs = Tuple

        module =
            path = Debug
            importAs = Debug
            globalValues =
                log
                todo

        module =
            path = Basics
            globalTypes =
                Int
            globalValues =
                assert
                clamp
                identity
                modBy
                min
                max
                cloneImm
                cloneUni

        module =
            path = Dict
            importAs = Dict
            globalTypes = Dict

        module =
            path = Self

        module =
            path = Array
            globalTypes = Array

        module =
            path = Hash
            globalTypes = Hash

        module =
            path = Set
            importAs = Set
            globalTypes = Set

        module =
            path = Result
            importAs = Result
            globalTypes = Result
            globalValues =
                'ok
                'err


        #
        # Compiler stuff
        # TODO Really shouldn't be all here
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

    # TODO remove this, should be only for Core
    sourceDir =
        path = "src"

        module =
           path = SPLib/Test
           importAs = Test
           globalTypes = Test

    """
