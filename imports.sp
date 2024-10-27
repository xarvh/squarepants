[# imports.sp uses the same syntax as normal SP files, which means all tools such as syntax highlight and formatting will work. #]

#
# Platform
#
library =
    platform = posix
    source = ":posix"

    module =
       path = IO
       globals = IO

    module =
       path = Path


sourceDir =
    path = "specs"


sourceDir =
    path = "src"

    module =
        path = ImportsFile
        globals =
            ImportsFile

    module =
        path = ExportsFile
        globals =
            ExportsFile

    module =
        path = Human/Type
        globals =
            usrToText

    module =
       path = SPLib/SPON
       importAs = SPON

    module =
       path = Targets/Javascript/Ast
       importAs = JA

    module =
       path = Platform
       importAs = Platform
       globals =
          Platform


#
# Libraries
#
library =
    source = ":test"
    module =
       path = Test
       importAs = Test
       globals = Test


library =
    source = ":dependencyResolution"

    module =
       path = RefHierarchy_Test


library =
    source = ":format"
    module =
       path = Format
       importAs = Fmt

    module =
       path = Format_Test
       importAs = Fmt_Test


library =
    source = ":compiler"

    module =
       path = Types/Ast
       importAs = Ast
       globals =
          Ref
          'refLocal
          'refGlobal
          'refPlaceholder

    module =
       path = Types/TypedAst
       importAs = TA

    module =
       path = Types/CanonicalAst
       importAs = CA

    module =
       path = Types/FormattableAst
       importAs = FA

    module =
        path = Types/Meta
        importAs = Meta
        globals =
            Imports
            Exports
            ByUsr
            USR
            UMR
            Source
            DependencyType
            'USR
            'UMR
            'valueDependency
            'constructorDependency
            'typeDependency

    module =
        path = Types/Op
        importAs = Op

    module =
       path = Types/Pos
       importAs = Pos
       globals =
            Pos
            At
            'at

    module =
        path = Types/Token
        importAs = Token
        globals =
            Token
            'token

    module =
       path = Compiler/LazyBuild

    module =
       path = Compiler/Error
       importAs = Error
       globals =
          Error
          Res

    module =
       path = Compiler/Parser

    module =
       path = Compiler/CoreDefs
       importAs = CoreDefs

    module =
       path = Compiler/MakeCanonical

    module =
       path = Compiler/MakeEmittable

    module =
       path = Human/Format


#
# Core
#
library =
    source = "core"

    module =
       path = Compiler/EmittableAst
       importAs = EA
       globals =
          Name
          UnivarId
          Uniqueness
          'uni
          'imm
          'depends
          toImm
          toUni


    # unlike sourceDirs, libraries don't automatically expose all available modules
    module =
        path = Array_Test
    module =
        path = Dict_Test
    module =
        path = Hash_Test
    module =
        path = List_Test
    module =
        path = Self_Test

    module =
       path = Core
       globals =
          None
          Bool
          Text
          List
          Number
          'none
          'true
          'false

    module =
       path = Debug
       globals =
          log
          todo
          toHuman

    module =
       path = Array
       globals =
          Array

    module =
       path = List

    module =
        path = Maybe
        globals =
            Maybe
            'just
            'nothing

    module =
        path = Text

    module =
        path = Tuple

    module =
        path = Basics
        globals =
            Int
            clamp
            cloneImm
            cloneUni
            identity
            not
            applyIf
            modBy
            min
            max
            btw

    module =
        path = Hash
        globals = Hash

    module =
        path = Dict
        globals = Dict

    module =
        path = Set
        globals = Set

    module =
        path = Result
        globals =
          Result
          'ok
          'err
          onOk

    module =
        path = Self

