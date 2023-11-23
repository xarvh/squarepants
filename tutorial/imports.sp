

sourceDir =
    path = "."


library =
    source = ":browser"

    module =
        path = Html
        globals =
            Html
            div

    module =
        path = VirtualDom


sourceDir =
    path = "../src"

    module =
        path = ImportsFile
        globals =
            ImportsFile

    module =
       path = Platform
       importAs = Platform
       globals =
          Platform

    module =
       path = Compiler/Error
       importAs = Error
       globals =
          Error
          Res

    module =
       path = SPLib/SPON
       importAs = SPON

    module =
       path = SPLib/RefHierarchy
       importAs = RefHierarchy

    module =
       path = SPLib/Format
       importAs = Fmt

    module =
       path = SPLib/Parser
       importAs = Parser

    module =
       path = Compiler/CoreDefs
       importAs = CoreDefs

    module =
       path = Targets/Javascript/Ast
       importAs = JA

library =
    source = "core"

    module =
        path = Compiler/Meta
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
       path = Compiler/Pos
       importAs = Pos
       globals =
            Pos
            At
            'at

    module =
        path = Compiler/Op
        importAs = Op

    module =
        path = Compiler/Token
        importAs = Token
        globals =
            Token
            'token

    module =
       path = Compiler/Ast
       importAs = Ast
       globals =
          Name
          Ref
          UnivarId
          Uniqueness
          'refLocal
          'refGlobal
          'refPlaceholder
          'uni
          'imm
          'depends
          toImm
          toUni

    module =
       path = Compiler/FormattableAst
       importAs = FA

    module =
       path = Compiler/CanonicalAst
       importAs = CA

    module =
       path = Compiler/TypedAst
       importAs = TA

    module =
       path = Compiler/EmittableAst
       importAs = EA

    module =
        path = Self

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
            applyIf
            clamp
            cloneImm
            cloneUni
            identity
            not
            modBy
            round
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
