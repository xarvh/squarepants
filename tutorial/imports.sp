

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
       path = Compiler/Error
       importAs = Error
       globals =
          Error
          Res

    module =
       path = Compiler/CoreDefs
       importAs = CoreDefs


    module =
       path = Types/Ast
       importAs = Ast
       globals =
          Name
          Ref
          UnivarId
          Uniqueness
          'refLocal
          'refGlobal
          'uni
          'imm
          'depends
          toImm
          toUni

#    module =
#       path = Types/CanonicalAst
#       importAs = CA
#
#    module =
#       path = Types/FormattableAst
#       importAs = FA
#
#    module =
#       path = Targets/Javascript/Ast
#       importAs = JA
#
#    module =
#       path = Compiler/TestHelpers
#       importAs = TH
#
#    module =
#        path = Types/Op
#        importAs = Op
#
#    module =
#       path = Types/Pos
#       importAs = Pos
#       globals =
#            Pos
#            At
#       globals =
#            'at
#
#
#    module =
#       path = SPLib/Parser
#       importAs = Parser


library =
    source = "core"

    module =
        path = Compiler/Meta
        importAs = Meta
        globals =
            Meta
            ByUsr
            USR
            UMR
            'USR
            'UMR

    module =
        path = Compiler/Token
        importAs = Token
        globals =
            Token
            'token

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
