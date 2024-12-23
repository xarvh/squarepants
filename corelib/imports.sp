
sourceDir =
    path = "src"

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
       path = BuildInfo


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

    module =
       path = Compiler/EmittableAst
       importAs = EA

    module =
        path = Compiler/Token
        importAs = Token
        globals =
            Token
            'token


library =
    source = ":test"
    module =
       path = Test
       importAs = Test
       globals = Test

