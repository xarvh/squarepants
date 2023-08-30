

tests as Test =
    Test.Group "Formatter" [
        , definitions
        , operators
        , functions
        , calls
        , lists
        , records
        , comments
        , textLiterals
        , uniqueness
        , tryAs
        , ifs
        ]


format as fn Text: Result Text Text =
    fn input:

    Compiler/Parser.textToFormattableModule {
         , errorModule = {
            , fsPath = "test"
            , content = input
          }
          , stripLocations = False
          , keepComments = True
    }
    >> Result.mapError toHuman __
    >> onOk fn faStatements:

    env as Human/Format.Env = {
        , isRoot = True
        , originalContent = input
        }

    faStatements
    >> Human/Format.formatStatements env __
    >> Fmt.render
    >> Ok


formatTest as fn Text, Text, Text: Test =
    fn title, input, expectedOutput:

    quote =
        "```\n" .. __ .. "\n```"

    Test.CodeExpectation toMaybeError =
        Test.isOkAndEqualTo expectedOutput

    Test.Single title "" fn None:
        input
        >> format
        >> toMaybeError quote __
        >> Test.maybeToOutcome


operators as Test =
    Test.Group "Operators"
        [
        , formatTest
            """
            Precedence
            """
            """
            a = (1 + 2 * 3 + 4)
            b = 5*(6+ 7)*8
            """
            """
            a =
                1 + 2 * 3 + 4


            b =
                5 * (6 + 7) * 8

            """
        , formatTest
            """
            Pipe on a single line stays on a single line
            """
            """
            a = b >> c >> d >> e
            """
            """
            a =
                b >> c >> d >> e

            """
        , formatTest
            """
            Pipe right gets indented if there is at least one line break
            """
            """
            a = b >> c
              >> d >> e
            """
            """
            a =
                b
                >> c
                >> d
                >> e

            """
        , formatTest
            """
            [reg] Preserve comment
            """
            """
            escape =
                x
                # comment
                >> replace
                >> replace
            """
            """
            escape =
                x
                # comment
                >> replace
                >> replace

            """
        ]



lists as Test =
    Test.Group "Lists"
        [
        , formatTest
            """
            Preserve one-liners
            """
            """
            a = [1, 2, 3]
            """
            """
            a =
                [ 1, 2, 3 ]

            """
        , formatTest
            """
            Preserve multilines
            """
            """
            a = [1
              , 2, 3]
            """
            """
            a =
                [
                , 1
                , 2
                , 3
                ]

            """

        , formatTest
            """
            [reg] weird shit happening when a definition is preceded by a blank line?
            """
            """

            ll as [Int] = []
            """
            """
            ll as [ Int ] =
                []

            """
        ]


records as Test =
    Test.Group "Records"
        [
        , formatTest
            """
            Reorder attributes
            """
            """
            a = { z, e, r }
            """
            """
            a =
                { e, r, z }

            """
        , formatTest
            """
            Comments
            """
            """
            a = {
                , z = 1
                # AAA
                , e = 2
                , r =
                  # BBB
                   3
               }
            """
            """
            a =
                {
                # AAA
                , e =
                    2
                , r =
                    # BBB
                    3
                , z = 1
                }

            """
        ]


uniqueness as Test =
    Test.Group "Uniqueness"
        [
        , formatTest
            """
            Preserve uniqueness annotations
            """
            """
            f as fn !a, 2?b, @c: !d = fn !a, 2?b, @c: blah
            """
            """
            f as fn !a, 2?b, @c: !d =
                fn !a, 2?b, @c: blah

            """
        ]


functions as Test =
    Test.Group "Functions"
        [
        [# Can these actually be done?

            stack vs indent
            ---------------

            * named functions: stack

                a =
                    fn b:
                    fn c:
                    d


            * as last argument: indent

                a =
                    List.for init list fn b:
                        c


            * as mid argument: indent

                a =
                    test
                        fn b:
                            c
                        d


            * as pseudo-do-notation: stack

                monad
                >> bind fn a:
                b


            blank vs no blank
            -----------------

            ??? with one evaluation

                someFunctionName =
                    fn x:
                    x + x


            ??? with many statements:

                someFunctionName =
                    fn x:

                    statement1

                    statement2

        #]
        , formatTest
            """
            Named functions should always have their body below
            """
            """
            f =
                fn a: fn b: c
            """
            """
            f =
                fn a:
                fn b:
                c
            """

        , formatTest
            """
            As multiline args
            """
            """
            a =
                test
                   fn _:
                      a
                      b
                  (assert blah)
            """
            """
            a =
                test
                    fn _:
                        a
                        b
                    (assert blah)
            """
        ]


calls as Test =
    Test.Group "Calls"
        [
        , formatTest
            """
            Arguments indentation
            """
            """
            a =
               fun
                  #comment1
                  a
                    #comment2
                    b
            """
            """
            a =
                fun
                    #comment1
                    a
                    #comment2
                    b

            """
        ]



definitions as Test =
    Test.Group "Definitions"
        [
        , formatTest
            """
            Alias
            """
            """
            alias Meh a b = Int
            """
            """
            alias Meh a b =
                Int

            """
        , formatTest
            """
            Unions 1
            """
            """
            union Meh = Meh z
            """
            """
            union Meh =
                , Meh z

            """
        , formatTest
            """
            Unions 2
            """
            """
            union Meh a b = Blah, Meh x y z
            """
            """
            union Meh a b =
                , Blah
                , Meh x y z

            """
        ]



textLiterals as Test =
    Test.Group "Text literals"
        [
        , formatTest
            """
            Single quotes escape quotes and backspaces
            """
            """
            a = "\\"\\\\"
            """
            """
            a =
                "\\"\\\\"

            """
        ]


comments as Test =
    Test.Group "Comments"
        [
        , formatTest
            """
            [reg] Should not add indent to empty lines
            """
            """
            if a then
                [#

                    comment

                #]
                b
              else
                c
            """
            """
            if a then
                [#

                    comment

                #]
                b
            else
                c

            """

        , formatTest
            """
            Preserve whether a comment has a trailing blank, line
            """
            """
            # blank

            a = 1

            # no blank
            b = 1
            """
            """
            # blank

            a =
                1


            # no blank
            b =
                1

            """

        , formatTest
            """
            Preserve whether a comment has a trailing blank, block
            """
            """
            [# blank #]

            a = 1

            [# no blank #]
            b = 1
            """
            """
            [# blank #]

            a =
                1


            [# no blank #]
            b =
                1

            """

        , formatTest
            """
            Comment order is preserved
            """
            """
            # a
            # b
            # c
            aaaaa =
            # d
            # e
            # f
                  bbbbb

                  #g
                  #h
                  #i
                  ccccc
            """
            """
            # a
            # b
            # c
            aaaaa =
            # d
            # e
            # f
                bbbbb

                #g
                #h
                #i
                ccccc

            """

        , formatTest
            """
            Normal comments get indented, line
            """
            """
            a =
             # b
                1
            """
            """
            a =
                # b
                1

            """
        , formatTest
            """
            "Quick" comments remain untouched, line
            """
            """
            a =
            # b
              1
            """
            """
            a =
            # b
                1

            """
        , formatTest
            """
            Normal comments get indented, block
            """
            """
            a =
             [# b
            blah#]
                1
            """
            """
            a =
                [# b
                blah#]
                1

            """
        , formatTest
            """
            "Quick" comments remain untouched, block
            """
            """
            a =
            [# b
            #]
              1
            """
            """
            a =
            [# b
            #]
                1

            """
        , formatTest
            """
            [reg] Section comment should stay before the if
            """
            """
            f =
                a = 1

            # comment

                if a then
                    b
                  else
                    c
            """
            """
            f =
                a =
                    1

            # comment

                if a then
                    b
                else
                    c

            """

        , formatTest
            """
            [reg] Block comment internal indent should remain consistent
            """
            """
            x =
                [#
                    comment
                #]
                d
            """
            """
            x =
                [#
                    comment
                #]
                d

            """
        ]



tryAs as Test =
    Test.Group "try..as"
        [
        , formatTest
            """
            Compact
            """
            """
            x = try blah as
              , Just a: a
              , Nothing:  b
            """
            """
            x =
                try blah as
                    , Just a: a
                    , Nothing: b

            """
        , formatTest
            """
            Long
            """
            """
            x = try blah as
              , Just a:
                    b = 1
                    b + a
              , Nothing:  b
            """
            """
            x =
                try blah as

                    , Just a:
                        b =
                            1

                        b + a

                    , Nothing:
                        b

            """
        , formatTest
            """
            SKIP (low priority) Preserves comments (compact)
            """
            """
            try e  as
                , T: 9
                # comment
                , _:  10
            """
            """
            try e as
                , T: 9
                # comment
                , _: 10

            """
        , formatTest
            """
            Preserves comments (long)
            """
            """
            try e as
                # AAA
                , T:
                # BBB
                    9
                , _: 10
            """
            """
            try e as

                # AAA
                , T:
                    # BBB
                    9

                , _:
                    10

            """
        ]


ifs as Test =
    Test.Group "if..then"
        [
        , formatTest
            """
            Single line
            """
            """
            x = if blah   then a  else b
            """
            """
            x =
                if blah then a else b

            """
        , formatTest
            """
            Make multiline
            """
            """
            x = if blah then
                  a = 1
                  a + b
              else  b
            """
            """
            x =
                if blah then
                    a =
                        1

                    a + b
                else
                    b

            """
        , formatTest
            """
            Preserve multiline
            """
            """
            x =
                if blah then
                    a
                else b
            """
            """
            x =
                if blah then
                    a
                else
                    b

            """
        , formatTest
            """
            if else
            """
            """
            x = if blah then a else if o then b else q
            """
            """
            x =
                if blah then
                    a
                else if o then
                    b
                else
                    q

            """
        ]

