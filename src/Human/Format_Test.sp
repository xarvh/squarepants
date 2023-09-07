

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
        , formatTest
            """
            Extension
            """
            """
            { z with e }
            """
            """
            { z with e }

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
            SKIP (low priority) Named functions should always have their body below
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
            Named functions should always have their body below
            """
            """
            x =
              a
              >> onOk fn b:

              c
              >> onOk fn d:

              e
            """
            """
            x =
                a
                >> onOk fn b:
                c
                >> onOk fn d:
                e

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

        , formatTest
            """
            precedence
            """
            """
            a (b  c)
            """
            """
            a (b c)

            """

        , formatTest
            """
            With multiline args
            """
            """
            a =
                (x >> y)
                   fn _:
                      a
                      b
                  (assert blah)
                  (zak meh)
            """
            """
            a =
                (x >> y)
                    (fn _:
                         a

                         b
                    )
                    (assert blah)
                    (zak meh)

            """

        , formatTest
            """
            Preserve aligned
            """
            """
            a =
                x fn _:
                y
            """
            """
            a =
                x fn _:
                y

            """
        , formatTest
            """
            Preserve indent
            """
            """
            a =
                x fn _:
                  y
            """
            """
            a =
                x fn _:
                    y

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
            var Meh = 'meh z
            """
            """
            var Meh =
                or 'meh z

            """
        , formatTest
            """
            Unions 2
            """
            """
            var Meh a b = 'blah or 'meh x y z
            """
            """
            var Meh a b =
                or 'blah
                or 'meh x y z

            """
        , formatTest
            """
            Annotated values
            """
            """
            (x as Type) & (y as Kind) = thing
            """
            """
            (x as Type) & (y as Kind) =
                thing

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
# TODO Escaping the escapes is more than my brain can take now.
#        , formatTest
#            """
#            SKIP (not enough brain to go through 3 inception levels of escaping...) Preserve single quotes
#            """
#            """
#            Text.replace "\\\"" "\"" __
#            """
#            """
#            """
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
            Preserves comments (long, inline comments)
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
        , formatTest
            """
            Preserves comments (long, block comments)
            """
            """
            try e as
                [# AAA #]
                , T:
                [# BBB #]
                    9
                , _: 10
            """
            """
            try e as

                [# AAA #]
                , T:
                    [# BBB #]
                    9

                , _:
                    10

            """
        , formatTest
            """
            Comments
            """
            """
            x =
                      try char as
                        , "":
                            None

            #            "@":

                        , "#":
                            start

            """
            """
            x =
                try char as

                    , "":
                        None

            #            "@":

                    , "#":
                        start

            """
        , formatTest
            """
            [reg] Should not move a comma inside a multi-line comment
            """
            """
            try char as
              , a: b

              [#
              comment
              #]

              , d: c
            """
            """
            try char as

                , a:
                    b

                [#
                comment
                #]

                , d:
                    c

            """
        , formatTest
            """
            [reg] Comments should not cause content to multiline
            """
            """
            try value as

                # TODO restore `None` here once it doesn't break JS any more
                , Err _:
                    formatIndented
            """
            """
            try value as

                # TODO restore `None` here once it doesn't break JS any more
                , Err _:
                    formatIndented

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

