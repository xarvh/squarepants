

tests as Test =
    Test.Group "CanonicalToJS"
        [ misc
        , mutation
        , ifs
        , try_
        , natives
        ]


codeTest as Text: Text: (Text: Result Text ok): Test.CodeExpectation ok: Test =
    Test.codeTest SPCore.toHuman


eval as Text: Text: Result Text Text =
    value: code:
    Ok "CanonicalToJS.eval not implemented"


#
# Misc stuff
#


misc as Test =
    Test.Group "misc"
        [ codeTest "definitions and mutations return None"
            """
            x =
              m @= 0

            y =
              m @= 0
              @m += 1

            a =
              { x, y }
            """
            (eval "Test.a")
            (Test.isOkAndEqualTo """{"x":null,"y":null}""")
        , codeTest "Cons"
            # TODO would be nicer to compare against `b = SPCore.Cons 1 SPCore.Nil`
            """
            a = 1 :: []
            """
            (eval "Test.a")
            (Test.isOkAndEqualTo """["SPCore.Cons",1,["SPCore.Nil"]]""")
        ]



#
# Mutation
#


mutation as Test =
    Test.Group "mutation"
        [ codeTest "basic sanity"
            """
            a =
              m @= 0
              @m += 1
              x = m
              @m := 10
              y = m
              @m += 1
              z = m
              { x, y, z, m }
            """
            (eval "Test.a")
            (Test.isOkAndEqualTo """{"m":11,"x":1,"y":10,"z":11}""")

        #
        , codeTest "nested record"
            """
            record = { x = { y = { z = 4 } } }

            result =
               m @= record
               @m.x.y :=  { z = 1 }
               @m.x.y.z += 1
               m
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """{"x":{"y":{"z":2}}}""")

        #
        , codeTest "pass mutable to function"
            """
            fun @m =
              @m += 55

            result =
               m @= 2
               fun @m
               m
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """57""")

        #
        , codeTest "pass nested mutable value to function"
            """
            fun @m =
              @m += 55

            record = { x = { y = { z = 4 } } }

            result =
               m @= record
               fun @m.x.y.z
               m
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """{"x":{"y":{"z":59}}}""")

        #
        , codeTest "[reg] mut args should be dereferenced and cloned"
            """
            result =
                l @= 3
                f @l

            f @a =
                as Number @: Number
                a
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "3")
        ]



#
# ifs
#


ifs as Test = 
    Test.Group "ifs"
        [ codeTest "basic sanity"
            """
            a =
              if True:
                1
              else
                2
            """
            (eval "Test.a")
            (Test.isOkAndEqualTo "1")
        ]



#
# try
#


try_ as Test = 
    Test.Group "try"
        [ codeTest "basic sanity"
            """
            union A = A Number, B, C Bool

            a x =
              try x as
                A 1: 11
                A n: n
                B: 3
                C False: 5
                C _: 6

            result =
             { x = a (A 2)
             , y = a (A 1)
             , z = a B
             , w = a (C False)
             , k = a (C True)
             }
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """{"k":6,"w":6,"x":2,"y":11,"z":3}""")
        , codeTest "[reg]: pattern any"
            """
            result =
               try 2 as
                 x: x
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "2")
        ]



#
# Natives
#


natives as Test = 
    Test.Group "natives"
        [ codeTest "SPCore/Debug.log"
            """
            result = log "this is produced by a test" True
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """true""")
        , codeTest "SPCore/Debug.log, partially applied"
            """
            result = log "if this gets actually logged, we have a problem"
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """undefined""")
        , codeTest "SPCore/Debug.todo"
            """
            a = todo "blah"
            result = 1
            """
            (eval "Test.result")
            (Test.errorContains [ "blah" ])
        , codeTest "Text concat (..)"
            """
            result = "a" .. "b" .. "c"
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "\"abc\"")
        , codeTest "add"
            """
            result = 1 + 2
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "3")
        , codeTest "subtract"
            """
            result = 5 - 3
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "2")
        , codeTest "multiply"
            """
            result = 3 * 2
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "6")
        , codeTest "divide"
            """
            result = 3 / 2
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "1.5")
        , codeTest "divide by zero"
            """
            result = 3 / 0
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "0")
        , codeTest "lesser than (<)"
            """
            result = 3 < 2 & 2 < 3
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """{"first":false,"second":true}""")
        , codeTest "greater than (>)"
            """
            result = 3 > 2 & 2 > 3
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo """{"first":true,"second":false}""")
        , codeTest "partial application"
            """
            result = (-) 2
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "undefined")
        , codeTest "mutable partial application"
            """
            f = (+=) 3

            result =
               m @= 1
               f @m
               m
            """
            (eval "Test.result")
            (Test.isOkAndEqualTo "4")
        ]
