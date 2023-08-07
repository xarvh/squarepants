

valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


#
tests as Test =
    Test.Group "Array"
        [
        , valueTest
            """
            push
            """
            fn _:
                !a = Array.fromList ["a"]
                Array.push @a "b"
                Array.push @a "c"
                Array.toList @a
            (Test.isOkAndEqualTo ["a", "b", "c"])
        , valueTest
            """
            pop 1
            """
            fn _:
                !a = Array.fromList ["x", "y", "z"]
                b = Array.pop @a
                c = Array.pop @a
                l = Array.toList @a
                { l, b, c }
            (Test.isOkAndEqualTo { l = ["x"], b = Just "z", c = Just "y" })
        , valueTest
            """
            pop empty
            """
            fn _:
                !a = Array.fromList []
                b = Array.pop @a
                l = Array.toList @a
                { l, b }
            (Test.isOkAndEqualTo { l = [], b = Nothing })
        , valueTest
            """
            get Just
            """
            fn _:
                !a = Array.fromList ["p", "q"]
                Array.get @a 1
            (Test.isOkAndEqualTo (Just "q"))
        , valueTest
            """
            get Nothing
            """
            fn _:
                !a = Array.fromList ["p", "q"]
                Array.get @a 3
            (Test.isOkAndEqualTo Nothing)
        , valueTest
            """
            set success
            """
            fn _:
                !a = Array.fromList [8, 9]
                r = Array.set @a 0 10
                l = Array.toList @a
                { l, r }
            (Test.isOkAndEqualTo { l = [10, 9], r = True })
        , valueTest
            """
            set fail
            """
            fn _:
                !a = Array.fromList [8, 9]
                r = Array.set @a 3 10
                l = Array.toList @a
                { l, r }
            (Test.isOkAndEqualTo { l = [8, 9], r = False })
        , valueTest
            """
            sortBy
            """
            fn _:
                !a = Array.fromList [55, 99, 22]
                Array.sortBy @a (fn x: -x)
                Array.toList @a
            (Test.isOkAndEqualTo [99, 55, 22])
        ]
#]
