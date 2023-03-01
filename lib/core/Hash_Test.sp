

valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


tests as Test =
    Test.Group "Hash"
        [
        , valueTest
            """
            insert
            """
            fn _:
                !h = Hash.fromList [1 & 2]
                Hash.insert @h 2 3
                h

            (Test.isOkAndEqualTo << Hash.fromList [1 & 2, 2 & 3])
        , valueTest
            """
            remove
            """
            fn _:
                !h = Hash.fromList [1 & 2, 3 & 4]
                Hash.remove @h 1
                h

            (Test.isOkAndEqualTo << Hash.fromList [3 & 4])
        , valueTest
            """
            get Just
            """
            fn _:
                !h = Hash.fromList [1 & 2, 3 & 4]
                Hash.get @h 1

            (Test.isOkAndEqualTo << Just 2)
        , valueTest
            """
            get Nothing
            """
            fn _:
                !h = Hash.fromList [1 & 2, 3 & 4]
                Hash.get @h 66

            (Test.isOkAndEqualTo Nothing)
        , valueTest
            """
            for
            """
            fn _:
                !hash = Hash.fromList [Just True & 2, Nothing & 4]

                []
                >> Hash.for @hash (fn k, v, a: [v & k, ...a]) __
                >> List.sortBy Tuple.first __

            (Test.isOkAndEqualTo [2 & Just True, 4 & Nothing])
        , valueTest
            """
            each
            """
            fn _:
                !a = Array.fromList []
                !hash = Hash.fromList [Just True & 2, Nothing & 1]
                Hash.each @hash fn k, v:
                    List.each (List.range 1 v) fn _:
                        Array.push @a k

                Array.sortBy @a identity

                a

            (Test.isOkAndEqualTo << Array.fromList [Just True, Just True, Nothing])
        ]

