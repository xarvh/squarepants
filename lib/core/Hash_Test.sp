

valueTest as Text: (None: a): Test.CodeExpectation a: Test =
    Test.valueTest toHuman


tests as Test =
    Test.Group "Hash"
        [
        , valueTest
            """
            insert
            """
            _:
                h @= Hash.fromList [1 & 2]
                Hash.insert @h 2 3
                h

            (Test.isOkAndEqualTo << Hash.fromList [1 & 2, 2 & 3])
        , valueTest
            """
            remove
            """
            _:
                h @= Hash.fromList [1 & 2, 3 & 4]
                Hash.remove @h 1
                h

            (Test.isOkAndEqualTo << Hash.fromList [3 & 4])
        , valueTest
            """
            get Just
            """
            _:
                h = Hash.fromList [1 & 2, 3 & 4]
                Hash.get h 1

            (Test.isOkAndEqualTo << Just 2)
        , valueTest
            """
            get Nothing
            """
            _:
                h = Hash.fromList [1 & 2, 3 & 4]
                Hash.get h 66

            (Test.isOkAndEqualTo Nothing)
        , valueTest
            """
            for
            """
            _:
                []
                    >> Hash.for (Hash.fromList [Just True & 2, Nothing & 4]) k: v: a: (v & k) :: a
                    >> List.sortBy Tuple.first

            (Test.isOkAndEqualTo [2 & Just True, 4 & Nothing])
        , valueTest
            """
            each
            """
            _:
                a @= Array.fromList []

                Hash.each (Hash.fromList [Just True & 2, Nothing & 1]) k: v:
                    List.each (List.range 1 v) _:
                        Array.push @a k

                Array.sortBy @a identity

                a

            (Test.isOkAndEqualTo << Array.fromList [Just True, Just True, Nothing])
        ]

