valueTest as fn Text, fn None: a, Test.CodeExpectation a: Test =
    Test.valueTest Debug.toHuman __ __ __


tests as Test =
    Test.'group
        "Hash"
        [
        , valueTest
            """
            insert
            """
            (fn _:
                 !h =
                     Hash.fromList [ 1 & 2 ]

                 Hash.insert @h 2 3

                 h
            )
            (Test.isOkAndEqualTo << Hash.fromList [ 1 & 2, 2 & 3 ])
        , valueTest
            """
            remove
            """
            (fn _:
                 !h =
                     Hash.fromList [ 1 & 2, 3 & 4 ]

                 Hash.remove @h 1

                 h
            )
            (Test.isOkAndEqualTo << Hash.fromList [ 3 & 4 ])
        , valueTest
            """
            get Just
            """
            (fn _:
                 !h =
                     Hash.fromList [ 1 & 2, 3 & 4 ]

                 Hash.get @h 1
            )
            (Test.isOkAndEqualTo << 'just 2)
        , valueTest
            """
            get Nothing
            """
            (fn _:
                 !h =
                     Hash.fromList [ 1 & 2, 3 & 4 ]

                 Hash.get @h 66
            )
            (Test.isOkAndEqualTo 'nothing)
        , valueTest
            """
            for
            """
            (fn _:
                 !hash =
                     Hash.fromList [ 'just 'true & 2, 'nothing & 4 ]

                 []
                 >> Hash.for @hash (fn k, v, a: [ v & k, a... ]) __
                 >> List.sortBy Tuple.first __
            )
            (Test.isOkAndEqualTo [ 2 & 'just 'true, 4 & 'nothing ])
        , valueTest
            """
            each
            """
            (fn _:
                 !a =
                     Array.fromList []

                 !hash =
                     Hash.fromList [ 'just 'true & 2, 'nothing & 1 ]

                 Hash.each @hash fn k, v:
                     List.each (List.range 1 v) fn _:
                         Array.push @a k

                 Array.sortBy @a identity

                 a
            )
            (Test.isOkAndEqualTo << Array.fromList [ 'just 'true, 'just 'true, 'nothing ])
        , valueTest
            """
            pop (empty)
            """
            (fn _:
                 !h =
                     Hash.fromList []

                 r =
                     Hash.pop @h

                 r & h
            )
            (Test.isOkAndEqualTo << 'nothing & Hash.fromList [])
        , valueTest
            """
            pop (one element)
            """
            (fn _:
                 !h =
                     Hash.fromList [ { b = 'nothing } & 2 ]

                 r =
                     Hash.pop @h

                 r & h
            )
            (Test.isOkAndEqualTo << 'just ({ b = 'nothing } & 2) & Hash.fromList [])
        , valueTest
            """
            pop (several element)
            """
            (fn _:
                 !h =
                     Hash.fromList [ { b = 'nothing } & 2, { b = 'just 4 } & 8 ]

                 r =
                     Hash.pop @h

                 r & h
            )
            (Test.isOkAndEqualTo << 'just ({ b = 'nothing } & 2) & Hash.fromList [ { b = 'just 4 } & 8 ])
        ]
