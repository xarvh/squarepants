
valueTest as Text: (None: a): Test.CodeExpectation a: Test =
    Test.valueTest toHuman


tests as Test =
    Test.Group "Dict"
        [
        , insertAndGet
        , lists
        ]


insertAndGet as Test =
    Test.Group "insertAndGet"
        [
        , valueTest
            """
            get, success
            """
            _:
                Dict.empty
                    >> Dict.insert (Just "a") 1
                    >> Dict.insert (Just "b") 2
                    >> Dict.get (Just "a")
            (Test.isOkAndEqualTo << Just 1)
        , valueTest
            """
            get, fail
            """
            _:
                Dict.empty
                    >> Dict.insert (Just "a") 1
                    >> Dict.insert (Just "b") 2
                    >> Dict.get (Just "c")
            (Test.isOkAndEqualTo << Nothing)
        ]


lists as Test =
    Test.Group "lists"
        [
        , valueTest
            """
            keys
            """
            _:
                Dict.empty
                    >> Dict.insert (Just "a") 1
                    >> Dict.insert (Just "b") 2
                    >> Dict.insert Nothing 2
                    >> Dict.keys
                    >> List.sortBy identity
            (Test.isOkAndEqualTo
                [
                , Just "a"
                , Just "b"
                , Nothing
                ]
            )
        , valueTest
            """
            values
            """
            _:
                Dict.empty
                    >> Dict.insert (Just "a") { a = 1 }
                    >> Dict.insert (Just "b") { a = 3 }
                    >> Dict.values
                    >> List.sortBy identity
            (Test.isOkAndEqualTo
                [
                , { a = 1 }
                , { a = 3 }
                ]
            )
        ]

