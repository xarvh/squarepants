
valueTest =
    Test.valueTest Debug.toHuman


tests =
    Test.Group "SPCore/Dict"
        [
        , insertAndGet
        , lists
        ]


insertAndGet =
    Test.Group "insertAndGet"
        [
        , valueTest
            """
            get, success
            """
            fn _:
                Dict.empty
                    >> Dict.insert (Just "a") 1
                    >> Dict.insert (Just "b") 2
                    >> Dict.get (Just "a")
            (Test.isOkAndEqualTo << Just 1)
        , valueTest
            """
            get, fail
            """
            fn _:
                Dict.empty
                    >> Dict.insert (Just "a") 1
                    >> Dict.insert (Just "b") 2
                    >> Dict.get (Just "c")
            (Test.isOkAndEqualTo << Nothing)
        ]


lists =
    Test.Group "lists"
        [
        , valueTest
            """
            keys
            """
            fn _:
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
            fn _:
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

