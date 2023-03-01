
valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


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
            fn _:
                Dict.empty
                    >> Dict.insert (Just "a") 1 __
                    >> Dict.insert (Just "b") 2 __
                    >> Dict.get (Just "a") __
            (Test.isOkAndEqualTo << Just 1)
        , valueTest
            """
            get, fail
            """
            fn _:
                Dict.empty
                    >> Dict.insert (Just "a") 1 __
                    >> Dict.insert (Just "b") 2 __
                    >> Dict.get (Just "c") __
            (Test.isOkAndEqualTo << Nothing)
        ]


lists as Test =
    Test.Group "lists"
        [
        , valueTest
            """
            keys
            """
            fn _:
                Dict.empty
                    >> Dict.insert (Just "a") 1 __
                    >> Dict.insert (Just "b") 2 __
                    >> Dict.insert Nothing 2 __
                    >> Dict.keys
                    >> List.sortBy identity __
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
                    >> Dict.insert (Just "a") { a = 1 } __
                    >> Dict.insert (Just "b") { a = 3 } __
                    >> Dict.values
                    >> List.sortBy identity __
            (Test.isOkAndEqualTo
                [
                , { a = 1 }
                , { a = 3 }
                ]
            )
        ]

