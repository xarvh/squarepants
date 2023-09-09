valueTest as fn Text, fn None: a, Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


tests as Test =
    Test.'group
        "Dict"
        [
        , insertAndGet
        , lists
        ]


insertAndGet as Test =
    Test.'group
        "insertAndGet"
        [
        , valueTest
            """
            get, success
            """
            (fn _:
                 Dict.empty
                 >> Dict.insert ('just "a") 1 __
                 >> Dict.insert ('just "b") 2 __
                 >> Dict.get ('just "a") __
            )
            (Test.isOkAndEqualTo << 'just 1)
        , valueTest
            """
            get, fail
            """
            (fn _:
                 Dict.empty
                 >> Dict.insert ('just "a") 1 __
                 >> Dict.insert ('just "b") 2 __
                 >> Dict.get ('just "c") __
            )
            (Test.isOkAndEqualTo << 'nothing)
        ]


lists as Test =
    Test.'group
        "lists"
        [
        , valueTest
            """
            keys
            """
            (fn _:
                 Dict.empty
                 >> Dict.insert ('just "a") 1 __
                 >> Dict.insert ('just "b") 2 __
                 >> Dict.insert 'nothing 2 __
                 >> Dict.keys
                 >> List.sortBy identity __
            )
            (Test.isOkAndEqualTo
                 [
                 , 'just "a"
                 , 'just "b"
                 , 'nothing
                 ]
            )
        , valueTest
            """
            values
            """
            (fn _:
                 Dict.empty
                 >> Dict.insert ('just "a") { a = 1 } __
                 >> Dict.insert ('just "b") { a = 3 } __
                 >> Dict.values
                 >> List.sortBy identity __
            )
            (Test.isOkAndEqualTo
                 [
                 , { a = 1 }
                 , { a = 3 }
                 ]
            )
        ]
