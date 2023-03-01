
valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


tests as Test =
    Test.Group "List"
        [ sortBy
        , concat
        ]


sortBy as Test =
    Test.Group "sortBy"
        [
        , valueTest
            """
            Can actually sort stuff
            """
            fn _:
                [
                , Just 23
                , Nothing
                , Just 11
                ]
                >> List.sortBy identity __
            (Test.isOkAndEqualTo
                [
                , Just 11
                , Just 23
                , Nothing
                ]
            )
        , valueTest
            """
            Correctly orders tuple-2
            """
            fn _:
                [
                , 23 & 1
                , 1 & 2
                , 11 & 3
                ]
                >> List.sortBy identity __
            (Test.isOkAndEqualTo
                [
                , 1 & 2
                , 11 & 3
                , 23 & 1
                ]
            )
        , valueTest
            """
            Correctly orders tuple-3
            """
            fn _:
                [
                , "z" & "a" & "2"
                , "a" & "b" & "33"
                , "z" & "a" & "1"
                , "z" & "b" & "3"
                ]
                >> List.sortBy identity __
            (Test.isOkAndEqualTo
                [
                , "a" & "b" & "33"
                , "z" & "a" & "1"
                , "z" & "a" & "2"
                , "z" & "b" & "3"
                ]
            )
        ]



concat as Test =
    Test.Group "concat"
        [
        , valueTest
            """
            concats two lists
            """
            (fn _: List.concat [ [1, 2], [3, 4] ])
            (Test.isOkAndEqualTo [ 1, 2, 3, 4])
        ]

