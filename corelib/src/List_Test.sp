valueTest as fn Text, fn None: a, Test.CodeExpectation a: Test =
    Test.valueTest Debug.toHuman __ __ __


tests as Test =
    Test.'group
        "List"
        [
        , sortBy
        , concat
        ]


sortBy as Test =
    Test.'group
        "sortBy"
        [
        , valueTest
            """
            Can actually sort stuff
            """
            (fn _:
                 [
                 , 'just 23
                 , 'nothing
                 , 'just 11
                 ]
                 >> List.sortBy identity __
            )
            (Test.isOkAndEqualTo
                 [
                 , 'just 11
                 , 'just 23
                 , 'nothing
                 ]
            )
        , valueTest
            """
            Correctly orders tuple-2
            """
            (fn _:
                 [
                 , 23 & 1
                 , 1 & 2
                 , 11 & 3
                 ]
                 >> List.sortBy identity __
            )
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
            (fn _:
                 [
                 , "z" & "a" & "2"
                 , "a" & "b" & "33"
                 , "z" & "a" & "1"
                 , "z" & "b" & "3"
                 ]
                 >> List.sortBy identity __
            )
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
    Test.'group
        "concat"
        [
        , valueTest
            """
            concats two lists
            """
            (fn _: List.concat [ [ 1, 2 ], [ 3, 4 ] ])
            (Test.isOkAndEqualTo [ 1, 2, 3, 4 ])
        ]
