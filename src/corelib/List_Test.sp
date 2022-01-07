
valueTest as Text: (None: a): Test.CodeExpectation a: Test =
    Test.valueTest SPCore.toHuman


tests as Test =
    Test.Group "SPCore/List"
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
            _:
                [
                , Just 23
                , Nothing
                , Just 11
                ]
                    >> List.sortBy identity
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
            _:
                [
                , 23 & 1
                , 1 & 2
                , 11 & 3
                ]
                    >> List.sortBy identity
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
            _:
                [
                , "z" & "a" & "2"
                , "a" & "b" & "33"
                , "z" & "a" & "1"
                , "z" & "b" & "3"
                ]
                    >> List.sortBy identity
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
            (_: List.concat [ [1, 2], [3, 4] ])
            (Test.isOkAndEqualTo [ 1, 2, 3, 4])
        ]

