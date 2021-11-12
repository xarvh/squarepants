
valueTest =
    Test.valueTest Debug.toHuman


tests =
    Test.Group "SPCore/List"
        [ sortBy
        ]


sortBy =
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
            fn _:
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
            fn _:
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

