#
# These are regressions from specific targets, but it's probbaly good to run them on every target
#
tests as Test =
    Test.Group
        """
        Emitted
        """
        [
        , javascript
        ]



valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __


#
# JavaScript
#
javascript as Test =
    Test.Group
        """
        JavaScript
        """
        [
        , valueTest
            """
            [reg] Imperative try..as should not force the function to return prematurely
            """
            (fn None:
                try False as
                    , False: 1
                    , True: 2
                "abc"
            )
            (Test.isOkAndEqualTo "abc")
        , valueTest
            """
            SKIP (it breaks everything) [reg] Pattern matching fails on None
            """
            (fn _:
                try None as
                    , None: 1
            )
            (Test.isOkAndEqualTo 1)
        ]

