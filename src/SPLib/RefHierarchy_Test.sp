

valueTest as fn Text, (fn None: a), Test.CodeExpectation a: Test =
    Test.valueTest toHuman __ __ __



graph1 as Dict Text (Text & Set Text) =

  x = fn k, l: k & (k & Set.fromList l)

  [
  , x "a" [ "b", "d" ]
  , x "b" [ "c", "e" ]
  , x "c" [ "e", "d" ]
  , x "d" []
  , x "e" []
  ]
      >> Dict.fromList



graph2 as Dict Text (Text & Set Text) =

  x = fn k, l: k & (k & Set.fromList l)

  [
  , x "a" [ "b", "d" ]
  , x "b" [ "c", "e" ]
  , x "c" [ "e", "d" ]
  , x "d" [ "b" ]
  , x "e" []
  ]
      >> Dict.fromList



canonicalJsTest as Test =
    valueTest
        """
        [reg] THIS SHOULD BE IN CANONICALTOJS
        """
        (fn _: Core.compare None None)
        (Test.isOkAndEqualTo 0)


tests as Test =
    Test.Group "RefHierarchy" [
        , canonicalJsTest
        , valueTest
            """
            Basic
            """
            fn _:
                RefHierarchy.reorder Tuple.second graph1

            (Test.isOkAndEqualTo << [] & ["d", "e", "c", "b", "a"])

        , valueTest
            """
            Circular
            """
            fn _:
                RefHierarchy.reorder Tuple.second graph2

            (Test.isOkAndEqualTo << [[ "b", "d", "c" ]] & ["d", "e", "c", "b", "a"])
        ]

