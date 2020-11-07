module Test exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class)


type alias Test =
    { name : String
    , maybeError : Maybe String
    }



----
--- Constructors
--


type alias SimpleArgs outcome =
    { name : String
    , run : () -> outcome
    , expected : outcome
    }


simple : (outcome -> String) -> SimpleArgs outcome -> Test
simple toString { name, run, expected } =
    { name = name
    , maybeError =
        let
            actual =
                run ()
        in
        if actual == expected then
            Nothing

        else
            [ "Expected: "
            , toString expected
            , "\n"
            , "Actual: "
            , toString actual
            ]
                |> String.join ""
                |> Just
    }



----
--- View
--


viewList : List Test -> Html msg
viewList tests =
    tests
        |> List.sortBy
            (\t ->
                if t.maybeError /= Nothing then
                    0

                else
                    1
            )
        |> List.map view
        |> (::) style
        |> Html.div []


view : Test -> Html msg
view test =
    Html.div
        [ class "test-item"
        , if test.maybeError == Nothing then
            class "test-ok"

          else
            class "test-error"
        ]
        [ Html.div [ class "test-name" ] [ Html.text test.name ]
        , Html.code []
            [ case test.maybeError of
                Nothing ->
                    Html.text "Ok!"

                Just error ->
                    Html.text error
            ]
        ]


style : Html msg
style =
    Html.node "style"
        []
        [ Html.text
            """
.test-item {
  padding: 1em;
}

.test-name {
  margin-bottom: 0.5em;
}

.test-ok {
  background-color: #6f6;
}

.test-error {
  background-color: #f66;
}
  """
        ]
