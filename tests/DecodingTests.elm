module DecodingTests exposing (suite)

import Expect
import Json.Decode exposing (decodeString)
import Main
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Decoding tests"
        [ test "Decode scene" <|
            \_ -> decodeString Main.decodeStory story |> Expect.equal (Ok expectedScene)
        ]


story =
    """
    {
    "scene": [
        {
            "home": "intro",
            "name": "Once upon a time",
            "route": [
                {"optionText": "Back", "target": "go_back" },
                {"optionText": "Look", "target": "butterfly"}
            ]
        },
        {
            "home": "go_back",
            "name": "Go back",
            "route": [
                {"optionText": "Hide", "target": "hide" },
                {"optionText": "Cough", "target": "cough"}
            ]
        }
        ]
    }
    """


expectedScene =
    { scene =
        [ { home = "intro"
          , name = "Once upon a time"
          , route =
                [ { optionText = "Back", target = "go_back" }
                , { optionText = "Look", target = "butterfly" }
                ]
          }
        , { home = "go_back"
          , name = "Go back"
          , route =
                [ { optionText = "Hide", target = "hide" }
                , { optionText = "Cough", target = "cough" }
                ]
          }
        ]
    }
