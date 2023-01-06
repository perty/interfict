module ConverterTests exposing (suite)

-- This file contains tests of the converter, hence the name.
-- Tests are run with a special tool called, started with npm run test.
-- When you run tests, they will fail as we have not yet written the
-- function that we will test.

import Expect exposing (FloatingPointTolerance(..))
import Main
import Test exposing (Test, describe, test)



-- A number of tests collected is called a suite.


suite : Test
suite =
    -- A that takes a string and a list of test cases. Here we have two cases.
    describe "Converter tests"
        [ -- The first test case is about 32 F which is freezing point 0 C.
          test "Test temperature convert 32" <|
            -- Here comes a function as argument to the test. A bit hairy in the beginning, a lambda expression.
            -- We are saying that the test is a function, when evaluated compares two decimal numbers.
            -- We also say that the numbers are equal if the difference is within 0.1.
            \_ -> Expect.within (Absolute 0.1) (Main.fromFahrenheit 32) 0.0

        -- The second test case. Copy of the first but with different values.
        , test "Test temperature convert 100" <|
            \_ -> Expect.within (Absolute 0.1) (Main.fromFahrenheit 100) 37.7
        ]
