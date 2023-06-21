module NoUnqualifiedFunctionsTest exposing (all)

import NoUnqualifiedFunctions as NUF
import Review.Test as RT
import Test as T exposing (Test)


all : Test
all =
    T.describe "NoUnqualifiedFunctions"
        [ T.test "should not report an error when only types and operators are imported" <|
            \() ->
                """module A exposing (..)

import B exposing (SomeType, (</>))

a = 1
"""
                    |> RT.run NUF.rule
                    |> RT.expectNoErrors
        , T.test "should report an error when a function or value is imported" <|
            \() ->
                """module A exposing (..)

import B exposing (someValue)

a = someValue 1
"""
                    |> RT.run NUF.rule
                    |> RT.expectErrors
                        [ RT.error
                            { message = "Importing functions is not allowed. Use qualified names instead."
                            , details =
                                [ "'A' imports 'someValue' from 'B'\n Uses of someValue should be replaced with B.someValue."
                                ]
                            , under = "import B exposing (someValue)"
                            }
                            |> RT.whenFixed """module A exposing (..)

import B exposing (someValue)

a = B.someValue 1
"""
                        ]
        , T.test "applying the fix should NOT apply incorrect prefixes to shadowed variables of the same name" <|
            \() ->
                """module A exposing (..)

import B exposing (someValue)

a = someValue 1

b someValue =
    someValue 2
"""
                    |> RT.run NUF.rule
                    |> RT.expectErrors
                        [ RT.error
                            { message = "Importing functions is not allowed. Use qualified names instead."
                            , details =
                                [ "'A' imports 'someValue' from 'B'\n Uses of someValue should be replaced with B.someValue."
                                ]
                            , under = "import B exposing (someValue)"
                            }
                            |> RT.whenFixed """module A exposing (..)

import B exposing (someValue)

a = B.someValue 1

b someValue =
    someValue 2
"""
                        ]
        , T.test "applying the fix should NOT modify prefixes from other modules" <|
            \() ->
                """module A exposing (..)

import B exposing (someValue)
import B.C as C

a = someValue 1

b = C.someValue 2
"""
                    |> RT.run NUF.rule
                    |> RT.expectErrors
                        [ RT.error
                            { message = "Importing functions is not allowed. Use qualified names instead."
                            , details =
                                [ "'A' imports 'someValue' from 'B'\n Uses of someValue should be replaced with B.someValue."
                                ]
                            , under = "import B exposing (someValue)"
                            }
                            |> RT.whenFixed """module A exposing (..)

import B exposing (someValue)
import B.C as C

a = B.someValue 1

b = C.someValue 2
"""
                        ]
        , T.test "applying the fix should NOT replace arguments with similar names inside let blocks" <|
            \() ->
                """module A exposing (..)

import B exposing (someValue)

a =
    let
        b someValue = someValue == 13
    in
    someValue
"""
                    |> RT.run NUF.rule
                    |> RT.expectErrors
                        [ RT.error
                            { message = "Importing functions is not allowed. Use qualified names instead."
                            , details =
                                [ "'A' imports 'someValue' from 'B'\n Uses of someValue should be replaced with B.someValue."
                                ]
                            , under = "import B exposing (someValue)"
                            }
                            |> RT.whenFixed """module A exposing (..)

import B exposing (someValue)

a =
    let
        b someValue = someValue == 13
    in
    B.someValue
"""
                        ]
        ]
