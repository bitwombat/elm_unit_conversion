module Tests exposing (tests)

import Dict
import Expect
import Test exposing (..)
import Units exposing (..)


t1 : Term
t1 =
    Term (Quantity 1.0 [ "J" ]) (Quantity 2.0 [ "s" ])


t2 : Term
t2 =
    Term (Quantity 1.0 [ "m" ]) (Quantity 1.0 [ "m" ])


t3 : Term
t3 =
    Term (Quantity 1.0 [ "F" ]) (Quantity 1.0 [ "F" ])


tests : Test
tests =
    describe "Make units"
        [ test "Make unit" <|
            \() ->
                Expect.equal t1
                    (Term (Quantity 1.0 [ "J" ]) (Quantity 2.0 [ "s" ]))
        , test "Test multiplication" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 [ "J", "m" ]) (Quantity 2.0 [ "s", "m" ]))
                    (multiply t1 t2)
        , test "Test 3-way multiplication" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 [ "J", "m", "F" ]) (Quantity 2.0 [ "s", "m", "F" ]))
                    (multiply
                        t2
                        t3
                        |> multiply t1
                    )
        , test "Test 3-way multiplication with f.g composition (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 [ "J", "m", "F" ]) (Quantity 2.0 [ "s", "m", "F" ]))
                    ((multiply t1 << multiply t2)
                        t3
                    )
        , test "Test 3-way multiplication with g.f composition (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 [ "J", "m", "F" ]) (Quantity 2.0 [ "s", "m", "F" ]))
                    ((multiply t2 >> multiply t1)
                        t3
                    )
        , test "Test 3-way multiplication with pipelines (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 [ "J", "m", "F" ]) (Quantity 2.0 [ "s", "m", "F" ]))
                    (t3 |> multiply t2 |> multiply t1)
        , test "Test unit counting" <|
            \() ->
                Expect.equal
                    (Dict.fromList
                        [ ( "a", 1 )
                        , ( "b", 2 )
                        , ( "c", 3 )
                        ]
                    )
                    (unitListToDict [ "a", "b", "b", "c", "c", "c" ])
        , test "Test unit cancelling" <|
            \() ->
                Expect.equal
                    (Dict.fromList
                        [ ( "a", 0 )
                        , ( "b", 1 )
                        , ( "c", 2 )
                        , ( "d", -1 )
                        ]
                    )
                    (combineUnitNumerAndDenom [ "a", "b", "b", "c", "c", "c" ] [ "a", "b", "c", "d" ])
        , test "Test unit cancelling, part deux" <|
            \() ->
                Expect.equal
                    ( [ "b"
                      , "c"
                      , "c"
                      ]
                    , [ "d"
                      ]
                    )
                    (cancelUnits [ "a", "b", "b", "c", "c", "c" ] [ "a", "b", "c", "d" ])
        , test "Test dict to list" <|
            \() ->
                Expect.equal
                    (dictToList
                        (Dict.fromList
                            [ ( "b", 1 )
                            , ( "c", 2 )
                            ]
                        )
                    )
                    [ "b", "c", "c" ]
        , test "Test Term to string" <|
            \() ->
                Expect.equal
                    (dictFractToDisplayString
                        ( Dict.fromList
                            [ ( "b", 1 )
                            , ( "c", 2 )
                            ]
                        , Dict.fromList
                            [ ( "d", 1 )
                            ]
                        )
                    )
                    ( "b∙c<sup>2</sup>", "d" )
        , test "Test complete simple operation" <|
            \() ->
                Expect.equal (Term (Quantity 6.25 [ "m" ]) (Quantity 1.0 [ "" ]))
                    (multiplyList
                        [ Term (Quantity 10.0 [ "k" ]) (Quantity 1.0 [ "" ])
                        , Term (Quantity 1.0 [ "m" ]) (Quantity 1.6 [ "k" ])
                        ]
                    )
        , test "Test complete operation 1" <|
            \() ->
                Expect.equal (Term (Quantity 6.25 [ "mi" ]) (Quantity 1.0 []))
                    (multiplyList
                        [ Term (Quantity 10.0 [ "km" ]) (Quantity 1.0 [])
                        , Term (Quantity 1.0 [ "mi" ]) (Quantity 1.6 [ "km" ])
                        ]
                    )
        , test "Test complete operation 2" <|
            \() ->
                Expect.equal (Term (Quantity 11.25 [ "mi" ]) (Quantity 1.0 [ "h" ]))
                    (multiplyList
                        [ Term (Quantity 5.0 [ "m" ]) (Quantity 1.0 [ "s" ])
                        , Term (Quantity 3600.0 [ "s" ]) (Quantity 1.0 [ "h" ])
                        , Term (Quantity 1.0 [ "mi" ]) (Quantity 1600.0 [ "m" ])
                        ]
                    )
        , test "Test complete operation 3" <|
            \() ->
                Expect.equal (Term (Quantity 1 [ "mi", "mi" ]) (Quantity 1.0 [ "h" ]))
                    (multiplyList
                        [ Term (Quantity 1.0 [ "m", "mi" ]) (Quantity 1.0 [ "s" ])
                        , Term (Quantity 1.0 [ "s" ]) (Quantity 1.0 [ "h" ])
                        , Term (Quantity 1.0 [ "mi" ]) (Quantity 1.0 [ "m" ])
                        ]
                    )
        ]
