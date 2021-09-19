module Tests exposing (tests)

import Dict
import Expect
import Test exposing (..)
import Units exposing (..)


t1 : Term
t1 =
    Term (Quantity 1.0 "J") (Quantity 2.0 "s")


t2 : Term
t2 =
    Term (Quantity 1.0 "m") (Quantity 1.0 "m")


t3 : Term
t3 =
    Term (Quantity 1.0 "F") (Quantity 1.0 "F")


tests : Test
tests =
    describe "Make units"
        [ test "Make unit" <|
            \() ->
                Expect.equal t1
                    (Term (Quantity 1.0 "J") (Quantity 2.0 "s"))
        , test "Test combining" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 "Jm") (Quantity 2.0 "sm"))
                    (combine t1 t2)
        , test "Test 3-way combining" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 "JmF") (Quantity 2.0 "smF"))
                    (combine
                        t2
                        t3
                        |> combine t1
                    )
        , test "Test 3-way combining with f.g composition (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 "JmF") (Quantity 2.0 "smF"))
                    ((combine t1 << combine t2)
                        t3
                    )
        , test "Test 3-way combining with g.f composition (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 "JmF") (Quantity 2.0 "smF"))
                    ((combine t2 >> combine t1)
                        t3
                    )
        , test "Test 3-way combining with pipelines (exercise)" <|
            \() ->
                Expect.equal (Term (Quantity 1.0 "JmF") (Quantity 2.0 "smF"))
                    (t3 |> combine t2 |> combine t1)
        , test "Test unit counting" <|
            \() ->
                Expect.equal
                    (Dict.fromList
                        [ ( 'a', 1 )
                        , ( 'b', 2 )
                        , ( 'c', 3 )
                        ]
                    )
                    (countUnits "abbccc")
        , test "Test unit cancelling" <|
            \() ->
                Expect.equal
                    (Dict.fromList
                        [ ( 'a', 0 )
                        , ( 'b', 1 )
                        , ( 'c', 2 )
                        , ( 'd', -1 )
                        ]
                    )
                    (cancelUnits "abbccc" "abcd")
        , test "Test unit cancelling, part deux" <|
            \() ->
                Expect.equal
                    ( Dict.fromList
                        [ ( 'b', 1 )
                        , ( 'c', 2 )
                        ]
                    , Dict.fromList
                        [ ( 'd', 1 )
                        ]
                    )
                    (cancelUnitsDeux "abbccc" "abcd")
        ]
