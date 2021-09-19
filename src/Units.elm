module Units exposing (..)

import Dict exposing (..)


type Quantity
    = Quantity Float String


type Term
    = Term Quantity Quantity



-- = 0.5 J/s   500 GWh/year = ____ J/s
-- `
-- 5 m/s^2 = 5 = m/s/s = 5 (m/s)/s = 5 m   1 = 5 m    1   1
--                         - x -       x  - x -
--                         s   s          s   s
--
-- Just use tuples?
-- Lists? m/s/s


combine : Term -> Term -> Term
combine (Term (Quantity fn1 sn1) (Quantity fd1 sd1)) (Term (Quantity fn2 sn2) (Quantity fd2 sd2)) =
    Term (Quantity (fn1 * fn2) (sn1 ++ sn2)) (Quantity (fd1 * fd2) (sd1 ++ sd2))


inserts : Char -> Dict Char Int -> Dict Char Int
inserts c d =
    case Dict.get c d of
        Just v ->
            Dict.insert c (v + 1) d

        Nothing ->
            Dict.insert c 1 d


countUnits : String -> Dict Char Int
countUnits s =
    String.foldl inserts Dict.empty s


onlyLeft : Char -> Int -> Dict Char Int -> Dict Char Int
onlyLeft k v accum =
    Dict.insert k v accum


onlyRight : Char -> Int -> Dict Char Int -> Dict Char Int
onlyRight k v accum =
    Dict.insert k (-1 * v) accum


inBoth : Char -> Int -> Int -> Dict Char Int -> Dict Char Int
inBoth k v1 v2 accum =
    Dict.insert k (v1 - v2) accum


reallyCancel : Dict Char Int -> Dict Char Int -> Dict Char Int
reallyCancel n d =
    Dict.merge onlyLeft inBoth onlyRight n d Dict.empty


cancelUnits : String -> String -> Dict Char Int
cancelUnits n d =
    reallyCancel (countUnits n) (countUnits d)


splitTest : Char -> Int -> Bool
splitTest _ v =
    v > 0


valueIsNonZero : Char -> Int -> Bool
valueIsNonZero _ v =
    v /= 0


filterOutOnes : Dict Char Int -> Dict Char Int
filterOutOnes d =
    Dict.filter valueIsNonZero d


fixSign : Char -> Int -> Int
fixSign _ v =
    -1 * v


fixSignDeux : ( Dict Char Int, Dict Char Int ) -> ( Dict Char Int, Dict Char Int )
fixSignDeux ( n, d ) =
    ( n, Dict.map fixSign d )


cancelUnitsDeux : String -> String -> ( Dict Char Int, Dict Char Int )
cancelUnitsDeux n d =
    fixSignDeux <| Dict.partition splitTest (filterOutOnes <| cancelUnits n d)
