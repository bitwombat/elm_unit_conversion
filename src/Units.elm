module Units exposing (..)

{- TODO:

   Support 5 J/s
             ---
             km

   Split km into 1000 m so that k's can cancel k's

-}

import Dict exposing (..)
import List
import String


type Quantity
    = Quantity Float (List String)


type Term
    = Term Quantity Quantity


unity : Quantity
unity =
    Quantity 1.0 []


unityFraction : Term
unityFraction =
    Term unity unity


multiply : Term -> Term -> Term
multiply (Term (Quantity fn1 sn1) (Quantity fd1 sd1)) (Term (Quantity fn2 sn2) (Quantity fd2 sd2)) =
    Term (Quantity (fn1 * fn2) (sn1 ++ sn2)) (Quantity (fd1 * fd2) (sd1 ++ sd2))


cancel : Term -> Term
cancel (Term (Quantity fn1 sn1) (Quantity fd1 sd1)) =
    let
        ( numer, denom ) =
            cancelUnits sn1 sd1
    in
    Term (Quantity (fn1 / fd1) numer) (Quantity 1.0 denom)


multiplyAndCancel : Term -> Term -> Term
multiplyAndCancel t1 t2 =
    multiply t1 t2
        |> cancel


countUnitOccurrences : String -> Dict String Int -> Dict String Int
countUnitOccurrences c d =
    case Dict.get c d of
        Just v ->
            Dict.insert c (v + 1) d

        Nothing ->
            Dict.insert c 1 d


unitListToDict : List String -> Dict String Int
unitListToDict s =
    List.foldl countUnitOccurrences Dict.empty s


unitInNumer : String -> Int -> Dict String Int -> Dict String Int
unitInNumer k v accum =
    Dict.insert k v accum


unitInDenom : String -> Int -> Dict String Int -> Dict String Int
unitInDenom k v accum =
    Dict.insert k (-1 * v) accum


unitInBoth : String -> Int -> Int -> Dict String Int -> Dict String Int
unitInBoth k v1 v2 accum =
    Dict.insert k (v1 - v2) accum


combineUnitNumerAndDenom : List String -> List String -> Dict String Int
combineUnitNumerAndDenom ns ds =
    let
        n =
            unitListToDict ns

        d =
            unitListToDict ds
    in
    Dict.merge unitInNumer unitInBoth unitInDenom n d Dict.empty


valueIsNonZero : String -> Int -> Bool
valueIsNonZero _ v =
    v /= 0


{-| If the exponent is 0, that's the same as 1, and not needed in unit
conversion multiplications.
-}
filterOutOnes : Dict String Int -> Dict String Int
filterOutOnes d =
    Dict.filter valueIsNonZero d


isPositive : String -> Int -> Bool
isPositive _ v =
    v > 0


splitIntoNumerAndDenom : Dict String Int -> ( Dict String Int, Dict String Int )
splitIntoNumerAndDenom =
    Dict.partition isPositive


{-| The denominator has negative numbers (that was the result of the combining,
and left in place for the splitting)
Give the denominator the correct exponents by negating all of them.
-}
fixDenomSign : ( Dict String Int, Dict String Int ) -> ( Dict String Int, Dict String Int )
fixDenomSign ( n, d ) =
    ( n, Dict.map (\_ v -> negate v) d )


toStr : String -> Int -> List String -> List String
toStr unit exponent ss =
    List.append ss (List.repeat exponent unit)


dictToList : Dict String Int -> List String
dictToList t =
    foldl toStr [] t


cancelUnits : List String -> List String -> ( List String, List String )
cancelUnits n d =
    let
        ( nDict, dDict ) =
            combineUnitNumerAndDenom n d
                |> filterOutOnes
                |> splitIntoNumerAndDenom
                |> fixDenomSign
    in
    ( dictToList nDict, dictToList dDict )


toDisplayStr : String -> Int -> List String -> List String
toDisplayStr unit exponent ss =
    case exponent of
        1 ->
            List.append ss [ unit ]

        _ ->
            List.append ss [ unit ++ "<sup>" ++ String.fromInt exponent ++ "</sup>" ]


dictToDisplayList : Dict String Int -> List String
dictToDisplayList t =
    foldl toDisplayStr [] t


multiplyList : List Term -> Term
multiplyList ts =
    List.foldl multiplyAndCancel unityFraction ts


concatWithDot : String -> String -> String
concatWithDot new existing =
    if existing == "" then
        new

    else
        new ++ "∙" ++ existing


listToString : List String -> String
listToString ss =
    List.foldr concatWithDot "" ss


dictFractToDisplayString : ( Dict String Int, Dict String Int ) -> ( String, String )
dictFractToDisplayString ( n, d ) =
    ( listToString (dictToDisplayList n), listToString (dictToDisplayList d) )



-- TODO: foldl gives the backwards result "cba" instead of "abc". Why? Turn the
-- foldls to foldrs throughout?
