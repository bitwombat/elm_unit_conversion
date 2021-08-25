module Units exposing (main)

import Html exposing (Html, div, h1, hr, img, input, text)
import Html.Attributes exposing (..)


type NumberWithUnit
    = Empty
    | Value Float String Float String


x : NumberWithUnit
x =
    Value 5.0 "J" 1.0 "s"


y : NumberWithUnit
y =
    Value 1.6 "km" 1 "mile"


chain : List NumberWithUnit
chain =
    [ x, y ]



--chainView : List NumberWithUnit -> Html msg
--chainView chain =


unitView : Html msg
unitView =
    div [ class "unit" ]
        [ input [ class "numerator" ] []
        , hr [] []
        , input [ class "denominator" ] []
        ]


view c =
    div
        [ class "content" ]
        [ h1 [] [ text "Magical Unit Converter (MUC)" ]
        , div [ id "expression" ]
            [ unitView, text "x", unitView, text "=", unitView ]
        ]


main =
    view chain
