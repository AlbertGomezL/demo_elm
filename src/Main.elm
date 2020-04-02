module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (..)

-- MODEL

type alias Product = 
    { id : Int
    , name : String
    , cost : Float
    }

type alias Model = List Product

newProduct : Int -> String -> Float -> Product
newProduct id name cost = 
    { id = id
    , name = name
    , cost = cost
    }

initModel : Model
initModel = 
    [ newProduct 1 "Tuercas" 5.5
    , newProduct 4 "Martillo" 7.0
    , newProduct 2 "Tornillos" 4.2
    , newProduct 3 "Rastrillo" 10
    , newProduct 5 "Destornillador" 3.4
    , newProduct 7 "Pala" 12
    , newProduct 8 "Alicates" 4
    , newProduct 6 "Llave" 6.3
    , newProduct 9 "Taladro" 22.5
    ]


-- UPDATE

type Msg 
    = NoOp 
    | SortByNombre
    | SortByPrecio
    | Delete Int

update: Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        SortByNombre ->
            List.sortBy .name model
        SortByPrecio ->
            List.sortBy .cost model            
        Delete id ->
            List.filter (\t -> t.id /= id) model


-- VIEW

totalCost : List Product -> Float
totalCost products =
    let
        costs = List.map .cost products
    in
        List.foldl (+) 0 costs

pageHeader : Html msg
pageHeader = 
    h1  [style "background-color" "darkcyan"
        , style "margin-top" "0px"
        , style "height" "100px"
        , style "text-align" "center"
        , style "padding-top" "50px"
        ] [text "Lista de productos"]

pageFooter : Html msg
pageFooter = 
    footer [] [
        a [href "https://github.com/AlbertGomezL/demo_elm.git"]
          [text "Repositorio de código"]
    ]

article : Product -> Html Msg
article art = 
    li 
        [ style "height" "40px"
        , style "padding-top" "20px"
        , style "margin-left" "20%"
        , style "margin-right" "20%"
        , style "margin-bottom" "10px"
        , style "border-radius" "15px"
        , style "background-color" "lightblue"
        ] 
        [ span [class "name", style "margin-left" "10%"] [text art.name]
        , span [class "cost", style "margin-left" "25%"] [text (String.fromFloat art.cost), text " €"]
        , button [class "delete"
                 , style "position" "absolute"
                 , style "right" "25%"
                 , onClick (Delete art.id)
                 ] 
                 [text "x"]
        ]

articles : List Product -> Html Msg
articles products =
    let
        entries = List.map article products
        elements = entries ++ [showTotal (totalCost products)]
    in
        ul [style "margin-left" "50px", style "margin-right" "50px"] elements

showTotal : Float -> Html msg
showTotal total =
    li 
        [ class "total"
        , style "height" "40px"
        , style "padding-top" "20px"
        , style "margin-left" "20%"
        , style "margin-right" "20%"
        , style "margin-bottom" "10px"
        , style "border-radius" "15px"
        , style "background-color" "mediumspringgreen"
        ]
        [ span [class "label", style "margin-left" "10%"] [text "Precio total"]
        , span [class "cost", style "margin-left" "25%"] [text (String.fromFloat total), text " €"]
        ]

view : Model -> Html Msg
view model = 
    div [id "container"] 
        [ pageHeader
        , div[style "text-align" "center"] 
            [ span [] [text "Ordenar por "]
            , button
                [class "sort left", onClick SortByNombre]
                [text "Nombre"]
            , button
                [class "sort", onClick SortByPrecio]
                [text "Precio"]
            ]
        , articles model
        , pageFooter
        ]


-- MAIN
main = 
    Browser.sandbox 
    { init = update SortByNombre initModel
    , view = view
    , update = update
    }

