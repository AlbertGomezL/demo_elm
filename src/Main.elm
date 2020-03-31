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
    h1 [] [text "App Products"]

pageFooter : Html msg
pageFooter = 
    footer [] [
        a [href "https://www.google.es"]
          [text "Aplicación de compra de products"]
    ]

article : Product -> Html Msg
article art = 
    li [] 
    [ span [class "name"] [text art.name]
    , span [class "cost"] [text (String.fromFloat art.cost)]
    , button [class "delete", onClick (Delete art.id)] [text "x"]
    ]

articles : List Product -> Html Msg
articles products =
    let
        entries = List.map article products
        elements = entries ++ [showTotal (totalCost products)]
    in
        ul [] elements

showTotal : Float -> Html msg
showTotal total =
    li [class "total"]
        [ span [class "label"] [text "Total"]
        , span [class "cost"] [text (String.fromFloat total)]
        ]

view : Model -> Html Msg
view model = 
    div [id "container"] 
        [ pageHeader
        , button
            [class "sort left", onClick SortByNombre]
            [text "Nombre"]
        , button
            [class "sort", onClick SortByPrecio]
            [text "Precio"]
        , articles model
        , pageFooter
        ]


-- main : Html msg
main = 
    Browser.sandbox 
    { init = update SortByNombre initModel
    , view = view
    , update = update
    }
    -- view initModel
    -- Html.text "Hola mundo!"


