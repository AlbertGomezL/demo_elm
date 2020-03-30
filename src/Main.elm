module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing (..)

-- MODEL

type alias Tema = 
    { titulo : String
    , duracion : String
    , id : Int
    }

type alias Model = List Tema

nuevoTema : String -> String -> Int -> Tema
nuevoTema titulo duracion id = 
    { titulo = titulo
    , duracion = duracion
    , id = id
    }

modeloInicial : Model
modeloInicial = 
    [ nuevoTema "1. Introduccion" "5" 1
    , nuevoTema "2. Tema1" "7" 4
    , nuevoTema "9. Cierre" "4" 2
    , nuevoTema "6. Otro tema más" "10" 3
    ]


-- UPDATE

type Msg = NoOp | SortByTitulo | SortByDuracion

update: Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model
        SortByTitulo ->
            List.sortBy .titulo model
        SortByDuracion ->
            List.sortBy .duracion model            


-- VIEW

pageHeader : Html msg
pageHeader = 
    h1 [] [text "Temario"]

pageFooter : Html msg
pageFooter = 
    footer [] [
        a [href "https://www.google.es"]
          [text "Generador de temarios"]
    ]

capitulo : Tema -> Html msg
capitulo cap = 
    li [] [
        span [class "titulo"] [text cap.titulo],
        span [class "duracion"] [text cap.duracion]
    ]

capitulos : List Tema -> Html msg
capitulos temas =
    ul [] (List.map capitulo temas)

view : Model -> Html Msg
view model = 
    div [id "container"] 
        [ pageHeader
        , button
            [class "sort left", onClick SortByTitulo]
            [text "Titulo"]
        , button
            [class "sort", onClick SortByDuracion]
            [text "Duracion"]
        , capitulos model
        , pageFooter
        ]


-- main : Html msg
main = 
    Browser.sandbox 
    { init = update SortByTitulo modeloInicial
    , view = view
    , update = update
    }
    -- view modeloInicial
    -- Html.text "Hola mundo!"

