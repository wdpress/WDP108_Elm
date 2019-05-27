module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)

type alias ToDo =    --(d1)
    { id : Int
    , message : String
    , status : ToDoStatus
    }

type ToDoStatus    --(d2)
    = Wip
    | Done

type alias Model =    --(d3)
    { toDoList : List ToDo
    , message : String
    , nextId : Int
    }

init : Model    --(d4)
init =
    { toDoList = []
    , message = ""
    , nextId = 0
    }

-- (d1)
type Msg
    = EnteredMessage String
    | ClickedCreateButton

-- (d2)
update : Msg -> Model -> Model
update msg model =
   case msg of
      EnteredMessage message ->
         { model | message = message }

      ClickedCreateButton ->
         let    --(d3)
            newToDo =
               { id = model.nextId
               , message = model.message
               , status = Wip
               }
         in
         { model
            | toDoList = newToDo :: model.toDoList --(d4)
            , message = ""    --(d5)
            , nextId = model.nextId + 1    --(d6)
         }

view : Model -> Html Msg    --(d1)
view model =
    div []
        [ input
            [ type_ "text"
            , onInput EnteredMessage --(d2)
            , value model.message
            ]
            []
        , button
            [ onClick ClickedCreateButton ] --(d3)
            [ text "作成" ]
        , div
            []
            (List.map viewToDo model.toDoList) --(d4)
        ]

viewToDo : ToDo -> Html Msg    --(d5)
viewToDo { message } =
    div [] [ text message ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }