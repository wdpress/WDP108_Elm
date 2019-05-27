module Main exposing (EditForm, Model, Msg(..), ToDo, ToDoId, ToDoStatus(..), beDone, find, init, main, nextToDoId, replace, update, updateMessage, updateToDo, view, viewCreateForm, viewEditForm, viewTitle, viewToDo, viewToDoId, viewToDoList, viewToDoListItem, viewToDoStatus)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)



---- ToDoList ----


type alias ToDo =
    { id : ToDoId
    , message : String
    , status : ToDoStatus
    }


type alias ToDoId =
    Int


type ToDoStatus
    = Wip
    | Done


updateToDo : (ToDo -> ToDo) -> ToDoId -> List ToDo -> List ToDo
updateToDo f toDoId toDoList =
    let
        makeNextToDo toDo =
            if toDo.id == toDoId then
                f toDo

            else
                toDo
    in
    List.map makeNextToDo toDoList


nextToDoId : List ToDo -> ToDoId
nextToDoId toDoList =
    let
        max a b =
            if a > b then
                a
            else
                b
    in
    toDoList
        |> List.foldl (\toDo biggerId -> max toDo.id biggerId) 0
        |> (+) 1


beDone : ToDoId -> List ToDo -> List ToDo
beDone toDoId toDoList =
    updateToDo (\toDo -> { toDo | status = Done }) toDoId toDoList


find : ToDoId -> List ToDo -> Maybe ToDo
find toDoId toDoList =
    let
        findToDo toDo acc =
            case ( acc, toDo.id == toDoId ) of
                ( Nothing, True ) ->
                    Just toDo

                _ ->
                    acc
    in
    List.foldl findToDo Nothing toDoList


replace : ToDo -> List ToDo -> List ToDo
replace toDo toDoList =
    updateToDo (\_ -> toDo) toDo.id toDoList



---- EditForm ----


type alias EditForm =
    Maybe ToDo


updateMessage : String -> EditForm -> EditForm
updateMessage nextMessage editForm =
    Maybe.map (\toDo -> { toDo | message = nextMessage }) editForm



---- Model ----


type alias Model =
    { toDoList : List ToDo
    , createForm : String
    , editForm : EditForm
    }


init : Model
init =
    { toDoList = []
    , createForm = ""
    , editForm = Nothing
    }



---- UPDATE ----


type Msg
    = EnteredCreateFormMessage String
    | ClickedCreateToDoButton
    | ClickedDoneButton ToDoId
    | ClickedBeginEditButton ToDoId
    | EnteredEditFormMessage String
    | ClickedSaveEditButton


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredCreateFormMessage message ->
            { model | createForm = message }

        ClickedCreateToDoButton ->
            let
                newToDo =
                    { id = nextToDoId model.toDoList
                    , message = model.createForm
                    , status = Wip
                    }
            in
            { model | toDoList = newToDo :: model.toDoList, createForm = "" }

        ClickedDoneButton toDoId ->
            { model | toDoList = beDone toDoId model.toDoList }

        ClickedBeginEditButton toDoId ->
            { model | editForm = find toDoId model.toDoList }

        EnteredEditFormMessage message ->
            { model | editForm = updateMessage message model.editForm }

        ClickedSaveEditButton ->
            case model.editForm of
                Just tmpToDo ->
                    { model | toDoList = replace tmpToDo model.toDoList, editForm = Nothing }

                _ ->
                    model



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "toDoPage" ]
        [ viewTitle
        , viewCreateForm model.createForm
        , viewToDoList model.editForm model.toDoList
        ]


viewTitle : Html msg
viewTitle =
    div [ class "pageTitle" ] [ text "ToDo List" ]


viewCreateForm : String -> Html Msg
viewCreateForm formMessage =
    div [ class "createForm" ]
        [ input [ onInput EnteredCreateFormMessage, type_ "text", value formMessage, class "createForm_messageInput" ] []
        , button [ onClick ClickedCreateToDoButton, class "createForm_createButton" ]
            [ text "作成"
            ]
        ]


viewToDoList : EditForm -> List ToDo -> Html Msg
viewToDoList editForm toDoList =
    div [ class "toDoList" ] <| List.map (viewToDoListItem editForm) toDoList


viewToDoListItem : EditForm -> ToDo -> Html Msg
viewToDoListItem editForm toDo =
    case editForm of
        Just tmpToDo ->
            if toDo.id == tmpToDo.id then
                viewEditForm tmpToDo

            else
                viewToDo toDo

        _ ->
            viewToDo toDo


viewToDo : ToDo -> Html Msg
viewToDo { id, message, status } =
    div [ class "toDoItem" ]
        [ viewToDoStatus id status
        , viewToDoId id
        , span [ class "toDoItem_message" ] [ text <| message ]
        , button [ onClick <| ClickedBeginEditButton id, class "toDoItem_editButton" ] [ text "編集" ]
        ]


viewEditForm : ToDo -> Html Msg
viewEditForm { id, message, status } =
    div [ class "toDoEditForm" ]
        [ viewToDoStatus id status
        , viewToDoId id
        , input [ value message, onInput EnteredEditFormMessage, type_ "text", class "toDoEditForm_messageInput" ] []
        , button [ onClick ClickedSaveEditButton, class "toDoEditForm_saveButton" ] [ text "保存" ]
        ]


viewToDoId : ToDoId -> Html msg
viewToDoId toDoId =
    span [ class "toDoId" ] [ text <| "#" ++ String.fromInt toDoId ]


viewToDoStatus : ToDoId -> ToDoStatus -> Html Msg
viewToDoStatus toDoId status =
    let
        className =
            case status of
                Wip ->
                    "toDoItem_status_wip"

                Done ->
                    "toDoItem_status_done"
    in
    button [ onClick <| ClickedDoneButton toDoId, class className ] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox
        { view = view
        , init = init
        , update = update
        }
