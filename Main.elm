port module Main exposing (..)

import Array
import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Markdown


-- hogefuga
-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- PORTS


port roomInfoSender : RoomInfo -> Cmd msg


port userNameSender : String -> Cmd msg


port editingMsgSender : String -> Cmd msg


port editedMsgSender : String -> Cmd msg



-- ()->にできない？


port exitRoomSender : String -> Cmd msg


port publicRoomsReceiver : (List String -> msg) -> Sub msg


port editingMsgReceiver : (List EditingMsg -> msg) -> Sub msg


port editedMsgReceiver : (EditedMsg -> msg) -> Sub msg


port enterRoomReceiver : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { page : Page
    , roomID : String
    , isPublic : Bool
    , userName : String
    , publicRooms : List String
    , myMessage : String
    , editingMessage : List EditingMsg
    , editedMessage : List EditedMsg
    , hasNameInputted : Bool
    }


type Page
    = Portal
    | Room


type alias RoomInfo =
    { roomID : String, isPublic : Bool }


type alias EditingMsg =
    { name : String
    , message : String
    }


type alias EditedMsg =
    { name : String
    , message : String
    , time : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Portal "" True "" [] "" [] [] False
    , Cmd.none
    )



-- UPDATE


type Msg
    = RoomInfoSend
    | UserNameSend
    | EditingMsgSend String
    | EditedMsgSend
    | ExitRoomSend
    | PublicRoomsRecv (List String)
    | EditingMsgRecv (List EditingMsg)
    | EditedMsgRecv EditedMsg
    | EnterRoomRecv String
    | RoomIDChanged String
    | UserNameChanged String
    | RoomTypeChanged



--type alias ListStr = List String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RoomInfoSend ->
            ( model
            , roomInfoSender { roomID = model.roomID, isPublic = model.isPublic }
            )

        UserNameSend ->
            ( { model
                | hasNameInputted = True
              }
            , userNameSender model.userName
            )

        EditingMsgSend editingMsg ->
            ( { model
                | myMessage = editingMsg
              }
            , editingMsgSender editingMsg
            )

        EditedMsgSend ->
            ( { model
                | myMessage = ""
              }
            , editedMsgSender model.myMessage
            )

        ExitRoomSend ->
            ( { model
                | page = Portal
                , userName = ""
                , hasNameInputted = False
              }
            , exitRoomSender "hoge"
            )

        PublicRoomsRecv rooms ->
            ( { model
                | publicRooms = rooms
              }
            , Cmd.none
            )

        EditingMsgRecv editingMsg ->
            ( { model
                | editingMessage = editingMsg
              }
            , Cmd.none
            )

        EditedMsgRecv editedMsg ->
            ( { model
                | editedMessage = editedMsg :: model.editedMessage
              }
            , Cmd.none
            )

        EnterRoomRecv tmp ->
            ( { model
                | page = Room
              }
            , Cmd.none
            )

        RoomIDChanged roomID ->
            ( { model
                | roomID = roomID
              }
            , Cmd.none
            )

        RoomTypeChanged ->
            ( { model
                | isPublic = not model.isPublic
              }
            , Cmd.none
            )

        UserNameChanged userName ->
            ( { model
                | userName = userName
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ publicRoomsReceiver PublicRoomsRecv
        , editingMsgReceiver EditingMsgRecv
        , editedMsgReceiver EditedMsgRecv
        , enterRoomReceiver EnterRoomRecv
        ]



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Portal ->
            div []
                [ headerHtml
                , button [ onClick ExitRoomSend ] [ text "Exit" ]
                , div []
                    [ input
                        [ placeholder "room ID + Enter"
                        , onInput RoomIDChanged
                        , onEnter RoomInfoSend
                        ]
                        []
                    ]
                , div []
                    [ text "The room will be"
                    , button [ onClick RoomTypeChanged ]
                        [ text
                            (if model.isPublic then
                                "public"

                             else
                                "private"
                            )
                        ]
                    ]
                , h3 [] [ text "Public rooms" ]
                , ul [] (List.map viewRooms model.publicRooms)
                ]

        Room ->
            div []
                [ headerHtml
                , button [ onClick ExitRoomSend ] [ text "Exit" ]
                , p []
                    [ text
                        (if model.isPublic then
                            "public"

                         else
                            "private"
                        )
                    ]
                , div []
                    [ input
                        [ placeholder "Your Name + Enter"
                        , value model.userName
                        , onInput UserNameChanged
                        , onEnter UserNameSend
                        , disabled
                            (if model.hasNameInputted then
                                True

                             else
                                False
                            )
                        ]
                        []
                    ]
                , div []
                    [ input
                        [ placeholder "message + Enter"
                        , value model.myMessage
                        , onInput EditingMsgSend
                        , onEnter EditedMsgSend
                        , hidden
                            (if model.userName == "" then
                                True

                             else
                                False
                            )
                        ]
                        []
                    ]
                , ul [] (List.map viewEditingMsg model.editingMessage)
                , ul [] (List.map viewEditedMsg model.editedMessage)
                ]


viewRooms : String -> Html msg
viewRooms publicRoom =
    li [] [ text publicRoom ]


viewEditingMsg : EditingMsg -> Html msg
viewEditingMsg msg =
    li [] [ text (msg.name ++ ":" ++ msg.message) ]


viewEditedMsg : EditedMsg -> Html msg
viewEditedMsg msg =
    li [] [ text (msg.name ++ ":" ++ msg.message ++ "(" ++ msg.time ++ ")") ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- CONSTATNTS


headerHtml : Html msg
headerHtml =
    Markdown.toHtml [ class "content" ] """

# Chat *Allegro*(β)

### About
- A simple chat app.
- Your input will be sent to a server on each key stroke. This means your message will be represented to other participant(s) in the chatting room "in realtime".
- You can start a new chatting room or join an existing room by inputting room ID in a textbox below.
- If you start a new room as **public**, the room ID will be listed below and open to everyone.
- If the room is **private**, only those who know the room ID of that room can participate.
 - (\\* Please note that this app is in beta. Thus, **You may well refrain from dumping any confidential contents here.**)

"""
