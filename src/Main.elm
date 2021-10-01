module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html,h1, button, div, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)

---- MODEL ----
type alias Model =
    { board : Array Masu
    , turn : Player
    , status : Status
    }

type Player
    = Maru
    | Batu


type Status
    = GameEnd Player
    | Gaming


type Masu
    = Used Player
    | Blank


---- INIT ----

init : ( Model, Cmd Msg )
init =
    ( { board =
            Array.fromList
                [ Blank
                , Blank
                , Blank
                , Blank
                , Blank
                , Blank
                , Blank
                , Blank
                , Blank
                ]
      , turn = Maru
      , status = Gaming
      }
    , Cmd.none
    )

---- VIEW ----
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [(text ( winer_str model.status) )]
        , div [] [ square_button 0 model, square_button 1 model, square_button 2 model ]
        , div [] [ square_button 3 model, square_button 4 model, square_button 5 model ]
        , div [] [ square_button 6 model, square_button 7 model, square_button 8 model ]
        , text (player_str model.turn ++ "のターン")
        ]

winer_str : Status -> String
winer_str status =
    case status of
        Gaming ->
            "頑張るぞい"
        GameEnd player ->
            player_str player ++ "の勝ち！！"

player_str : Player -> String
player_str player =
    case player of
        Maru ->
            "○"

        Batu ->
            "×"


square_button : Int -> Model -> Html Msg
square_button zahyou model =
    let
        masu_moji =
            case Array.get zahyou model.board of
                Just Blank ->
                    "-"

                Just (Used Maru) ->
                    "○"

                Just (Used Batu) ->
                    "×"

                Nothing ->
                    "?"
    in
    button [ onClick (Put zahyou) ] [ text masu_moji ]


---- UPDATE ----
type Msg
    = Put Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Put zahyo ->
            let
                new_board =
                    board_process zahyo model

                next_turn =
                    case model.turn of
                        Maru ->
                            Batu

                        Batu ->
                            Maru
                new_model = 
                    case model.status of
                        Gaming ->
                            { board = new_board, turn = next_turn, status = (judge_win model.board) }
                        _ ->
                            model 
            in
            ( new_model
            , Cmd.none
            )

-- クソコード閲覧注意!!!
judge_win : Array Masu -> Status
judge_win board =
    let
        winner =
            -- 横
            if Array.get 0 board == Array.get 1 board && Array.get 2 board == Array.get 3 board then
                Array.get 0 board

            else if Array.get 3 board == Array.get 4 board && Array.get 4 board == Array.get 5 board then
                Array.get 3 board

            else if Array.get 6 board == Array.get 7 board && Array.get 7 board == Array.get 8 board then
                Array.get 6 board
                -- 縦

            else if Array.get 0 board == Array.get 3 board && Array.get 3 board == Array.get 6 board then
                Array.get 0 board

            else if Array.get 1 board == Array.get 4 board && Array.get 4 board == Array.get 7 board then
                Array.get 1 board

            else if Array.get 2 board == Array.get 5 board && Array.get 5 board == Array.get 8 board then
                Array.get 2 board
                -- 斜

            else if Array.get 0 board == Array.get 4 board && Array.get 4 board == Array.get 8 board then
                Array.get 0 board

            else if Array.get 2 board == Array.get 4 board && Array.get 4 board == Array.get 6 board then
                Array.get 2 board
                -- 未決着

            else
                Nothing
    in
    case winner of
        Nothing ->
            Gaming

        Just Blank ->
            Gaming

        Just ( Used winner_ ) ->
            GameEnd winner_



board_process : Int -> Model -> Array Masu
board_process zahyou model =
    let
        put_masu =
            Used model.turn
    in
    Array.fromList
        (List.map
            (\tmp ->
                if tmp == zahyou then
                    put_masu

                else
                    case Array.get tmp model.board of
                        Nothing ->
                            Blank

                        Just masu ->
                            masu
            )
            (List.range 0 8)
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
