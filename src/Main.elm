module Main exposing (Model, Msg(..), Player, Team, init, main, update, view)

import Array exposing (Array)
import Browser
import Csv.Decode as Decode exposing (Decoder)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, input, p, span, table, td, text, textarea, th, thead, tr)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import Random
import Random.List
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Player =
    { apexName : String
    , discordName : String
    , rank : String
    , selected : Bool
    }


type alias Team =
    List (Maybe Player)


type alias Model =
    { csv : Maybe String
    , players : Array Player
    , teams : List Team
    , err : Maybe Decode.Error
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { csv = Nothing
      , players = Array.empty
      , teams = []
      , err = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Pick
    | GotFile File
    | FileRead String
    | Toggle Int
    | ShufflePlayers
    | CreateTeams (List Player)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.file [ "text/*" ] GotFile
            )

        GotFile file ->
            ( model, Task.perform FileRead (File.toString file) )

        FileRead contents ->
            ( { model | players = playersFromResult (playerDecoder contents) }, Cmd.none )

        Toggle index ->
            let
                updatePlayer i p =
                    if index == i then
                        { p | selected = not p.selected }

                    else
                        p
            in
            ( { model | players = Array.indexedMap updatePlayer model.players }, Cmd.none )

        ShufflePlayers ->
            ( model, Random.generate CreateTeams (Random.List.shuffle (Array.toList model.players)) )

        CreateTeams players ->
            ( { model | teams = createTeams (sortedPlayers (Array.fromList players)) }, Cmd.none )


createTeams players =
    List.range 0 20
        |> List.map (\x -> [ Array.get x players, Array.get (x + 20) players, Array.get (60 - x) players ])


sortedPlayers players =
    players
        |> selectedPlayers
        |> List.sortBy rankValue
        |> Array.fromList


selectedPlayers : Array Player -> List Player
selectedPlayers players =
    players
        |> Array.toList
        |> List.filter (\p -> p.selected == True)


rankValue player =
    case player.rank of
        "Predator" ->
            1

        "Masters" ->
            2

        "Diamond" ->
            3

        "Platinum" ->
            4

        "Gold" ->
            5

        "Silver" ->
            6

        "Bronze" ->
            7

        _ ->
            8


playersFromResult : Result error (List DecoderResult) -> Array Player
playersFromResult input =
    case input of
        Err err ->
            Array.fromList []

        Ok players ->
            Array.fromList (List.map (\p -> { apexName = p.apexName, discordName = p.discordName, rank = p.rank, selected = False }) players)


playerDecoder : String -> Result Decode.Error (List DecoderResult)
playerDecoder contents =
    Decode.decodeCsv Decode.FieldNamesFromFirstRow playerParser contents


type alias DecoderResult =
    { apexName : String
    , discordName : String
    , rank : String
    }


playerParser : Decoder DecoderResult
playerParser =
    Decode.into DecoderResult
        |> Decode.pipeline (Decode.column 1 Decode.string)
        |> Decode.pipeline (Decode.column 3 Decode.string)
        |> Decode.pipeline (Decode.column 6 Decode.string)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ style "width" "49%", style "display" "inline-block", style "max-height" "600px", style "overflow" "auto" ]
            [ button [ onClick Pick ] [ text "Upload CSV" ]
            , table []
                (thead []
                    [ th [] [ text "Apex Name" ]
                    , th [] [ text "Discord Name" ]
                    , th [] [ text "Rank Name" ]
                    ]
                    :: Array.toList (Array.indexedMap playerTable model.players)
                )
            ]
        , div [ style "width" "49%", style "display" "inline-block", style "max-height" "600px", style "overflow" "auto" ]
            [ button [ onClick ShufflePlayers ] [ text ("Create Teams (selected " ++ String.fromInt (List.length (selectedPlayers model.players)) ++ ")") ]
            , div [] (List.map teamTable model.teams)
            ]
        ]


teamTable team =
    table []
        [ thead []
            [ th [ style "width" "300px" ] [ text "Player 1" ]
            , th [ style "width" "300px" ] [ text "Player 2" ]
            , th [ style "width" "300px" ] [ text "Player 3" ]
            ]
        , tr [] (List.map teamRow team)
        ]


teamRow team =
    case team of
        Nothing ->
            td [ style "border" "1px solid #555" ] []

        Just player ->
            td [ style "border" "1px solid #555" ]
                [ div [] [ text player.apexName ]
                , div [ style "color" "#666" ] [ text player.discordName ]
                , div [ style "color" "#99f" ] [ text player.rank ]
                ]


playerTable : Int -> Player -> Html Msg
playerTable index player =
    tr [ style "background-color" (getBackgroundColor player.selected), style "height" "30px", onClick (Toggle index) ]
        [ td [] [ input [ type_ "checkbox", checked player.selected, onClick (Toggle index) ] [] ]
        , td [] [ text player.apexName ]
        , td [ style "color" "#666" ] [ text player.discordName ]
        , td [ style "color" "#ebe" ] [ text player.rank ]
        ]


getBackgroundColor value =
    if value then
        "red"

    else
        "white"
