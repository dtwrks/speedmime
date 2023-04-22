module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import Time


wordsPerTurn : Int
wordsPerTurn =
    5


turnDuration : Int
turnDuration =
    10


type alias Model =
    { teamScore : Dict Int Int
    , teamTurn : Int
    , turn : Int
    , turnInSeconds : Int
    , turnStarted : Bool
    , turnWords : List String
    , turnWordsGuessed : Set String
    , wordsArchive : List String
    }


init : List String -> Model
init words =
    { teamScore = Dict.fromList [ ( 0, 0 ), ( 1, 0 ) ]
    , teamTurn = 0
    , turn = 0
    , turnInSeconds = 0
    , turnStarted = False
    , turnWords = []
    , turnWordsGuessed = Set.empty
    , wordsArchive = words
    }


type Msg
    = StartTurn
    | TurnTick
    | Word String


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartTurn ->
            { model
                | turn = model.turn + 1
                , teamTurn =
                    modBy
                        (Dict.size model.teamScore)
                        (model.teamTurn + 1)
                , turnStarted = True
                , turnInSeconds = 0
                , turnWordsGuessed = Set.empty
                , turnWords = List.take wordsPerTurn model.wordsArchive
                , wordsArchive = List.drop wordsPerTurn model.wordsArchive   
            }

        TurnTick ->
            let
                turnInSeconds : Int
                turnInSeconds =
                    model.turnInSeconds + 1
            in
            if turnInSeconds < turnDuration then
                { model | turnInSeconds = turnInSeconds }
            else
                { model
                    | turnStarted = False
                    , teamScore =
                        Dict.update
                            model.teamTurn
                            (\score ->
                                score
                                    |> Maybe.withDefault 0
                                    |> (+) (Set.size model.turnWordsGuessed)
                                    |> Just
                            )
                            model.teamScore
                    }

        Word word ->
            let
                turnWordsGuessed : Set String
                turnWordsGuessed =
                    Set.insert word model.turnWordsGuessed

                turnRemainingWords : List String
                turnRemainingWords =
                    model.turnWords
                        |> List.filter (\w -> not <| Set.member w turnWordsGuessed)
                        |> Debug.log ""
            in
            if List.isEmpty turnRemainingWords then
                { model
                    | turnWordsGuessed = turnWordsGuessed
                    , turnWords = model.turnWords ++ List.take wordsPerTurn model.wordsArchive
                    , wordsArchive = List.drop wordsPerTurn model.wordsArchive
                }

            else
                { model | turnWordsGuessed = turnWordsGuessed }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.turnStarted then
        Time.every 1000 (\_ -> TurnTick)

    else
        Sub.none


view : Model -> H.Html Msg
view model =
    H.div []
        [ model.teamScore
            |> Dict.toList
            |> List.map (\(number, score) ->
                H.li
                    []
                    [ H.text "Team "
                    , H.text (String.fromInt number)
                    , H.text ": "
                    , H.text (String.fromInt score)
                    ]
            )
            |> H.ul []
        
        , if model.turnStarted then
            H.div
                []
                [ H.p [] [ H.text (String.fromInt model.turnInSeconds) ]
                , model.turnWords
                    |> List.map
                        (\word ->
                            H.li
                                []
                                [ H.button
                                    [ HE.onClick (Word word)
                                    , if Set.member word model.turnWordsGuessed then
                                        HA.disabled True

                                      else
                                        HA.class ""
                                    ]
                                    [ H.text word ]
                                ]
                        )
                    |> H.ul []
                ]

          else
            H.div
                []
                [ H.button
                    [ HE.onClick StartTurn ]
                    [ H.text "Start turn" ]
                ]
        ]


main : Program (List String) Model Msg
main =
    Browser.element
        { init = \words -> ( init words, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

