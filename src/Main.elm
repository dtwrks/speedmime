module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import Time
import W.Styles
import W.Button


wordsPerTurn : Int
wordsPerTurn =
    5


turnDuration : Int
turnDuration =
    10


maxScore : Int
maxScore =
    50


type alias Model =
    { teams : Dict Int Team
    , teamTurn : Int
    , turnState : TurnState
    , turnWords : List String
    , turnWordsGuessed : Set String
    , wordsArchive : List String
    }


type alias Team =
    { name : String
    , score : Int
    , turns : Int
    }


type TurnState
    = Start
    | End
    | Running Int


teams : Dict Int Team
teams =
    [ { name = "Claro", score = 0, turns = 0 }
    , { name = "Escuro", score = 0, turns = 0 }
    ]
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


init : List String -> Model
init words =
    { teams = teams
    , teamTurn = 0
    , turnState = Start
    , turnWords = []
    , turnWordsGuessed = Set.empty
    , wordsArchive = words
    }


type Msg
    = StartTurn
    | EndTurn
    | TurnTick
    | ToggleWord String
    | MoreWords


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartTurn ->
            { model
                | teamTurn =
                    modBy
                        (Dict.size model.teams)
                        (model.teamTurn + 1)
                , turnState = Running 0
                , turnWordsGuessed = Set.empty
                , turnWords = List.take wordsPerTurn model.wordsArchive
                , wordsArchive = List.drop wordsPerTurn model.wordsArchive
            }

        EndTurn ->
            { model
                | turnState = Start
                , teams =
                    Dict.update
                        model.teamTurn
                        (\team ->
                            team
                                |> Maybe.map
                                    (\t ->
                                        { t
                                            | turns = t.turns + 1
                                            , score =
                                                t.score
                                                    + Set.size model.turnWordsGuessed
                                        }
                                    )
                        )
                        model.teams
            }

        TurnTick ->
            case model.turnState of
                Running seconds_ ->
                    let
                        seconds : Int
                        seconds =
                            seconds_ + 1
                    in
                    if seconds < turnDuration then
                        { model | turnState = Running seconds }

                    else
                        { model | turnState = End }
                _ ->
                    model

        ToggleWord word ->
            if Set.member word model.turnWordsGuessed then
                { model
                    | turnWordsGuessed =
                        Set.remove word model.turnWordsGuessed
                }

            else
                { model
                    | turnWordsGuessed =
                        Set.insert word model.turnWordsGuessed
                }

        MoreWords ->
            { model
                | turnWords = model.turnWords ++ List.take wordsPerTurn model.wordsArchive
                , wordsArchive = List.drop wordsPerTurn model.wordsArchive
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.turnState of
        Running _ ->
            Time.every 1000 (\_ -> TurnTick)

        _ ->
            Sub.none


view : Model -> H.Html Msg
view model =
    H.div
        []
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , model.teams
            |> Dict.values
            |> List.map
                (\team ->
                    H.li
                        []
                        [ H.text ("Time " ++ team.name ++ ": ")
                        , H.text (String.fromInt team.score)
                        ]
                )
            |> H.ul []
        , case model.turnState of
            Running seconds ->
                H.div
                    []
                    [ H.p [] [ H.text (String.fromInt seconds) ]
                    , viewTurnWords model
                    , H.button
                        [ HE.onClick MoreWords ]
                        [ H.text "More Words" ]
                    ]

            Start ->
                H.div
                    []
                    [ H.button
                        [ HE.onClick StartTurn ]
                        [ H.text "Start" ]
                    ]

            End ->
                H.div
                    []
                    [ viewTurnWords model
                    , H.button
                        [ HE.onClick StartTurn ]
                        [ H.text "Finish" ]
                    ]
        ]


viewTurnWords : Model -> H.Html Msg
viewTurnWords model =
    model.turnWords
        |> List.map
            (\word ->
                H.li
                    []
                    [ W.Button.view
                        [ if Set.member word model.turnWordsGuessed then
                            W.Button.primary

                          else
                            W.Button.noAttr
                        ]
                        { onClick = ToggleWord word
                        , label = [ H.text word ]
                        }
                    ]
            )
        |> H.ul []


main : Program (List String) Model Msg
main =
    Browser.element
        { init = \words -> ( init words, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

