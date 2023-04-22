module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)
import Time
import W.Button
import W.Container
import W.Heading
import W.Styles
import W.Text


wordsPerTurn : Int
wordsPerTurn =
    5


turnDuration : Int
turnDuration =
    3


maxScore : Int
maxScore =
    2


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
    | GameOver
    | Running Int


initialTeams : Dict Int Team
initialTeams =
    [ { name = "Claro", score = 0, turns = 0 }
    , { name = "Escuro", score = 0, turns = 0 }
    ]
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


init : List String -> Model
init words =
    { teams = initialTeams
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
                | turnState = Running 0
                , turnWordsGuessed = Set.empty
                , turnWords = List.take wordsPerTurn model.wordsArchive
                , wordsArchive = List.drop wordsPerTurn model.wordsArchive
            }

        EndTurn ->
            let
                teams : Dict Int Team
                teams =
                    Dict.update
                        model.teamTurn
                        (\team ->
                            team
                                |> Maybe.map
                                    (\t ->
                                        { t
                                            | turns = t.turns + 1
                                            , score =
                                                t.score + Set.size model.turnWordsGuessed
                                        }
                                    )
                        )
                        model.teams
                
                teamsMaxScore : Int
                teamsMaxScore =
                    teams
                    |> Dict.values
                    |> List.foldl (\{ score } acc ->
                       max acc score
                    ) 0

                teamsTurnsMatch : Bool
                teamsTurnsMatch =
                     teams
                     |> Dict.values
                     |> List.map .turns
                     |> Set.fromList
                     |> Set.size
                     |> (==) 1
            in
            { model
                | turnState =
                    if teamsMaxScore >= maxScore && teamsTurnsMatch then
                        GameOver

                    else
                        Start
                , teamTurn = modBy (Dict.size model.teams) (model.teamTurn + 1)
                , teams = teams
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
        , case model.turnState of
            Running seconds ->
                W.Heading.view
                    []
                    [ H.text (String.fromInt seconds)
                    ]
                    |> List.singleton
                    |> W.Container.view [ W.Container.padBottom_4, W.Container.horizontal, W.Container.gap_2, W.Container.alignCenterX, W.Container.alignCenterY ]

            End ->
                W.Container.view [ W.Container.padBottom_4 ]
                    [ W.Button.viewDummy [ W.Button.disabled True, W.Button.danger ]
                        [ H.text "Seu tempo acabou!" ]
                    ]

            Start ->
                viewScoreBoard model
            
            GameOver ->
                W.Container.view [ W.Container.gap_4 ]
                    [ W.Button.viewDummy [ W.Button.disabled True, W.Button.warning ]
                        [ H.text "O jogo acabou!" ]
                    , viewScoreBoard model
                    ]
        , case model.turnState of
            Running _ ->
                H.div
                    []
                    [ viewTurnWords model
                    , W.Container.view [ W.Container.padTop_4 ]
                        [ W.Button.view [ W.Button.invisible ]
                            { onClick = MoreWords
                            , label = [ H.text "Mais Palavras" ]
                            }
                        ]
                    ]

            Start ->
                W.Container.view []
                    [ W.Button.view []
                        { onClick = StartTurn
                        , label = [ H.text "Iniciar" ]
                        }
                    ]

            End ->
                H.div
                    []
                    [ viewTurnWords model
                    , W.Container.view [ W.Container.padTop_4 ]
                        [ W.Button.view []
                            { onClick = EndTurn
                            , label = [ H.text "Finalizar" ]
                            }
                        ]
                    ]
            GameOver ->
               H.text ""
        ]


viewScoreBoard : Model -> H.Html Msg
viewScoreBoard model =
    model.teams
        |> Dict.values
        |> List.map
            (\team ->
                W.Heading.view
                    []
                    [ H.text (String.fromInt team.score)
                    ]
            )
        |> List.intersperse (W.Text.view [] [ H.text "x" ])
        |> W.Container.view [ W.Container.padBottom_4, W.Container.horizontal, W.Container.gap_2, W.Container.alignCenterX, W.Container.alignCenterY ]

viewTurnWords : Model -> H.Html Msg
viewTurnWords model =
    model.turnWords
        |> List.map
            (\word ->
                W.Button.view
                    [ if Set.member word model.turnWordsGuessed then
                        W.Button.success

                      else
                        W.Button.outlined
                    ]
                    { onClick = ToggleWord word
                    , label = [ H.text word ]
                    }
            )
        |> W.Container.view [ W.Container.gap_2 ]


main : Program (List String) Model Msg
main =
    Browser.element
        { init = \words -> ( init words, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

