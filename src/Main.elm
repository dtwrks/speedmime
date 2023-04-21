module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Dict exposing (Dict)
import Set exposing (Set)
import Time


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
    | MoreWords


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
                , turnInSeconds = 0
                , turnWords = []
                , turnWordsGuessed = Set.empty
                , wordsArchive = model.wordsArchive
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

        TurnTick ->
            { model | turnInSeconds = model.turnInSeconds + 1 }
        
        
        Word word ->
            { model | turnWordsGuessed = Set.insert word model.turnWordsGuessed }

        MoreWords ->
            { model
                | turnWords = []
                , wordsArchive = []
            }


subscriptions : Model -> Sub Msg 
subscriptions model =
    if model.turnStarted then
        Time.every 1000 (\_ -> TurnTick)

    else
        Sub.none
        

view : Model -> Html Msg
view model =
    if model.turnStarted then
        H.div
            []
            [ H.p [] [ H.text (String.fromInt model.turnInSeconds)
            , model.turnWords
                |> List.map (\word ->
                    H.li
                        []
                        [ H.button
                          [ HE.onClick (Word word) 
                          , if Set.member word model.turnWordsGuessed then
                                HA.disabled
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


main : Program (List String) Model Msg
main =
    Browser.element
        { init = \words -> ( init words, Cmd.none )
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }
