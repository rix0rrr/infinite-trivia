module Main exposing (Game, Question, State(..), createGame, view, viewQuestion)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Game, Cmd msg )
init _ =
    let
        game =
            createGame { category = "science", question = "how many planets does our solar system have", answer = "8" }
    in
    ( game, Cmd.none )


type State
    = Category Question
    | Ask Question
    | Answer Question


type alias Question =
    { category : String
    , question : String
    , answer : String
    }


type alias Game =
    { seenQuestions : List Question
    , currentStage : State
    , futureQuestions : List Question
    }


createGame : Question -> Game
createGame question =
    { seenQuestions = []
    , currentStage = Category question
    , futureQuestions = []
    }



-- VIEW


view : Game -> Html msg
view game =
    viewQuestion game.currentStage


viewQuestion : State -> Html msg
viewQuestion state =
    let
        content =
            case state of
                Category question ->
                    viewCategory question

                Ask question ->
                    viewAsk question

                Answer question ->
                    viewAnswer question
    in
    Html.div []
        [ content
        ]


viewCategory : Question -> Html msg
viewCategory question =
    Html.text question.category


viewAsk : Question -> Html msg
viewAsk question =
    Html.text question.question


viewAnswer : Question -> Html msg
viewAnswer question =
    Html.text question.answer



-- UPDATE


type Message
    = DoNothing


update : Message -> Game -> ( Game, Cmd Message )
update _ model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Game -> Sub msg
subscriptions model =
    Sub.none
