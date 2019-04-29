module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import OpenTDB


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Game, Cmd Message )
init _ =
    let
        game =
            createGame defaultQuestion
    in
    ( game, getFreshQuestions )


getFreshQuestions : Cmd Message
getFreshQuestions =
    Http.get
        { url = "https://opentdb.com/api.php?amount=10"
        , expect = Http.expectJson GotQuestionBatch OpenTDB.responseDecoder
        }


defaultQuestion : Question
defaultQuestion =
    { category = "science"
    , question = "how many planets does our solar system have"
    , answer = "8"
    , stage = Category
    }


type QuestionStage
    = Category
    | Asking
    | Answering


type alias Question =
    { category : String
    , question : String
    , answer : String
    , stage : QuestionStage
    }


type alias Game =
    { seenQuestions : List Question
    , currentQuestion : Question
    , futureQuestions : List Question
    }


createGame : Question -> Game
createGame question =
    { seenQuestions = []
    , currentQuestion = question
    , futureQuestions = []
    }


skip : Game -> Game
skip { seenQuestions, currentQuestion, futureQuestions } =
    let
        nextQuestion =
            List.head futureQuestions
                |> Maybe.withDefault defaultQuestion

        nextFutureQuestions =
            futureQuestions
                |> List.tail
                |> Maybe.withDefault []
    in
    { seenQuestions = currentQuestion :: seenQuestions
    , currentQuestion = nextQuestion
    , futureQuestions = nextFutureQuestions
    }


next : Game -> Game
next =
    skip


ask : Game -> Game
ask game =
    let
        currentQuestion =
            game.currentQuestion

        nextCurrentQuestion =
            { currentQuestion | stage = Asking }
    in
    { game | currentQuestion = nextCurrentQuestion }


answer : Game -> Game
answer game =
    let
        currentQuestion =
            game.currentQuestion

        nextCurrentQuestion =
            { currentQuestion | stage = Answering }
    in
    { game | currentQuestion = nextCurrentQuestion }


toQuestion : OpenTDB.Question -> Question
toQuestion { category, question, correctAnswer } =
    { category = category
    , question = question
    , answer = correctAnswer
    , stage = Category
    }



-- VIEW


view : Game -> Html Message
view game =
    viewQuestion game.currentQuestion


viewQuestion : Question -> Html Message
viewQuestion question =
    let
        content =
            case question.stage of
                Category ->
                    viewCategory question

                Asking ->
                    viewAsk question

                Answering ->
                    viewAnswer question
    in
    Html.div []
        [ content
        ]


viewCategory : Question -> Html Message
viewCategory question =
    Html.div []
        [ Html.div []
            [ Html.text question.category
            ]
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Skip ] [ Html.text "skip" ]
            , Html.button [ Event.onClick Ask ] [ Html.text "ask" ]
            ]
        ]


viewAsk : Question -> Html Message
viewAsk question =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text question.category ]
            , Html.div [] [ Html.text question.question ]
            ]
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Skip ] [ Html.text "skip" ]
            , Html.button [ Event.onClick Answer ] [ Html.text "answer" ]
            ]
        ]


viewAnswer : Question -> Html Message
viewAnswer question =
    Html.div []
        [ Html.div []
            [ Html.div [] [ Html.text question.category ]
            , Html.div [] [ Html.text question.question ]
            , Html.div [] [ Html.text question.answer ]
            ]
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Next ] [ Html.text "next" ]
            ]
        ]



-- UPDATE


type Message
    = Skip
    | Ask
    | Answer
    | Next
    | GotQuestionBatch (Result Http.Error OpenTDB.Response)


update : Message -> Game -> ( Game, Cmd Message )
update message game =
    let
        nextGame =
            case message of
                Skip ->
                    skip game

                Ask ->
                    ask game

                Answer ->
                    answer game

                Next ->
                    next game

                GotQuestionBatch result ->
                    case result of
                        Ok response ->
                            appendFutureQuestions response game

                        Err error ->
                            game

        -- TODO: Handle error
    in
    ( nextGame, Cmd.none )


appendFutureQuestions : OpenTDB.Response -> Game -> Game
appendFutureQuestions { questions } game =
    let
        freshQuestions =
            List.map toQuestion questions

        nextFutureQuestions =
            List.append game.futureQuestions freshQuestions
    in
    { game | futureQuestions = nextFutureQuestions }



-- SUBSCRIPTIONS


subscriptions : Game -> Sub msg
subscriptions model =
    Sub.none
