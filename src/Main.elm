module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event

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
            createGame defaultQuestion
    in
    ( game, Cmd.none )


defaultQuestion : Question
defaultQuestion =
    { category = "science"
    , question = "how many planets does our solar system have"
    , answer = "8"
    , stage = Category
    }


type QuestionStage
    = Category
    | Ask
    | Answer


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
            { currentQuestion | stage = Ask }
    in
    { game | currentQuestion = nextCurrentQuestion }


answer : Game -> Game
answer game =
    let
        currentQuestion =
            game.currentQuestion

        nextCurrentQuestion =
            { currentQuestion | stage = Answer }
    in
    { game | currentQuestion = nextCurrentQuestion }



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

                Ask ->
                    viewAsk question

                Answer ->
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
            [ Html.button [ Event.onClick DoSkip ] [ Html.text "skip" ]
            , Html.button [ Event.onClick DoAsk ] [ Html.text "ask" ]
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
            [ Html.button [ Event.onClick DoSkip] [ Html.text "skip" ]
            , Html.button [ Event.onClick DoAnswer] [ Html.text "answer" ]
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
            [ Html.button [ Event.onClick DoNext ] [ Html.text "next" ]
            ]
        ]



-- UPDATE


type Message
    = DoSkip
    | DoAsk
    | DoAnswer
    | DoNext


update : Message -> Game -> ( Game, Cmd Message )
update message game =
    let
        nextGame =
            case message of
                DoSkip ->
                    skip game

                DoAsk ->
                    ask game

                DoAnswer ->
                    answer game

                DoNext ->
                    next game
    in
    ( nextGame, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Game -> Sub msg
subscriptions model =
    Sub.none
