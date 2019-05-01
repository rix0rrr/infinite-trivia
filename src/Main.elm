module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OpenTDB


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Message )
init _ =
    ( Loading, OpenTDB.getFreshQuestions GotQuestionBatch )


type Model
    = Loading
    | Nominal Game
    | InsufficientQuestions
    | InitialFetchError Http.Error
    | StarvedForQuestions Game
    | ProgressiveFetchError Http.Error Game


type alias Game =
    { currentQuestion : Question
    , futureQuestions : List Question
    }


type alias Question =
    { category : String
    , question : String
    , answer : String
    , stage : QuestionStage
    }


type QuestionStage
    = Category
    | Asking
    | Answering


gameFromList : List Question -> Maybe Game
gameFromList questions =
    let
        createGameWith =
            questions
                |> List.tail
                |> Maybe.withDefault []
                |> createGame
    in
    questions
        |> List.head
        |> Maybe.map createGameWith


createGame : List Question -> Question -> Game
createGame futureQuestions firstQuestion =
    { currentQuestion = firstQuestion
    , futureQuestions = futureQuestions
    }


skip : Game -> Maybe Game
skip { currentQuestion, futureQuestions } =
    case List.head futureQuestions of
        Just nextQuestion ->
            let
                nextFutureQuestions =
                    futureQuestions
                        |> List.tail
                        |> Maybe.withDefault []
            in
            Just
                { currentQuestion = nextQuestion
                , futureQuestions = nextFutureQuestions
                }

        Nothing ->
            Nothing


next : Game -> Maybe Game
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


view : Model -> Html Message
view model =
    Html.div [ Attribute.class "main" ] [ viewModel model ]


viewModel : Model -> Html Message
viewModel model =
    case model of
        Loading ->
            Html.text "Loading..."

        Nominal game ->
            viewQuestion game.currentQuestion

        InitialFetchError _ ->
            Html.text "Could not load questions. Reload the app."

        ProgressiveFetchError _ game ->
            -- TODO: show problem with loading more questions
            viewQuestion game.currentQuestion

        InsufficientQuestions ->
            Html.text "Could not load enough questions. Reload the app."

        StarvedForQuestions game ->
            -- TODO: show problem with loading more questions
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
    Html.div [] content


viewCategory : Question -> List (Html Message)
viewCategory question =
    [ viewCard question Category
    , Html.div [ Attribute.class "controls" ]
        [ Html.button [ Attribute.class "btn-skip", Event.onClick Skip ] [ Html.text "skip" ]
        , Html.button [ Attribute.class "btn-ask", Event.onClick Ask ] [ Html.text "ask" ]
        ]
    ]


viewCard : Question -> QuestionStage -> Html Message
viewCard question stage =
    flippableCard (stage /= Category) (viewCardFront question stage) (viewCardBack question)


viewCardBack : Question -> List (Html Message)
viewCardBack question =
    literalHtml question.category


viewCardFront : Question -> QuestionStage -> List (Html Message)
viewCardFront question stage =
    List.concat
        [ if stage == Asking || stage == Answering then
            [ Html.div [ Attribute.class "txt-category" ] <| literalHtml question.category ]

          else
            []
        , if stage == Asking || stage == Answering then
            [ Html.div [ Attribute.class "txt-question" ] <| literalHtml question.question ]

          else
            []
        , if stage == Answering then
            [ Html.div [ Attribute.class "txt-answer" ] <| literalHtml question.answer ]

          else
            []
        ]


viewAsk : Question -> List (Html Message)
viewAsk question =
    [ viewCard question Asking
    , Html.div [ Attribute.class "controls" ]
        [ Html.button [ Attribute.class "btn-skip", Event.onClick Skip ] [ Html.text "skip" ]
        , Html.button [ Attribute.class "btn-answer", Event.onClick Answer ] [ Html.text "answer" ]
        ]
    ]


viewAnswer : Question -> List (Html Message)
viewAnswer question =
    [ viewCard question Answering
    , Html.div [ Attribute.class "controls" ]
        [ Html.button [ Attribute.class "btn-next", Event.onClick Next ] [ Html.text "next" ]
        ]
    ]


literalHtml : String -> List (Html a)
literalHtml s =
    Html.Parser.run s
        |> Result.map Html.Parser.Util.toVirtualDom
        |> Result.withDefault [ Html.text s ]


type CardFace
    = Front
    | Back


styleAsCard : CardFace -> List (Html a) -> Html a
styleAsCard face contents =
    Html.div
        [ Attribute.class <|
            case face of
                Back ->
                    "card-back"

                Front ->
                    "card-front"
        ]
        contents


flippableCard : Bool -> List (Html a) -> List (Html a) -> Html a
flippableCard flipped front back =
    Html.div
        [ Attribute.classList [ ( "flippable-card", True ), ( "flipped", flipped ) ] ]
        [ styleAsCard Front front, styleAsCard Back back ]



-- UPDATE


type Message
    = Skip
    | Ask
    | Answer
    | Next
    | GotQuestionBatch (Result Http.Error OpenTDB.Response)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case model of
        Loading ->
            updateWithoutAGame message model

        Nominal game ->
            updateWithAGame game message model

        InsufficientQuestions ->
            updateWithoutAGame message model

        InitialFetchError _ ->
            updateWithoutAGame message model

        ProgressiveFetchError _ game ->
            updateWithAGame game message model

        StarvedForQuestions game ->
            updateWithAGame game message model


updateWithoutAGame : Message -> Model -> ( Model, Cmd Message )
updateWithoutAGame message model =
    let
        nextModel =
            case message of
                GotQuestionBatch result ->
                    case result of
                        Ok response ->
                            let
                                questions =
                                    questionsFromResponse response

                                game =
                                    gameFromList questions
                            in
                            game
                                |> Maybe.map Nominal
                                |> Maybe.withDefault InsufficientQuestions

                        Err error ->
                            InitialFetchError error

                _ ->
                    model
    in
    ( nextModel, Cmd.none )


updateWithAGame : Game -> Message -> Model -> ( Model, Cmd Message )
updateWithAGame game message model =
    let
        nextModel =
            case message of
                Skip ->
                    skip game
                        |> Maybe.map Nominal
                        |> Maybe.withDefault (StarvedForQuestions game)

                Ask ->
                    Nominal <| ask game

                Answer ->
                    Nominal <| answer game

                Next ->
                    next game
                        |> Maybe.map Nominal
                        |> Maybe.withDefault (StarvedForQuestions game)

                GotQuestionBatch result ->
                    case result of
                        Ok response ->
                            Nominal <| appendFutureQuestions response game

                        Err error ->
                            ProgressiveFetchError error game

        nextCommand =
            case nextModel of
                -- TODO: are these all when we need to fetch next questions
                StarvedForQuestions _ ->
                    OpenTDB.getFreshQuestions GotQuestionBatch

                Nominal ng ->
                    if List.length ng.futureQuestions <= 2 then
                        OpenTDB.getFreshQuestions GotQuestionBatch

                    else
                        Cmd.none

                _ ->
                    Cmd.none
    in
    ( nextModel, nextCommand )


questionsFromResponse : OpenTDB.Response -> List Question
questionsFromResponse { questions } =
    List.map toQuestion questions


appendFutureQuestions : OpenTDB.Response -> Game -> Game
appendFutureQuestions response game =
    let
        nextFutureQuestions =
            List.append game.futureQuestions (questionsFromResponse response)
    in
    { game | futureQuestions = nextFutureQuestions }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
