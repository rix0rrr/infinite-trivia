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


type Failure
    = FetchError Http.Error
    | ExhaustedBatch
    | NoQuestionsInCategory


type Model
    = Loading
    | Nominal Game
    | FailureImminent Failure Game
    | OutOfCards Failure


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

        OutOfCards e ->
            Html.div [] (viewProblem e)

        FailureImminent _ game ->
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


viewProblem : Failure -> List (Html Message)
viewProblem error =
    [ Html.div [] [ Html.text "Problem loading questions:", Html.text (errorText error) ]
    , Html.div [ Attribute.class "controls" ]
        [ Html.button [ Attribute.class "btn-reload", Event.onClick Reload ] [ Html.text "Try again" ] ]
    ]


errorText : Failure -> String
errorText e =
    "OEPS"


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
    | Reload


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( model, message ) of
        ( _, Reload ) ->
            ( model, OpenTDB.getFreshQuestions GotQuestionBatch )

        (Nominal game, Skip) ->
            skip game
                |> Maybe.map Nominal
                |> Maybe.withDefault (OutOfCards ExhaustedBatch)
                |> addLoadingCommand

        (Nominal game, Ask) ->
            (Nominal <| ask game, Cmd.none)

        (Nominal game, Answer) ->
            (Nominal <| answer game, Cmd.none)

        (Nominal game, Next) ->
            next game
                |> Maybe.map Nominal
                |> Maybe.withDefault (OutOfCards ExhaustedBatch)
                |> addLoadingCommand

        (Nominal game, GotQuestionBatch (Ok response)) ->
            appendFutureQuestions response game
                |> Nominal
                |> addLoadingCommand

        (Nominal game, GotQuestionBatch (Err error)) ->
            game
                |> FailureImminent (FetchError error)
                |> addLoadingCommand

        (FailureImminent failure game, Skip) ->
            skip game
                |> Maybe.map Nominal
                |> Maybe.withDefault (OutOfCards failure)
                |> addLoadingCommand

        (FailureImminent failure game, Ask) ->
            (Nominal <| ask game, Cmd.none)

        (FailureImminent failure game, Answer) ->
            (Nominal <| answer game, Cmd.none)

        (FailureImminent failure game, Next) ->
            next game
                |> Maybe.map Nominal
                |> Maybe.withDefault (OutOfCards failure)
                |> addLoadingCommand

        (FailureImminent failure game, GotQuestionBatch (Ok response)) ->
            appendFutureQuestions response game
                |> FailureImminent failure
                |> addLoadingCommand

        (FailureImminent failure game, GotQuestionBatch (Err error)) ->
            game
                |> FailureImminent (FetchError error)
                |> addLoadingCommand

        ( _, GotQuestionBatch (Ok response) ) ->
            ( modelFromResponse response, Cmd.none )

        ( _, GotQuestionBatch (Err error) ) ->
            ( OutOfCards <| FetchError error, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


modelFromResponse : OpenTDB.Response -> Model
modelFromResponse response =
    let
        questions =
            questionsFromResponse response

        game =
            gameFromList questions

        model =
            game
                |> Maybe.map Nominal
                |> Maybe.withDefault (OutOfCards NoQuestionsInCategory)
    in
    model


gameFromModel : Model -> Maybe Game
gameFromModel model =
    case model of
        FailureImminent _ g ->
            Just g

        Nominal g ->
            Just g

        _ ->
            Nothing


addLoadingCommand : Model -> (Model, Cmd Message)
addLoadingCommand model =
    let
        command = gameFromModel model
            |> Maybe.map
                (\ng ->
                    if List.length ng.futureQuestions <= 2 then
                        OpenTDB.getFreshQuestions GotQuestionBatch

                    else
                        Cmd.none
                )
            |> Maybe.withDefault Cmd.none
    in (model, command)


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
