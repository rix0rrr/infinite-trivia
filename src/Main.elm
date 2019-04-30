module Main exposing (main)

import Browser
import Css exposing (..)
import Html as PlainHtml
import Html.Parser
import Html.Parser.Util
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import OpenTDB


main =
    Browser.element
        { init = init
        , view = view >> Html.toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Message )
init _ =
    let
        model =
            emptyModel
                |> updateStatus Loading
    in
    ( model, OpenTDB.getFreshQuestions GotQuestionBatch )


type alias Model =
    { game : Maybe Game
    , status : Status
    }


type Status
    = Idle
    | Loading
    | FetchError
    | Nominal


emptyModel : Model
emptyModel =
    { game = Nothing
    , status = Idle
    }


type alias Game =
    { seenQuestions : List Question
    , currentQuestion : Question
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


createModel : Status -> Maybe Game -> Model
createModel status game =
    { game = game, status = status }


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }


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
    { seenQuestions = []
    , currentQuestion = firstQuestion
    , futureQuestions = futureQuestions
    }


skip : Game -> Maybe Game
skip { seenQuestions, currentQuestion, futureQuestions } =
    case List.head futureQuestions of
        Just nextQuestion ->
            let
                nextFutureQuestions =
                    futureQuestions
                        |> List.tail
                        |> Maybe.withDefault []
            in
            Just
                { seenQuestions = currentQuestion :: seenQuestions
                , currentQuestion = nextQuestion
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
    Html.div
        [ Attribute.css
            [ backgroundImage (url "/static/interlaced.png")
            , backgroundRepeat repeat
            , position absolute
            , left (px 0)
            , top (px 0)
            , right (px 0)
            , bottom (px 0)
            ]
        ]
        [ -- Background div
          Html.div
            [ Attribute.css
                [ maxWidth (px 768)
                , margin2 (px 0) auto
                ]
            ]
            [ -- Main div (play area)
              viewModel model
            ]
        ]


viewModel : Model -> Html Message
viewModel model =
    case model.game of
        Just game ->
            viewQuestion game.currentQuestion

        Nothing ->
            viewStatus model.status

viewStatus : Status -> Html msg
viewStatus status =
    let
        text =
            case status of
                Idle -> 
                    "Idling..."

                Loading ->
                    "Loading..."

                FetchError ->
                    "Something went wrong. Try reloading the app."

                Nominal ->
                    "All is well"

    in
        Html.text text

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
        [ styleAsCard Back <| literalHtml question.category
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Skip ] [ Html.text "skip" ]
            , Html.button [ Event.onClick Ask ] [ Html.text "ask" ]
            ]
        ]


viewAsk : Question -> Html Message
viewAsk question =
    Html.div []
        [ styleAsCard Front
            [ Html.div [] <| literalHtml question.category
            , Html.div [] <| literalHtml question.question
            ]
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Skip ] [ Html.text "skip" ]
            , Html.button [ Event.onClick Answer ] [ Html.text "answer" ]
            ]
        ]


viewAnswer : Question -> Html Message
viewAnswer question =
    Html.div []
        [ styleAsCard Front
            [ Html.div [] <| literalHtml question.category
            , Html.div [] <| literalHtml question.question
            , Html.div [] <| literalHtml question.answer
            ]
        , Html.div [ Attribute.class "controls" ]
            [ Html.button [ Event.onClick Next ] [ Html.text "next" ]
            ]
        ]


literalHtml : String -> List (Html a)
literalHtml s =
    Html.Parser.run s
        |> Result.map Html.Parser.Util.toVirtualDom
        |> Result.map (List.map Html.fromUnstyled)
        |> Result.withDefault [ Html.text s ]


type CardFace
    = Front
    | Back


styleAsCard : CardFace -> List (Html a) -> Html a
styleAsCard face contents =
    let
        backgroundStyle =
            case face of
                Back ->
                    backgroundImage (url "/static/kale-salad.jpg")

                Front ->
                    backgroundColor (hex "ffffff")

        textColor =
            case face of
                Back ->
                    hex "ffffff"

                Front ->
                    hex "000000"
    in
    Html.div
        [ Attribute.css
            [ backgroundStyle
            , Css.boxShadow4 (px 0) (px 5) (px 8) (hex "000000")
            , color textColor
            , borderRadius (em 1)
            , padding (em 4)
            , margin (em 1)
            , displayFlex
            , flexDirection column
            , justifyContent center
            , alignItems center
            , height (px 300)
            , fontFamilies [ "georgia", "palatino" ]
            , fontSize (pt 20)
            ]
        ]
        contents



-- UPDATE


type Message
    = Skip
    | Ask
    | Answer
    | Next
    | GotQuestionBatch (Result Http.Error OpenTDB.Response)


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case model.game of
        Just game ->
            let
                nextGame =
                    case message of
                        Skip ->
                            skip game

                        Ask ->
                            Just <| ask game

                        Answer ->
                            Just <| answer game

                        Next ->
                            next game

                        GotQuestionBatch result ->
                            case result of
                                Ok response ->
                                    Just (appendFutureQuestions response game)

                                Err error ->
                                    Nothing

                nextStatus =
                    nextGame
                        |> Maybe.map (\_ -> Nominal)
                        |> Maybe.withDefault FetchError

                nextCommand =
                    nextGame
                        |> Maybe.andThen
                            (\ng ->
                                if List.length ng.futureQuestions <= 2 then
                                    Just <| OpenTDB.getFreshQuestions GotQuestionBatch

                                else
                                    Nothing
                            )
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | game = nextGame, status = nextStatus }, nextCommand )

        Nothing ->
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
                            ( createModel Nominal game, Cmd.none )

                        Err error ->
                            ( updateStatus FetchError model, Cmd.none )

                _ ->
                    ( emptyModel, Cmd.none )


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
