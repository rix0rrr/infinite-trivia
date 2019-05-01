module OpenTDB exposing (Question, Response, decode, getFreshQuestions, responseDecoder)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (..)
import Url.Builder exposing (crossOrigin, int)


type alias Response =
    { questions : List Question
    }


type alias Question =
    { category : String
    , questionType : Type
    , difficulty : Difficulty
    , question : String
    , correctAnswer : String
    , incorrectAnswers : List String
    }

type Type
    = MultipleChoice
    | Boolean
    | UnknownType String


type Difficulty
    = Easy
    | Medium
    | Hard
    | UnknownDifficulty


getFreshQuestions : (Result Http.Error Response -> a) -> Cmd a
getFreshQuestions m =
    let
        url =
            crossOrigin "https://opentdb.com" [ "api.php" ] [ int "amount" 10 ]
    in
    Http.get
        { url = url
        , expect = Http.expectJson m responseDecoder
        }


decode : String -> Result Decode.Error Response
decode input =
    Decode.decodeString responseDecoder input


responseDecoder : Decoder Response
responseDecoder =
    Decode.succeed Response
        |> required "results" (Decode.list questionDecoder)


questionDecoder : Decoder Question
questionDecoder =
    Decode.succeed Question
        |> required "category" Decode.string
        |> required "type" typeDecoder
        |> required "difficulty" difficultyDecoder
        |> required "question" Decode.string
        |> required "correct_answer" Decode.string
        |> required "incorrect_answers" (Decode.list Decode.string)


typeDecoder : Decoder Type
typeDecoder =
    let
        toType input =
            case input of
                "multiple" ->
                    MultipleChoice

                "boolean" ->
                    Boolean

                _ ->
                    UnknownType input
    in
    Decode.map toType Decode.string


difficultyDecoder : Decoder Difficulty
difficultyDecoder =
    let
        toDifficulty input =
            case input of
                "easy" ->
                    Easy

                "medium" ->
                    Medium

                "hard" ->
                    Hard

                _ ->
                    UnknownDifficulty
    in
    Decode.map toDifficulty Decode.string
