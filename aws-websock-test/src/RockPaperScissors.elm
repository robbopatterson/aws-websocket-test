module RockPaperScissors exposing (main)

import Browser
import Delay
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias MainModel =
    { byline : String
    , imageUrl : String
    , title : String
    , username : String
    , usernameLogo : String
    }


type alias ItemModel =
    { byline : String
    , imageUrl : String
    , title : String
    , href: String
    }


type alias NavButtonModel =
    { name : String
    , url : String
    }


type alias Model =
    { main : Maybe MainModel
    , items : List ItemModel
    , navButtons : List NavButtonModel
    }


view : Model -> Html msg
view model =
    case model.main of
        Nothing ->
            div [] [ text "Loading" ]

        Just mainProfile ->
            div [] [text "Rock Paper Scissors"]

initialModel : Model
initialModel =
    { main = Nothing
    , items = []
    , navButtons = []
    }


type Msg
    = SendHttpRequest
    | MainReceived (Result Http.Error MainModel)
    | ItemsReceived (Result Http.Error (List ItemModel))
    | NavButtonsReceived (Result Http.Error (List NavButtonModel))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getMain )

        MainReceived result ->
            case result of
                Ok mainModel ->
                    ( { model | main = Just mainModel }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        ItemsReceived result ->
            case result of
                Ok items ->
                    ( { model | items = items }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )

        NavButtonsReceived result ->
            case result of
                Ok navButtons ->
                    ( { model | navButtons = navButtons }, Cmd.none )

                Err httpError ->
                    ( model, Cmd.none )


mainDecoder : Decoder MainModel
mainDecoder =
    Decode.succeed MainModel
        |> Json.Decode.Pipeline.required "byline" string
        |> Json.Decode.Pipeline.required "imageUrl" string
        |> Json.Decode.Pipeline.required "title" string
        |> Json.Decode.Pipeline.required "username" string
        |> Json.Decode.Pipeline.required "username-logo" string


itemDecoder : Decoder ItemModel
itemDecoder =
    Decode.succeed ItemModel
        |> Json.Decode.Pipeline.required "byline" string
        |> Json.Decode.Pipeline.required "imageUrl" string
        |> Json.Decode.Pipeline.required "title" string
        |> Json.Decode.Pipeline.required "href" string


navButtonDecoder : Decoder NavButtonModel
navButtonDecoder =
    Decode.succeed NavButtonModel
        |> Json.Decode.Pipeline.required "name" string
        |> Json.Decode.Pipeline.required "url" string


host =
    "https://portfolio-api.robdev.ca"


getMain : Cmd Msg
getMain =
    Http.get
        { url = host ++ "/portfolio/main"
        , expect = Http.expectJson MainReceived mainDecoder
        }


getItems : Cmd Msg
getItems =
    Http.get
        { url = host ++ "/portfolio-item"
        , expect = Http.expectJson ItemsReceived (Decode.list itemDecoder)
        }


getNavButtons : Cmd Msg
getNavButtons =
    Http.get
        { url = host ++ "/nav-button"
        , expect = Http.expectJson NavButtonsReceived (Decode.list navButtonDecoder)
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.batch [ getMain, getItems, getNavButtons ] )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
