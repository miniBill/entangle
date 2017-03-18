module Quipper exposing (Config, State, Msg, Response, init, config, update, view)

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Debounce


type alias State =
    { code : String
    , debounceState : Debounce.State
    }


type Msg
    = Code String
    | Deb (Debounce.Msg Msg)
    | Transform String


type alias Response =
    { qpmc : String
    , nodes : String
    , tree : String
    }


type Config model msg
    = Config (model -> State) (model -> State -> model) (Msg -> msg) (Result Http.Error Response -> msg)


debounceCfg : Debounce.Config State Msg
debounceCfg =
    Debounce.config
        .debounceState
        (\model s -> { model | debounceState = s })
        Deb
        500


debCmd : Msg -> Cmd Msg
debCmd =
    Debounce.debounceCmd debounceCfg


config : (model -> State) -> (model -> State -> model) -> (Msg -> msg) -> (Result Http.Error Response -> msg) -> Config model msg
config getState updateState msg result =
    Config getState updateState msg result


update : Config model msg -> Msg -> model -> ( model, Cmd msg )
update (Config getter setter lift result) msg model =
    case msg of
        Code code ->
            let
                qmodel =
                    getter model

                cmd =
                    Cmd.map lift <| debCmd (Transform code)
            in
                ( setter model { qmodel | code = code }, cmd )

        Deb a ->
            let
                ( model_, cmd ) =
                    Debounce.update debounceCfg a model
            in
                ( model_, Cmd.map lift cmd )

        Transform quipperState ->
            ( model, Cmd.map result <| transformCmd quipperState )


transformCmd : String -> Cmd (Result Http.Error Response)
transformCmd quipper =
    let
        url =
            "http://localhost:3113"

        body =
            Http.stringBody "text/plain" quipper

        decoder =
            Decode.map3
                (\qpmc nodes tree ->
                    { qpmc = qpmc
                    , nodes = nodes
                    , tree = tree
                    }
                )
                (Decode.field "qpmc" Decode.string)
                (Decode.field "nodes" Decode.string)
                (Decode.field "tree" Decode.string)

        request =
            Http.post url body decoder
    in
        Http.send identity request


init : Config model msg -> ( State, Cmd msg )
init (Config _ _ _ result) =
    let
        quipperCode =
            String.join "\n"
                [ "\\q -> do"
                , " hadamard_at q"
                , " return q"
                ]

        state =
            { code = quipperCode
            , debounceState = Debounce.init
            }
    in
        ( state
        , Cmd.map result <| transformCmd quipperCode
        )


view : Config model msg -> model -> Html msg
view (Config getter _ lift _) model =
    Form.form []
        [ Form.group []
            [ Form.label
                [ for "code" ]
                [ text "Code" ]
            , Textarea.textarea
                [ Textarea.id "code"
                , Textarea.value (getter model).code
                , Textarea.onInput (lift << Code)
                , Textarea.rows 10
                , Textarea.attrs
                    [ style
                        [ ( "font-family", "Fira Code, monospace" )
                        ]
                    ]
                ]
            ]
        ]
