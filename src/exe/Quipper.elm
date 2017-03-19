module Quipper exposing (Config, State, Msg, Response, init, config, update, view)

import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Regex
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Form.Input as Input
import Debounce


type alias State =
    { functionName : String
    , inputArity : Int
    , output : OutputArity
    , code : String
    , debounceState : Debounce.State
    }


type OutputArity
    = OutputRecursive
    | OutputArity Int


type Msg
    = Code String
    | FunctionName String
    | Input Int
    | Output OutputArity
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
    let
        qmodel =
            getter model

        trans qmodel_ =
            let
                cmd =
                    Cmd.map lift <| debCmd (Transform qmodel_.code)
            in
                ( setter model qmodel_, cmd )
    in
        case msg of
            Code code ->
                trans ({ qmodel | code = code })

            FunctionName _ ->
                trans qmodel

            Input _ ->
                trans qmodel

            Output _ ->
                trans qmodel

            Deb a ->
                let
                    ( qmodel_, cmd ) =
                        Debounce.update debounceCfg a qmodel
                in
                    ( setter model qmodel_, Cmd.map lift cmd )

            Transform quipperState ->
                ( model, Cmd.map result <| transformCmd quipperState )


transformCmd : String -> Cmd (Result Http.Error Response)
transformCmd quipper =
    let
        url =
            "http://localhost:3113"

        code =
            ("\\q -> (do; " ++ quipper ++ ")")
                |> Regex.replace Regex.All (Regex.regex "\n") (always "; ")

        body =
            Http.stringBody "text/plain" code

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
                [ "hadamard_at q"
                , "return q"
                ]

        state =
            { code = quipperCode
            , functionName = "f"
            , inputArity = 1
            , output = OutputArity 1
            , debounceState = Debounce.init
            }
    in
        ( state
        , Cmd.map result <| transformCmd quipperCode
        )


monospaced : Attribute msg
monospaced =
    style
        [ ( "font-family", "Fira Code, monospace" )
        , ( "white-space", " pre" )
        ]


tt : List String -> Html msg
tt values =
    span [ monospaced ] <|
        List.intersperse (br [] []) <|
            List.map text values


view : Config model msg -> model -> Html msg
view (Config getter _ lift _) model =
    let
        qmodel =
            getter model

        qtuple n =
            case n of
                1 ->
                    "Qubit"

                _ ->
                    "(" ++ String.join " , " (List.repeat n "Qubit") ++ ")"
    in
        Form.form []
            [ Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Name" ]
                , Form.col [ Col.sm10 ]
                    [ Input.text
                        [ Input.value qmodel.functionName
                        , Input.onInput (lift << FunctionName)
                        ]
                    ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Body" ]
                , Form.col [ Col.sm10 ]
                    [ Textarea.textarea
                        [ Textarea.value qmodel.code
                        , Textarea.onInput (lift << Code)
                        , Textarea.rows 10
                        , Textarea.attrs [ monospaced ]
                        ]
                    ]
                ]
            , Form.row []
                [ Form.colLabel [ Col.sm2 ] [ text "Code" ]
                , Form.colLabel [ Col.sm10 ]
                    [ tt
                        [ let
                            out =
                                case qmodel.output of
                                    OutputRecursive ->
                                        "RecAction"

                                    OutputArity n ->
                                        qtuple n

                            type_ =
                                qtuple qmodel.inputArity ++ " -> " ++ out
                          in
                            qmodel.functionName ++ " :: " ++ type_
                        , qmodel.functionName ++ " q = do"
                        , ("  " ++ qmodel.code)
                            |> Regex.replace Regex.All (Regex.regex "\n") (always "\n  ")
                        ]
                    ]
                ]
            ]
