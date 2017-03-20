module Quipper exposing (Config, State, Msg, Response, init, config, code, update, view)

import Json.Decode as Decode
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Regex
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Debounce


type alias State =
    { functionName : String
    , input : Int
    , output : Output
    , code : String
    , debounceState : Debounce.State
    }


type Output
    = Recursive
    | Qubits Int


type Msg
    = Code String
    | FunctionName String
    | Input Int
    | Output Output
    | Deb (Debounce.Msg Msg)
    | Transform


type alias Response =
    { qpmc : String
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
                    Cmd.map lift <| debCmd Transform
            in
                ( setter model qmodel_, cmd )
    in
        case msg of
            Code code ->
                trans { qmodel | code = code }

            FunctionName _ ->
                trans qmodel

            Input input ->
                trans { qmodel | input = input }

            Output output ->
                trans { qmodel | output = output }

            Deb a ->
                let
                    ( qmodel_, cmd ) =
                        Debounce.update debounceCfg a qmodel
                in
                    ( setter model qmodel_, Cmd.map lift cmd )

            Transform ->
                ( model, Cmd.map result <| transformCmd qmodel )


transformCmd : State -> Cmd (Result Http.Error Response)
transformCmd model =
    let
        url =
            "http://localhost:3113"

        code =
            ("\\q -> (do; " ++ model.code ++ ")")
                |> Regex.replace Regex.All (Regex.regex "\n") (always "; ")

        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "code", Encode.string code )
                    , ( "recursive", Encode.bool <| isRecursive model.output )
                    ]

        decoder =
            Decode.map2
                (\qpmc tree ->
                    { qpmc = qpmc
                    , tree = tree
                    }
                )
                (Decode.field "qpmc" Decode.string)
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
            , input = 1
            , output = Qubits 1
            , debounceState = Debounce.init
            }
    in
        ( state
        , Cmd.map result <| transformCmd state
        )


monospaced : Attribute msg
monospaced =
    style
        [ ( "font-family", "Fira Code, monospace" )
        , ( "white-space", "pre-wrap" )
        ]


qtuple : Int -> String
qtuple n =
    case n of
        1 ->
            "Qubit"

        _ ->
            "(" ++ String.join ", " (List.repeat n "Qubit") ++ ")"


signature : State -> String
signature model =
    let
        in_ =
            qtuple model.input

        out =
            case model.output of
                Recursive ->
                    "RecAction"

                Qubits n ->
                    qtuple n

        type_ =
            in_ ++ " -> Circ " ++ out
    in
        model.functionName ++ " :: " ++ type_


view : Config model msg -> model -> Html msg
view (Config getter _ lift _) model =
    let
        rowsHead =
            [ ( "Name", nameRow )
            , ( "Input qubits", inputRow )
            , ( "Recursive", recursiveRow )
            ]

        rowsMid =
            case (getter model).output of
                Recursive ->
                    []

                Qubits _ ->
                    [ ( "Output qubits", outputRow ) ]

        rowsTail =
            [ ( "Body", bodyRow )
            , ( "Code", codeRow )
            ]

        rows =
            rowsHead ++ rowsMid ++ rowsTail
    in
        Form.form [] <|
            List.map
                (\( name, content ) ->
                    Form.row [ Row.rightSm ]
                        [ Form.colLabel
                            [ Col.xs12, Col.sm2, Col.md3, Col.lg2 ]
                            [ text name ]
                        , Form.col
                            [ Col.xs12, Col.sm10, Col.md9, Col.lg10 ]
                            [ Html.map lift <| content <| getter model ]
                        ]
                )
                rows


nameRow : State -> Html Msg
nameRow model =
    Input.text
        [ Input.value model.functionName
        , Input.onInput FunctionName
        ]


bodyRow : State -> Html Msg
bodyRow model =
    Textarea.textarea
        [ Textarea.value model.code
        , Textarea.onInput Code
        , Textarea.rows 10
        , Textarea.attrs [ monospaced ]
        ]


inputRow : State -> Html Msg
inputRow model =
    Input.number
        [ Input.onInput (Input << Result.withDefault model.input << String.toInt)
        , Input.value <| toString model.input
        ]


isRecursive : Output -> Bool
isRecursive output =
    case output of
        Recursive ->
            True

        Qubits _ ->
            False


recursiveRow : State -> Html Msg
recursiveRow model =
    Checkbox.checkbox
        [ Checkbox.checked <|
            isRecursive model.output
        , Checkbox.onCheck
            (\c ->
                Output <|
                    if c then
                        Recursive
                    else
                        Qubits 0
            )
        ]
        "Recursive"


outputRow : State -> Html Msg
outputRow model =
    let
        int =
            case model.output of
                Recursive ->
                    0

                Qubits n ->
                    n
    in
        Input.number
            [ Input.onInput (Output << Qubits << Result.withDefault int << String.toInt)
            , Input.value <|
                toString int
            ]


codeRow : State -> Html Msg
codeRow model =
    span [ monospaced ] [ text <| code model ]


code : State -> String
code model =
    String.join "\n"
        [ signature model
        , model.functionName ++ " q = do"
        , ("  " ++ model.code)
            |> Regex.replace Regex.All (Regex.regex "\n") (always "\n  ")
        ]
