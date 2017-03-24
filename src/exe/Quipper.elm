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
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Debounce


type alias State =
    { functionName : String
    , input : Int
    , output : Output
    , code : String
    , kind : Kind
    , debounceState : Debounce.State
    }


type Kind
    = Symbolic
    | Numeric


type Output
    = Recursive
    | Qubits Int


type Msg
    = Code String
    | FunctionName String
    | Input Int
    | Output Output
    | Kind Kind
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

            FunctionName functionName ->
                trans { qmodel | functionName = functionName }

            Input input ->
                trans { qmodel | input = input }

            Output output ->
                trans { qmodel | output = output }

            Kind kind ->
                trans { qmodel | kind = kind }

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
            ("\\" ++ qtuple model.input ++ " -> (do; " ++ model.code ++ ")")
                |> Regex.replace Regex.All (Regex.regex "\n") (always "; ")

        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string model.functionName )
                    , ( "type", Encode.string <| signature model )
                    , ( "code", Encode.string code )
                    , ( "recursive", Encode.bool <| isRecursive model.output )
                    , ( "kind"
                      , Encode.string <|
                            case model.kind of
                                Symbolic ->
                                    "symbolic"

                                Numeric ->
                                    "numeric"
                      )
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
                [ "reset_at q"
                , "return q"
                ]

        state =
            { code = quipperCode
            , functionName = "resetCirc"
            , input = 1
            , output = Qubits 1
            , kind = Symbolic
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
            "q"

        _ ->
            let
                qs =
                    List.range 1 n |> List.map (\i -> "q" ++ toString i)
            in
                "(" ++ String.join ", " qs ++ ")"


qtupleType : Int -> String
qtupleType n =
    case n of
        1 ->
            "Qubit"

        _ ->
            "(" ++ String.join ", " (List.repeat n "Qubit") ++ ")"


signature : State -> String
signature model =
    let
        in_ =
            qtupleType model.input

        out =
            case model.output of
                Recursive ->
                    "RecAction"

                Qubits n ->
                    qtupleType n
    in
        in_ ++ " -> Circ " ++ out


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
            [ ( "Kind", kindRow )
            , ( "Body", bodyRow )
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
                            [ Col.xs12, Col.sm2, Col.md3, Col.lg3 ]
                            [ text name ]
                        , Form.col
                            [ Col.xs12, Col.sm10, Col.md9, Col.lg9 ]
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


bodyRow : State -> Html Msg
bodyRow model =
    Textarea.textarea
        [ Textarea.value model.code
        , Textarea.onInput Code
        , Textarea.rows 10
        , Textarea.attrs [ monospaced ]
        ]


codeRow : State -> Html Msg
codeRow model =
    Textarea.textarea
        [ Textarea.value <| code model
        , Textarea.rows 10
        , Textarea.attrs [ monospaced ]
        , Textarea.disabled
        ]


kindRow : State -> Html Msg
kindRow model =
    let
        kinds =
            [ Symbolic, Numeric ]

        kindToRadio kind =
            Radio.create
                [ Radio.onClick (Kind kind)
                , Radio.checked <| model.kind == kind
                , Radio.inline
                ]
                (toString kind)
    in
        span [] <|
            Radio.radioList "kind" <|
                List.map kindToRadio kinds


code : State -> String
code model =
    String.join "\n"
        [ model.functionName ++ " :: " ++ signature model
        , model.functionName ++ " " ++ qtuple model.input ++ " = do"
        , ("  " ++ model.code)
            |> Regex.replace Regex.All (Regex.regex "\n") (always "\n  ")
        ]
