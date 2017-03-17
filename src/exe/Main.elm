module Main exposing (main)

import Json.Encode as Encode
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Base64
import Debounce
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row


main : Program Never Model Msg
main =
    program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


type alias Model =
    { quipper : String
    , qpmc : String
    , nodes : String
    , tree : String
    , debounceState : Debounce.State
    }


type alias Response =
    { qpmc : String
    , nodes : String
    , tree : String
    }


type Msg
    = Quipper String
    | Deb (Debounce.Msg Msg)
    | Transform String
    | TransformResult (Result Http.Error Response)


init : ( Model, Cmd Msg )
init =
    let
        quipper =
            String.join "\n"
                [ "(\\q -> hadamard q)"
                ]
    in
        ( { quipper = quipper
          , qpmc = ""
          , nodes = ""
          , tree = ""
          , debounceState = Debounce.init
          }
        , transformCmd quipper
        )


view : Model -> Html Msg
view model =
    let
        cardList =
            [ [ ( "Quipper", quipperView, "Main.hs", .quipper )
              , ( "QPMC", otherView, "output.qpmc", .qpmc )
              ]
            , [ ( "Nodes", otherView, "nodes.log", .nodes )
              , ( "Tree", otherView, "tree.log", .tree )
              ]
            ]

        rows =
            List.map viewRow cardList

        viewRow cards =
            Grid.row [ Row.attrs [ class "pt-4" ] ] <|
                List.map viewCard cards

        viewCard ( name, subview, filename, property ) =
            Grid.col [ Col.sm6 ]
                [ Card.config []
                    |> Card.headerH3
                        [ class "text-center"
                        , class "mt-2"
                        ]
                        [ text name ]
                    |> Card.block []
                        [ Card.text [] [ subview (property model) ] ]
                    |> Card.footer []
                        [ downloadLink filename property ]
                    |> Card.view
                ]

        downloadLink filename property =
            Button.linkButton
                [ Button.primary
                , Button.attrs
                    [ let
                        url =
                            head ++ encoded

                        head =
                            "data:application/force-download;charset=UTF-8;base64,"

                        encoded =
                            Result.withDefault "" <| Base64.encode (property model)
                      in
                        href url
                    , attribute "download" filename
                    ]
                ]
                [ text "Download!" ]
    in
        Grid.containerFluid [] rows


cfg : Debounce.Config Model Msg
cfg =
    Debounce.config
        .debounceState
        (\model s -> { model | debounceState = s })
        Deb
        200


debCmd : Msg -> Cmd Msg
debCmd =
    Debounce.debounceCmd cfg


quipperView : String -> Html Msg
quipperView quipper =
    Form.form []
        [ Form.group []
            [ Form.label
                [ for "code" ]
                [ text "Code" ]
            , Textarea.textarea
                [ Textarea.id "code"
                , Textarea.value quipper
                , Textarea.onInput Quipper
                , Textarea.rows 10
                , Textarea.attrs
                    [ style
                        [ ( "font-family", "Fira Code, monospace" )
                        ]
                    ]
                ]
            ]
        ]


otherView : String -> Html Msg
otherView value =
    Form.form []
        [ Form.group []
            [ Textarea.textarea
                [ Textarea.id "code"
                , Textarea.disabled
                , Textarea.value value
                , Textarea.rows 11
                , Textarea.attrs
                    [ style
                        [ ( "font-family", "Fira Code, monospace" )
                        ]
                    ]
                ]
            ]
        ]


transformCmd : String -> Cmd Msg
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
        Http.send TransformResult request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Quipper quipper ->
            ( { model | quipper = quipper }, debCmd (Transform quipper) )

        Deb a ->
            Debounce.update cfg a model

        Transform quipper ->
            ( model, transformCmd quipper )

        TransformResult r ->
            let
                get p =
                    case Result.map p r of
                        Ok s ->
                            s

                        Err e ->
                            toString e
            in
                ( { model
                    | qpmc = get .qpmc
                    , nodes = get .nodes
                    , tree = get .tree
                  }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
