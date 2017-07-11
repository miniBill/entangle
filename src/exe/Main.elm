module Main exposing (main)

import Base64
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Quipper
import Time exposing (Time)


main : Program Never Model Msg
main =
    program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init = init
        }


type alias Model =
    { quipperState : Quipper.State
    , showTree : Bool
    , tree : String
    , qpmc : String
    , elapsed : String
    }


type Msg
    = Quipper Quipper.Msg
    | ShowTree Bool
    | TransformResult (Result Http.Error ( Quipper.Response, Time ))


quipperCfg : Quipper.Config Model Msg
quipperCfg =
    Quipper.config
        .quipperState
        (\model s -> { model | quipperState = s })
        Quipper
        TransformResult


init : ( Model, Cmd Msg )
init =
    let
        ( quipperState, quipperCmd ) =
            Quipper.init quipperCfg
    in
    ( { quipperState = quipperState
      , showTree = True
      , tree = ""
      , qpmc = ""
      , elapsed = ""
      }
    , quipperCmd
    )


view : Model -> Html Msg
view model =
    let
        basename =
            model.quipperState.functionName

        quipperCard =
            { name = "Quipper"
            , view = Quipper.view quipperCfg
            , extension = "hs"
            , content = Quipper.code model.quipperState
            , width =
                [ Col.xs12
                , if model.showTree then
                    Col.md6
                  else
                    Col.md12
                , Col.lg6
                , if model.showTree then
                    Col.xl4
                  else
                    Col.xl6
                ]
            , classes = [ "flex-first" ]
            }

        treeCard =
            { name = "Tree"
            , view = treeView
            , extension = "log"
            , content = model.tree
            , width =
                [ Col.xs12
                , Col.md6
                , Col.xl4
                ]
            , classes = [ "flex-last", "flex-md-unordered" ]
            }

        qpmcCard =
            { name = "QPMC"
            , view = qpmcView
            , extension = "qpmc"
            , content = model.qpmc
            , width =
                [ Col.xs12
                , Col.md12
                , if model.showTree then
                    Col.lg12
                  else
                    Col.lg6
                , if model.showTree then
                    Col.xl4
                  else
                    Col.xl6
                ]
            , classes = [ "flex-md-last" ]
            }

        cardDescriptions =
            if model.showTree then
                [ quipperCard
                , treeCard
                , qpmcCard
                ]
            else
                [ quipperCard
                , qpmcCard
                ]

        rows =
            List.map
                (\description ->
                    Grid.col
                        (Col.attrs
                            [ ("mt-4" :: description.classes)
                                |> List.map (\c -> ( c, True ))
                                |> classList
                            ]
                            :: description.width
                        )
                        [ viewCard description ]
                )
                cardDescriptions

        viewCard description =
            Card.config []
                |> Card.headerH3
                    [ class "text-center" ]
                    [ text description.name ]
                |> Card.block []
                    [ Card.text [] [ description.view model ] ]
                |> Card.footer []
                    [ downloadLink description ]
                |> Card.view

        downloadLink description =
            Button.linkButton
                [ Button.primary
                , Button.attrs
                    [ let
                        url =
                            head ++ encoded

                        head =
                            "data:application/force-download;charset=UTF-8;base64,"

                        encoded =
                            Result.withDefault "" <| Base64.encode description.content
                      in
                      href url
                    , attribute "download" <| basename ++ "." ++ description.extension
                    ]
                ]
                [ text "Download" ]
    in
    Grid.containerFluid []
        [ Grid.row [] rows ]


treeCheckbox : Model -> Html Msg
treeCheckbox model =
    Checkbox.checkbox
        [ Checkbox.onCheck ShowTree
        , Checkbox.checked model.showTree
        ]
        "Show tree"


codeArea : String -> Html msg
codeArea value =
    Textarea.textarea
        [ Textarea.id "code"
        , Textarea.disabled
        , Textarea.value value
        , Textarea.rows 30
        , Textarea.attrs
            [ style
                [ ( "font-family", "Fira Code, monospace" )
                ]
            ]
        ]


treeView : Model -> Html Msg
treeView model =
    Form.form []
        [ Form.group []
            [ treeCheckbox model
            , codeArea model.tree
            ]
        ]


qpmcView : Model -> Html Msg
qpmcView model =
    Form.form []
        [ Form.group [] <|
            if model.showTree then
                [ text model.elapsed, br [] [], codeArea model.qpmc ]
            else
                [ text model.elapsed, br [] [], treeCheckbox model, codeArea model.qpmc ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Quipper q ->
            Quipper.update quipperCfg q model

        ShowTree showTree ->
            ( { model | showTree = showTree }, Cmd.none )

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
                | qpmc = get (Tuple.first >> .qpmc)
                , elapsed = get (\( _, elapsed ) -> "Elapsed time: " ++ toString (Time.inSeconds elapsed) ++ "s")
                , tree = get (Tuple.first >> .tree)
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
