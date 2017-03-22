module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Base64
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Quipper


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
    }


type Msg
    = Quipper Quipper.Msg
    | ShowTree Bool
    | TransformResult (Result Http.Error Quipper.Response)


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
          }
        , quipperCmd
        )


view : Model -> Html Msg
view model =
    let
        quipperCard =
            ( "Quipper"
            , Quipper.view quipperCfg
            , "Main.hs"
            , .quipperState >> Quipper.code
            , [ Col.xs12
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
            , [ "flex-first" ]
            )

        treeCard =
            ( "Tree"
            , treeView
            , "tree.log"
            , .tree
            , [ Col.xs12
              , Col.md6
              , Col.xl4
              ]
            , [ "flex-last", "flex-md-unordered" ]
            )

        qpmcCard =
            ( "QPMC"
            , qpmcView
            , "output.qpmc"
            , .qpmc
            , [ Col.xs12
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
            , [ "flex-md-last" ]
            )

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
                (\( name, subview, filename, property, width, classes ) ->
                    Grid.col
                        (Col.attrs
                            [ ("mt-4" :: classes)
                                |> List.map (\c -> ( c, True ))
                                |> classList
                            ]
                            :: width
                        )
                        [ viewCard ( name, subview, filename, property, width ) ]
                )
                cardDescriptions

        viewCard ( name, subview, filename, property, width ) =
            Card.config []
                |> Card.headerH3
                    [ class "text-center" ]
                    [ text name ]
                |> Card.block []
                    [ Card.text [] [ subview model ] ]
                |> Card.footer []
                    [ downloadLink filename property ]
                |> Card.view

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
                [ codeArea model.qpmc ]
            else
                [ treeCheckbox model, codeArea model.qpmc ]
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
                    | qpmc = get .qpmc
                    , tree = get .tree
                  }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
