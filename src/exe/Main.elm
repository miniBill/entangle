module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Base64
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
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
    , qpmc : String
    , nodes : String
    , tree : String
    }


type Msg
    = Quipper Quipper.Msg
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
          , qpmc = ""
          , nodes = ""
          , tree = ""
          }
        , quipperCmd
        )


view : Model -> Html Msg
view model =
    let
        cardList =
            [ [ ( "Quipper", (\_ -> Quipper.view quipperCfg model), "Main.hs", .quipperState >> .code )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Quipper q ->
            Quipper.update quipperCfg q model

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
