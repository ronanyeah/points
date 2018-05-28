module Main exposing (main)

import Browser exposing (Env, Page)
import Element exposing (centerX, centerY, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Time exposing (Posix)


-- MAIN


main : Program () Model Msg
main =
    Browser.fullscreen
        { init = init
        , update = update
        , view = view
        , onNavigation = Nothing
        , subscriptions =
            .timer
                >> Maybe.map (\x -> Time.every 10 (always (Tick x)))
                >> Maybe.withDefault Sub.none
        }



-- MODEL


type alias Model =
    { red : Int
    , blue : Int
    , timer : Maybe ( Int, Int, Int )
    }


init : Browser.Env () -> ( Model, Cmd Msg )
init _ =
    ( { red = 0
      , blue = 0
      , timer = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Red Int
    | Blue Int
    | Clear
    | Start
    | Tick ( Int, Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Red i ->
            ( { model | red = model.red + i }, Cmd.none )

        Blue i ->
            ( { model | blue = model.blue + i }, Cmd.none )

        Clear ->
            ( { model | blue = 0, red = 0, timer = Nothing }, Cmd.none )

        Start ->
            ( { model | timer = Just ( 5, 0, 0 ) }, Cmd.none )

        Tick t ->
            let
                timer =
                    case t of
                        ( 0, 0, 0 ) ->
                            Nothing

                        ( m, 0, 0 ) ->
                            Just ( m - 1, 59, 99 )

                        ( 0, s, 0 ) ->
                            Just ( 0, s - 1, 99 )

                        ( 0, 0, ms ) ->
                            Just ( 0, 0, ms - 1 )

                        ( m, s, 0 ) ->
                            Just ( m, s - 1, 99 )

                        ( m, s, ms ) ->
                            Just ( m, s, ms - 1 )
            in
            ( { model | timer = timer }, Cmd.none )



-- VIEW


pad : Int -> String
pad =
    String.fromInt >> String.padLeft 2 '0'


view : Model -> Page Msg
view model =
    { title = "Points"
    , body =
        [ Html.node "meta"
            [ Html.Attributes.name "viewport"
            , Html.Attributes.attribute "content"
                "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
            ]
            []
        , Element.layout [] <|
            column [ spacing 20 ]
                [ row []
                    [ el [ width fill, height <| px 40 ] <| el [ centerX, centerY ] <| text <| String.fromInt model.red
                    , el [ width fill, height <| px 40 ] <| el [ centerX, centerY ] <| text <| String.fromInt model.blue
                    ]
                , row [ Element.spaceEvenly, width fill ]
                    [ button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Red 2
                        , label = el [] <| text "2"
                        }
                    , button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Blue 2
                        , label = el [] <| text "2"
                        }
                    ]
                , row [ Element.spaceEvenly, width fill ]
                    [ button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Red 3
                        , label = el [] <| text "3"
                        }
                    , button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Blue 3
                        , label = el [] <| text "3"
                        }
                    ]
                , row [ Element.spaceEvenly, width fill ]
                    [ button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Red 4
                        , label = el [] <| text "4"
                        }
                    , button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                        { onPress = Just <| Blue 4
                        , label = el [] <| text "4"
                        }
                    ]
                , case model.timer of
                    Just _ ->
                        Element.text ""

                    Nothing ->
                        button
                            [ width fill, Background.color <| Element.rgb 20 20 20 ]
                            { onPress = Just Start
                            , label = el [] <| text "Start"
                            }
                , case model.timer of
                    Just ( m, s, ms ) ->
                        column []
                            [ button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                                { onPress = Just Clear
                                , label =
                                    row [ Element.spaceEvenly ]
                                        [ el [ Element.padding 20 ] <| text <| pad m
                                        , el [ Element.padding 20 ] <| text <| pad s
                                        , el [ Element.padding 20 ] <| text <| pad ms
                                        ]
                                }
                            , button [ width fill, Background.color <| Element.rgb 20 20 20 ]
                                { onPress = Just Clear
                                , label = el [] <| text "Reset"
                                }
                            ]

                    Nothing ->
                        Element.none
                ]
        ]
    }
