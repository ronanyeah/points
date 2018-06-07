module Main exposing (main)

import Browser exposing (Env, Page)
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Time exposing (Posix)


rgb : Float -> Float -> Float -> Color
rgb r g b =
    Element.rgb (r / 255) (g / 255) (b / 255)



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
        , Element.layout [ height fill, Background.color uc ] <|
            column [ spacing 20, height fill ]
                [ row []
                    [ el [ width fill, padding 10, Font.size 40, Background.color red ] <| el [ centerX, centerY ] <| text <| String.fromInt model.red
                    , el [ width fill, padding 10, Font.size 40, Background.color blue ] <| el [ centerX, centerY ] <| text <| String.fromInt model.blue
                    ]
                , pointRow 2
                , pointRow 3
                , pointRow 4
                , case model.timer of
                    Just ( m, s, ms ) ->
                        column [ spacing 10, padding 10 ]
                            [ el [ width fill, padding 15, Background.color sc, Font.color white ] <|
                                el [ centerX ] <|
                                    row [ spacing 15 ]
                                        [ el [] <| text <| pad m
                                        , el [] <| text ":"
                                        , el [] <| text <| pad s
                                        , el [] <| text ":"
                                        , el [] <| text <| pad ms
                                        ]
                            , reset
                            ]

                    Nothing ->
                        column [ spacing 10, padding 10 ]
                            [ button
                                [ width fill, padding 15, Background.color sc, Font.color white, shadow ]
                                { onPress = Just Start
                                , label = el [] <| text "Start"
                                }
                            , reset
                            ]
                ]
        ]
    }


pointRow : Int -> Element Msg
pointRow i =
    row [ spacing 10, width fill, padding 10, Font.color white ]
        [ button [ width fill, Background.color black, padding 10, shadow ]
            { onPress = Just <| Red i
            , label = el [] <| text <| String.fromInt i
            }
        , button [ width fill, Background.color black, padding 10, shadow ]
            { onPress = Just <| Blue i
            , label = el [] <| text <| String.fromInt i
            }
        ]


reset : Element Msg
reset =
    button [ width fill, padding 15, Background.color sc, Font.color white, shadow ]
        { onPress = Just Clear
        , label = el [] <| text "Reset"
        }


white : Color
white =
    rgb 255 255 255


blue : Color
blue =
    rgb 2 103 193


red : Color
red =
    rgb 242 66 54


black : Color
black =
    rgb 10 17 40


uc : Color
uc =
    rgb 88 114 145


sc : Color
sc =
    rgb 85 5 39


grey : Color
grey =
    rgb 169 169 169


shadow : Attribute Msg
shadow =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 3
        , size = 1
        , color = grey
        }
