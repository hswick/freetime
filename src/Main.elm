

import Browser
import Element exposing (Element, alignRight, centerY, column, el, fill, height, maximum, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Maybe exposing (Maybe)


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


-- MODEL


type alias HourUnit =
    { dateHour : String
    , content : Maybe String
    }


type alias Model =
    { input : String
    , week : List HourUnit
    , errorMessage : String
    }


getWeek : Cmd Msg
getWeek =
    Http.post
        { url = "/week"
        , expect = Http.expectJson GetWeek weekDecoder
        , body = Http.jsonBody (E.list E.string ["4_20_2016", "4_21_2016", "4_22_2016", "4_23_2016", "4_24_2016", "4_25_2016", "4_26_2016"])
        }

hourUnitDecoder : D.Decoder HourUnit
hourUnitDecoder =
    D.map2 HourUnit
        (D.field "date_hour" D.string)
        (D.field "content" (D.nullable D.string))


weekDecoder : D.Decoder (List HourUnit)
weekDecoder =
    D.list hourUnitDecoder


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , week = []
      , errorMessage = ""
      }
    , getWeek
    )


errorMessage : Http.Error -> String
errorMessage error =
  case error of
    Http.BadUrl s ->
      ( "Bad Url error " ++ s )
    Http.Timeout ->
      "Timeout"
    Http.NetworkError ->
      "Network Error"
    Http.BadStatus code ->
      ( "Bad Status " ++ (String.fromInt code) )
    Http.BadBody s ->
      ( "BadBody " ++ s )
    

-- UPDATE


type Msg
    = Change String
    | PressButton
    | GetWeek (Result Http.Error (List HourUnit))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newInput ->
            ( { model | input = newInput }, Cmd.none )

        PressButton ->
            ( model, Cmd.none )

        GetWeek result ->
            case result of
                Ok week ->
                    ( { model | week = week }, Cmd.none )

                Err err ->
                    ( { model | errorMessage = (errorMessage err) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] (planner model)


planner : Model -> Element Msg
planner model =
    row []
        [ inputColumn model
        , errorView model
        ]


errorView : Model -> Element Msg
errorView model =
    text model.errorMessage

        
inputColumn : Model -> Element Msg
inputColumn model =
    column [ Element.alignTop, width fill ]
        [ inputElement model
        , inputButton
        ]


inputElement : Model -> Element Msg
inputElement model =
    Input.multiline [ width (fill |> Element.minimum 300), height (fill |> Element.minimum 100) ]
        { onChange = Change
        , text = model.input
        , placeholder = Nothing
        , label = Input.labelAbove [] (el [ padding 30 ] (text "4/20/2069"))
        , spellcheck = False
        }


inputButton : Element Msg
inputButton =
    Input.button [ padding 10, Element.alignRight, Background.color (rgb255 0 100 255), Font.color (rgb255 255 255 255) ]
        { onPress = Just PressButton
        , label = el [] (text "Submit")
        }
