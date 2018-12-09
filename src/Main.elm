module Main exposing (HourUnit, Model, Msg(..), dayColumn, dayDecoder, dayHourView, getDay, hour, hourUnitDecoder, hoursColumn, init, inputButton, inputColumn, inputElement, main, planner, subscriptions, update, view)

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
    , day : List HourUnit
    , errorMessage : String
    }


getDay : Cmd Msg
getDay =
    Http.get
        { url = "/day/4_20_2069"
        , expect = Http.expectJson GetDay dayDecoder
        }

hourUnitDecoder : D.Decoder HourUnit
hourUnitDecoder =
    D.map2 HourUnit
        (D.field "date_hour" D.string)
        (D.field "content" (D.nullable D.string))


dayDecoder : D.Decoder (List HourUnit)
dayDecoder =
    D.list hourUnitDecoder


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , day = []
      , errorMessage = ""
      }
    , getDay
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
    | GetDay (Result Http.Error (List HourUnit))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newInput ->
            ( { model | input = newInput }, Cmd.none )

        PressButton ->
            ( model, Cmd.none )

        GetDay result ->
            case result of
                Ok day ->
                    ( { model | day = day }, Cmd.none )

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
        [ hoursColumn
        , dayColumn2 model "Sunday"
        , dayColumn "Monday"
        , dayColumn "Tuesday"
        , dayColumn "Wednesday"
        , dayColumn "Thursday"
        , dayColumn "Friday"
        , dayColumn "Saturday"
        , inputColumn model
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
        

dayColumn : String -> Element msg
dayColumn day =
    column [ Element.alignTop ]
        (List.concat
            [ [ el [ padding 30, width fill ] (text day) ]
            , List.map dayHourView (List.range 0 12)
            ]
        )

dayColumn2 : Model -> String -> Element msg
dayColumn2 model day =
    column [ Element.alignTop ]
        (List.concat
            [ [ el [ padding 30, width fill ] (text day) ]
            , List.map dayHourView2 model.day
            ]
        )
        

dayHourView : Int -> Element msg
dayHourView _ =
    Input.button
        [ padding 30, width fill ]
        { onPress = Nothing, label = el [] (text "foo") }

dayHourView2 : HourUnit -> Element msg
dayHourView2 hourUnit =
    Input.button
        [ padding 30, width fill ]
        { onPress = Nothing, label = el [] (text (Maybe.withDefault "" hourUnit.content)) }
            

hoursColumn : Element msg
hoursColumn =
    column [ width fill ]
        [ el [ padding 30, width fill ] (text "freetime")
        , hour "8:00"
        , hour "9:00"
        , hour "10:00"
        , hour "11:00"
        , hour "12:00"
        , hour "13:00"
        , hour "14:00"
        , hour "15:00"
        , hour "16:00"
        , hour "17:00"
        , hour "18:00"
        , hour "19:00"
        , hour "20:00"
        ]


hour : String -> Element msg
hour h =
    el
        [ Background.color (rgb255 100 255 100)
        , padding 30
        , width fill
        ]
        (text h)
