import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
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
    , selectedHourUnit : HourUnit
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , week = []
      , errorMessage = ""
      , selectedHourUnit = { content = (Just ""), dateHour = "" }
      }
    , getWeek
    )
    

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


saveHourUnit : HourUnit -> Cmd Msg
saveHourUnit hourUnit =
    Http.post
        { url = "/hour"
        , expect = Http.expectString SavedHour
        , body = Http.jsonBody (hourUnitEncoder hourUnit)
        }


hourUnitEncoder : HourUnit -> E.Value
hourUnitEncoder hourUnit =
    E.object
        [ ("date_hour", E.string hourUnit.dateHour)
        , ("content", E.string (Maybe.withDefault "" hourUnit.content))
        ]
 
        
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
    | SelectHourUnit HourUnit
    | SavedHour (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newInput ->
            ( { model | input = newInput }, Cmd.none )

        PressButton ->
            ( { model | input = "" }, saveHourUnit { dateHour = model.selectedHourUnit.dateHour, content = (Just model.input) })

        SelectHourUnit hourUnit ->
            ( { model | selectedHourUnit = hourUnit }, Cmd.none )

        GetWeek result ->
            case result of
                Ok week ->
                    ( { model | week = week }, Cmd.none )

                Err err ->
                    ( { model | errorMessage = (errorMessage err) }, Cmd.none )

        SavedHour result ->
            case result of
                Ok message ->
                    ( { model | errorMessage = message }, getWeek )

                Err err ->
                    ( { model | errorMessage = (errorMessage err) }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "", value model.input, onInput Change ] []
        , button [ onClick PressButton ] [ text "Submit" ]
        , (text model.errorMessage)
        , selectedHourUnitView model
        , weekView model
        ]

        
selectedHourUnitView : Model -> Html Msg
selectedHourUnitView model =
    div []
        [ (text model.selectedHourUnit.dateHour)
        , (text (Maybe.withDefault "" model.selectedHourUnit.content))
        ]


weekView : Model -> Html Msg
weekView model =
    div []
        (List.map hourUnitView model.week)

            
hourUnitView : HourUnit -> Html Msg
hourUnitView hourUnit =
    div []
        [ Html.h2 [] [ (text (Maybe.withDefault "" hourUnit.content)) ]
        , button [ onClick (SelectHourUnit hourUnit) ] [ (text hourUnit.dateHour) ]
        ]
