import Browser

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value, placeholder, hidden)
import Html.Styled.Events exposing (onClick, onInput, onMouseOver, onMouseLeave)

import Http
import Json.Decode as D exposing (Decoder, field, string)
import Json.Encode as E
import Maybe exposing (Maybe)
import Array exposing (Array)
import Time exposing (millisToPosix, utc)
import Date exposing (Date, floor, add, format, compare, fromPosix)
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }


-- MODEL


type alias HourUnit =
    { dateHour : String
    , content : String
    }


type alias Model =
    { input : String
    , week : List HourUnit
    , errorMessage : String
    , selectedHourUnit : HourUnit
    , mouseOverHourUnit : HourUnit
    , inputVisibility : String
    , inputMessage : String
    , indexDate : Date
    , today : Date
    }

    
emptyHourUnit : HourUnit
emptyHourUnit = { content = "", dateHour = "" }

                
init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , week = []
      , errorMessage = ""
      , selectedHourUnit = emptyHourUnit
      , mouseOverHourUnit = emptyHourUnit
      , inputVisibility = "hidden"
      , inputMessage = "Select an hour unit and fill in the content."
      , indexDate = (fromPosix utc (millisToPosix 0))
      , today = (fromPosix utc (millisToPosix 0))
      }
    , getToday
    )

    
getToday : Cmd Msg
getToday =
    Task.perform GetToday Date.today


beforeTodayVisibility : (Date, HourUnit) -> String
beforeTodayVisibility (today, hourUnit) =
    let
        dateString = Maybe.withDefault "" ((String.split "_" hourUnit.dateHour) |> List.head)
                     
        date = Date.fromIsoString dateString
    in
        case date of
            Ok d ->
                if (compare d today) == LT then
                    "hidden"
                else
                    "visible"

            Err _ ->
                "hidden"
    

weekDates : Date -> List String
weekDates date =
    let
        beginningOfWeek = floor Date.Monday date
    in
        List.map dateToString
            [ beginningOfWeek
            , add Date.Days 1 beginningOfWeek
            , add Date.Days 2 beginningOfWeek
            , add Date.Days 3 beginningOfWeek
            , add Date.Days 4 beginningOfWeek
            ]


dateToString : Date -> String
dateToString date =
    Date.format "y-M-d" date
    
                
getWeek : List String -> Cmd Msg
getWeek dates =
    Http.post
        { url = "/week"
        , expect = Http.expectJson GetWeek weekDecoder
        , body = Http.jsonBody (E.list E.string dates)
        }


hourUnitDecoder : D.Decoder HourUnit
hourUnitDecoder =
    D.map2 HourUnit
        (D.field "date_hour" D.string)
        (D.field "content" D.string)


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
        , ("content", E.string hourUnit.content)
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
    | MouseOverHourUnit HourUnit
    | MouseLeaveTable
    | SavedHour (Result Http.Error String)
    | GetToday Date
    | GetLastWeek
    | GetCurrentWeek
    | GetNextWeek


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newInput ->
            ( { model | input = newInput }, Cmd.none )

        PressButton ->
            let
                newHourUnit = { dateHour = model.selectedHourUnit.dateHour, content = model.input }
            in
                ( { model | input = "", selectedHourUnit = newHourUnit }, saveHourUnit newHourUnit )

        SelectHourUnit hourUnit ->
            let
                visibility = beforeTodayVisibility (model.today, hourUnit)
            in
                ( { model | selectedHourUnit = hourUnit, inputVisibility = visibility, inputMessage = (formatDateHour hourUnit.dateHour) }, Cmd.none )

        MouseOverHourUnit hourUnit ->
            ( { model | mouseOverHourUnit = hourUnit }, Cmd.none )

        MouseLeaveTable ->
            ( { model | mouseOverHourUnit = emptyHourUnit }, Cmd.none )

        GetWeek result ->
            case result of
                Ok week ->
                    ( { model | week = week }, Cmd.none )

                Err err ->
                    ( { model | errorMessage = (errorMessage err) }, Cmd.none )

        SavedHour result ->
            case result of
                Ok message ->
                    ( { model | errorMessage = message }, getWeek (weekDates model.indexDate) )

                Err err ->
                    ( { model | errorMessage = (errorMessage err) }, Cmd.none )

        GetToday date ->
            ( { model | indexDate = date, today = date }, getWeek (weekDates date) )

        GetLastWeek ->
            let
                newIndexDate = (add Date.Days -7 model.indexDate)
            in
                ( { model | indexDate = newIndexDate }, getWeek (weekDates newIndexDate) )

        GetCurrentWeek ->
            ( model, getToday )

        GetNextWeek ->
            let
                newIndexDate = (add Date.Days 7 model.indexDate)
            in
                ( { model | indexDate = newIndexDate }, getWeek (weekDates newIndexDate) ) 
                    


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ hourUnitTableView model
        , terminalView model
        ]


terminalView : Model -> Html Msg
terminalView model =
    div [ css [ width (pct 49), float left, paddingLeft (pct 1) ] ]
        [ weekNavigationView model
        , mouseOverView model
        , selectedHourUnitView model
        , inputView model
        ]


weekNavigationView : Model -> Html Msg
weekNavigationView model =
    div [ css [ height (px 100), borderBottom2 (px 10) solid ] ]
        [ weekButton ((textAlign left), GetLastWeek, "Last")
        , weekButton ((textAlign center), GetCurrentWeek, "Current")
        , weekButton ((textAlign right), GetNextWeek, "Next")
        ]
        

weekButton : (Style, Msg, String) -> Html Msg
weekButton (s, m, t) =
    let
        buttonContainerStyle = [ width (pct 33), float left, s ]
                               
        buttonStyle = css [ backgroundColor (rgb 255 255 255 ), border2 (px 3) solid, float center, margin auto, width (px 100) ]
    in
        div [ css buttonContainerStyle ] [ button [ onClick m, buttonStyle ] [ (text t) ] ]

        
formatDateHour : String -> String
formatDateHour dateHour =
    let
        splitDateHour = String.split "_" dateHour
    in
        case splitDateHour of
            [ date, hour ] ->
                (date ++ " " ++ hour ++ ":00 UTC")

            _ ->
                "oh no... :O"

            
mouseOverView : Model -> Html Msg
mouseOverView model =
    let
        hourUnit = model.mouseOverHourUnit
                   
        style = css [ marginBottom (px 5) ]
    in
        if hourUnit.dateHour == "" then
            div [ css [ height (px 100) ] ]
                [ div [ style ]
                      [ (text "Hover mouse over a tile to preview it's contents") ]
                ]
        else
            div [ css [ height (px 100) ] ]
                [ div [ style ] [ (text (formatDateHour hourUnit.dateHour)) ]
                , div [] [ (text hourUnit.content) ]
                ]
                
                
inputView : Model -> Html Msg
inputView model =
    div [ css [ marginBottom (px 5) ], hidden (decodeInputVisibility model.inputVisibility) ]
        [ input [ placeholder "", value model.input, onInput Change, css [ width (pct 100) ] ] []
        , div [] [ inputButton
                  , div [ css [ float left ] ] [ (text model.errorMessage) ]
                  ]
        ]


inputButton : Html Msg
inputButton =              
    button [ onClick PressButton, css [ backgroundColor (rgb 255 255 255 )
                                      , border2 (px 3) solid
                                      , marginRight (px 10)
                                      , float left
                                      , width (px 100)
                                      ]
           ]
        [ text "Submit" ]
    
decodeInputVisibility : String -> Bool
decodeInputVisibility inputVisibility =
    if inputVisibility == "visible" then
        False
    else
        True

selectedHourUnitView : Model -> Html Msg
selectedHourUnitView model =
    div [ css [ paddingTop (px 10), height (px 100), borderTop2 (px 10) solid ] ]
        [ div [ css [ borderBottom2 (px 1) solid, marginBottom (px 5) ] ] [ (text model.inputMessage) ]
        , div [] [ (text model.selectedHourUnit.content) ]
        ]


partitionHourUnits : (Array HourUnit) -> (List (List (Maybe HourUnit)))
partitionHourUnits hourUnits =
    ( List.map ( splitHours hourUnits ) (List.range 0 12) )

        
splitHours : (Array HourUnit) -> Int -> List (Maybe HourUnit)
splitHours hourUnits hour =
    ( List.map
          (\day -> Array.get ((day * 13) + hour) hourUnits)
          (List.range 0 4)
    )
        
hourUnitTableView : Model -> Html Msg
hourUnitTableView model =
    let
        hourUnitTable = (model.week |> Array.fromList |> partitionHourUnits)
    in
        Html.Styled.table
            [ css [ float left, width (pct 50), height (vh 97)]
            , onMouseLeave MouseLeaveTable
            ]
            (List.map (hourRowView model) hourUnitTable)


hourRowView : Model -> List (Maybe HourUnit) -> Html Msg
hourRowView model hourUnitRow =
    tr [ css [] ]
       (List.map (maybeHourUnitView model) hourUnitRow)

           
maybeHourUnitView : Model -> Maybe HourUnit -> Html Msg
maybeHourUnitView model hourUnit =
    let
        hu = Maybe.withDefault { dateHour = "not found", content = "" } hourUnit
    in
        case hu.dateHour of
            "not found" ->
                td [] [ (text "not found") ]

            _ ->
                hourUnitView model hu
                    

hourUnitView : Model -> HourUnit -> Html Msg
hourUnitView model hourUnit =
    let
        color = selectHourUnitColor model hourUnit
    in
        td
        [ css [ border2 (px 1) solid, backgroundColor color ]
        , onClick (SelectHourUnit hourUnit)
        , onMouseOver (MouseOverHourUnit hourUnit)
        ]
        []


selectHourUnitColor : Model -> HourUnit -> Color
selectHourUnitColor model hourUnit =
    if model.selectedHourUnit.dateHour == hourUnit.dateHour then
        (rgb 255 0 0)
    else if model.mouseOverHourUnit.dateHour == hourUnit.dateHour then
        (rgb 255 255 0)
    else if hourUnit.content == "" then
        (rgb 255 255 255)
    else
        (rgb 0 255 0)
