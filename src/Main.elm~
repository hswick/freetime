import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Element exposing (Element, el, text, row, alignRight, fill, width, height, rgb255, spacing, centerY, padding, column, maximum)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type alias Model = 
  { input : String 
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { input = "" }, Cmd.none )



-- UPDATE


type Msg
 = Change String
 | PressButton


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newInput ->
      ( { model | input = newInput }, Cmd.none )

    PressButton ->
        ( model, Cmd.none )



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
    , dayColumn "Sunday"
    , dayColumn "Monday"
    , dayColumn "Tuesday"
    , dayColumn "Wednesday"
    , dayColumn "Thursday"
    , dayColumn "Friday"
    , dayColumn "Saturday"
    , inputColumn model
    ]

inputColumn : Model -> Element Msg
inputColumn model =
  column [ Element.alignTop, width fill ]
    [ inputElement model
    , inputButton
    ]

inputElement : Model -> Element Msg
inputElement model =
  Input.multiline [ (width (fill |> Element.minimum 300)), height (fill |> Element.minimum 100) ]
      { onChange = Change
      , text = model.input
      , placeholder = Nothing
      , label = (Input.labelAbove [] (el [ padding 30 ] (text "4/20/2069")))
      , spellcheck = False
      } 

inputButton : Element Msg
inputButton =
  Input.button [ padding 10, Element.alignRight, Background.color (rgb255 0 100 255), Font.color (rgb255 255 255 255)]
               { onPress = (Just PressButton)
               , label = (el [] (text "Submit"))
               }

dayColumn : String -> Element msg
dayColumn day =
    column [ Element.alignTop ]
    ( List.concat [ [ el [ padding 30, width fill] (text day) ]
                  , List.map dayHourView ( List.range 0 12 )
                  ]
    )

dayHourView : Int -> Element msg
dayHourView _ =
    Input.button 
        [ padding 30, width fill ]
        { onPress = Nothing, label = (el [] (text "foo")) }

hoursColumn : Element msg
hoursColumn =
    column [ width fill ] 
    [ el [ padding 30, width fill] (text "freetime")
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
    [ Background.color ( rgb255 100 255 100)
    , padding 30
    , width fill
    ]
    (text h)