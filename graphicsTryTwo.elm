import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model = Time


init : (Model, Cmd Msg)
init =
  (0, Cmd.none)



-- UPDATE


type Msg
  = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (newTime, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model)
      
    changeY1 =
      toString (50 + 40 * tan angle)
     
    changeY2 =
      toString (50 + 30 * tan angle)
      
  in
    svg [ viewBox "0 0 500 500", width "700px" ]
      [ circle [ cx "50", cy changeY1, r "15", fill "#0B79CE"] []
      , circle [ cx "100", cy changeY2, r "15", fill "#0B79CE" ] []
      , circle [ cx "150", cy changeY1, r "15", fill "#0B79CE" ] []
      , circle [ cx "200", cy changeY2, r "15", fill "#0B79CE" ] []
      ]
