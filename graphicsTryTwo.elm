--This code has 4 circles that fall from the top of the page
--It cycles through with a Tangent function so that it always has
--circles falling.


--Html to start the main off and initiazlize everything
import Html exposing (Html)
--Svg and Svg.Attributes are used for the animation
import Svg exposing (..)
import Svg.Attributes exposing (..)
--The use of time here was not great, it makes everything jolt when it runs
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

--init with 0 and no commands (0, Cmd.none)
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

--I wanted things falling from the page and
--they do. I am not yet sure if having them fall
--faster at the end is what I will end up with though.

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
