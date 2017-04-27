-- 341 Final Project with Elm --
-- Natalie Peters
-- 
--
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Time exposing (..)
import AnimationFrame

main = program
  { init = (initialModel, Cmd.none),
  view = view,
  subscriptions = subscriptions,
  update = updateWithCommand
  }

initialModel = [ ]

names = ["a", "b", "c", "d", "e", "f"]

--randomListNumer = Int

type alias Drop = { x : Int
                  , y : Int 
                  , name : String 
                  , dropColor : Color }

subscriptions model = 
  Sub.batch 
  [ AnimationFrame.diffs Tick ]

type Msg = Begin | Tick Time

updateWithCommand msg model =
    (update msg model, Cmd.none)

update msg model = 
  case msg of
    Begin -> createCircle model
    Tick time -> tick model

createCircle model = 
    creatMoreCircles 5 -200 model

creatMoreCircles numC xOffset model =
    case numC of 
        0 -> model
        _ -> creatMoreCircles 
               (numC - 1 ) 
               (xOffset + 100)  
               ({ x = xOffset
               , y = 300
               , name = getName
               , dropColor = getColor } :: model)

getName =
    --Random.generate randomListNumer (Random.int 1 (length names))
    "hi"
    --List.head names
getColor =
    red
    --List of color

tick model =
    List.map fall model

fall model = 
  { model | y = model.y - 0.4}

drawCanvas model =
  List.append [ backDropBox blue ] (List.map drawCircle model)
    |> collage 500 500
    |> toHtml

drawCircle model =
    group [ circle 30
            |> filled model.dropColor 
          , model.name
            |> fromString
            |> Collage.text]
            |> move (model.x, model.y)

backDropBox color =
    group [ square 500
            |> filled color]

view model =
  div []
    [ button [ onClick Begin ] [ Html.text "Begin" ]
    , drawCanvas model ]