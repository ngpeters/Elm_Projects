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
import Array

main = program
  { init = (initialModel, Cmd.none),
  view = view,
  subscriptions = subscriptions,
  update = updateWithCommand
  }

initialModel = { droplist = [ ]
               , time = 0
               , position = 0 }

names = Array.fromList ["a", "b", "c", "d", "e", "f"]
colors = Array.fromList [ (hsl (degrees 190) 0.77 0.5)
                        , (hsl (degrees 121) 0.74 0.5)
                        , (hsl (degrees 41) 0.90 0.5)
                        , (hsl (degrees 16) 0.82 0.5)
                        , (hsl (degrees 317) 0.49 0.5)
                        , (hsl (degrees 285) 0.57 0.5)]

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
    Tick _ -> tick model

createCircle model = 
    creatMoreCircles 5 -200 model

creatMoreCircles numC xOffset model =
    case numC of 
        0 -> model
        _ -> creatMoreCircles 
                (numC - 1 ) 
                (xOffset + 100)  
                { model | droplist = ({ x = xOffset
                                    , y = 300
                                    , name = getName model.position
                                    , dropColor = getColor model.position } :: model.droplist)
                , position = (model.position + 1)% Array.length names}


getName position =
    case Array.get position names of
        Just s -> s
        _ -> "No"

getColor position =
    case Array.get position colors of
        Just s -> s
        _ -> red

tick model =
    model
        |> updateDrops
        |> updateTime

updateTime model =
    case model.time of
        300 -> addCircles { model | time = 0 }
        _ -> { model | time = model.time + 1 }

updateDrops model =
    { model | droplist = List.map fall model.droplist }

fall drop = 
  { drop | y = drop.y - 0.4 }

addCircles model =
    creatMoreCircles 5 -200 model

drawCanvas model =
  List.append [ backDropBox blue ] (List.map drawCircle model.droplist)
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