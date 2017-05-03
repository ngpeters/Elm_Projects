-- 341 Final Project with Elm --
-- Natalie Peters
-- Random falling droplets of color
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
import Random
import Mouse

main = program
  { init = (initialModel, generateInitialSeed),
  view = view,
  subscriptions = subscriptions,
  update = updateWithCommand
  }

generateInitialSeed = Random.generate InitialSeed (Random.int Random.minInt Random.maxInt)

initialModel = { droplist = []
               , time = 0
               , singleCircleTime = 0
               , position = 0 
               , seed = Random.initialSeed 0
               , singleCircle = [] }

names1 = Array.fromList ["青", "緑", "オレンジ", "赤", "ピーンク", "紫"]
names = Array.fromList ["blue", "green", "orange", "red", "pink", "purple"]
colors = Array.fromList [ (hsl (degrees 190) 0.77 0.5)
                        , (hsl (degrees 121) 0.74 0.5)
                        , (hsl (degrees 41) 0.90 0.5)
                        , (hsl (degrees 16) 0.82 0.5)
                        , (hsl (degrees 337) 0.88 0.77)
                        , (hsl (degrees 285) 0.57 0.5)]

type alias Drop = { x : Int
                  , y : Int 
                  , name : String 
                  , dropColor : Color }

subscriptions model = 
  Sub.batch 
  [ --Mouse.clicks MouseMsg ,
   AnimationFrame.diffs Tick ]

type Msg = Begin 
         | Tick Time 
         | InitialSeed Int 
         --| MouseMsg Mouse.Position

updateWithCommand msg model =
    (update msg model, Cmd.none)

update msg model = 
  case msg of
    Begin -> createCircle model
    Tick _ -> tick model
    InitialSeed val -> { model | seed = Random.initialSeed val }
    --MouseMsg position ->  checkPosition model

--checkPosition model = 
  --  model
    --checkPositionHelper (List.length model.droplist) model position

--checkPositionHelper num model position =


createCircle model = 
    creatMoreCircles 5 -200 model

creatMoreCircles numC xOffset model =
    case numC of 
        0 -> model
        _ -> let (randomValue, newSeed) = Random.step (Random.int 0 5) model.seed in     
                creatMoreCircles 
                    (numC - 1 ) 
                    (xOffset + 100)  
                    { model | seed = newSeed
                        , droplist = ({ x = xOffset
                                        , y = 350
                                        , name = getName2 randomValue
                                        , dropColor = getColor randomValue } :: model.droplist) }
getName position =
    case Array.get position names of
        Just s -> s
        _ -> "No"

getName2 i = Maybe.withDefault "" (Array.get i names)

getColor i =
    case Array.get i colors of
        Just s -> s
        _ -> red

tick model =
    model
        |> updateSingleCircle
        |> updateDrops
        |> updateTime
        --|> deleteDrops

updateSingleCircle model =
    case model.singleCircleTime of
        800 -> { model | singleCircleTime =  0 } 
        1 -> createSingleCircleHelper { model | singleCircleTime = model.singleCircleTime + 1 }
        _ -> { model | singleCircleTime = model.singleCircleTime + 1 }

createSingleCircleHelper model =
    createSingleCircle 1 model

createSingleCircle num model =
    case num of 
        0 -> model
        _ -> let (randomValue, newSeed) = Random.step (Random.int 0 5) model.seed in     
                createSingleCircle 
                    (num - 1 )  
                    { model | seed = newSeed
                        , singleCircle = ({ x = 305
                                        , y = 270
                                        , name = getName2 randomValue
                                        , dropColor = getColor randomValue } :: model.singleCircle) }

updateDrops model =
    { model | droplist = List.map fall model.droplist }

fall drop = 
  { drop | y = drop.y - 0.4 }

updateTime model =
    case model.time of
        300 -> addCircles { model | time = 0 }
        _ -> { model | time = model.time + 1 }

addCircles model =
    creatMoreCircles 5 -200 model

drawCanvas model =
  List.append [ backDropBox blue ] (List.map drawCircle model.droplist)
    |> collage 700 600
    |> toHtml

drawCircle model =
    group [ circle 30
            |> filled model.dropColor
          , model.name
            |> fromString
            |> Collage.text]
            |> move (model.x, model.y)

backDropBox color =
    group [ square 600
            |> filled color]

view model =
  div []
    [ button [ onClick Begin ] [ Html.text "Begin" ]
    , drawCanvas model ]