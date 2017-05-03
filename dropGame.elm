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

type alias Model = { droplist : List Drop
                    , time : Int
                    , singleCircleTime : Int
                    , seed : Random.Seed 
                    , singleCircle : List Drop
                    , mPositionX : Int
                    , mPositionY : Int 
                    , arrayPosition : Int}

initialModel = { droplist = []
               , time = 0
               , singleCircleTime = 0
               , seed = Random.initialSeed 0
               , singleCircle = [] 
               , mPositionX = 0
               , mPositionY = 0 
               , arrayPosition = 0 }

names1 = Array.fromList ["青", "緑", "オレンジ", "赤", "ピーンク", "紫"]
names = Array.fromList ["blue", "green", "orange", "red", "pink", "purple"]
names2 = Array.fromList ["あお", "みどり", "おれんじ", "あか", "ぴんく", "むらさき"]

colors = Array.fromList [ (hsl (degrees 190) 0.77 0.5)
                        , (hsl (degrees 121) 0.74 0.5)
                        , (hsl (degrees 41) 0.90 0.5)
                        , (hsl (degrees 16) 0.82 0.5)
                        , (hsl (degrees 337) 0.88 0.77)
                        , (hsl (degrees 285) 0.57 0.5)]

pointColors = Array.fromList [ (hsl (degrees 171) 0.86 0.42)
                            , (hsl (degrees 157) 0.86 0.47)
                            , (hsl (degrees 131) 0.43 0.47)
                            , (hsl (degrees 195) 0.70 0.51)
                            , (hsl (degrees 208) 0.68 0.51)
                            , (hsl (degrees 252) 0.39 0.51)]

type alias Drop = { x : Float
                  , y : Float 
                  , name : String 
                  , dropColor : Color
                  , dropColorIndex : Int }

subscriptions model = 
  Sub.batch 
  [ Mouse.clicks MouseMsg
   , AnimationFrame.diffs Tick ]

type Msg = Begin 
         | Tick Time 
         | InitialSeed Int 
         | MouseMsg Mouse.Position

updateWithCommand msg model =
    (update msg model, Cmd.none)

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Begin -> createCircle model
    Tick _ -> tick model
    InitialSeed val -> { model | seed = Random.initialSeed val }
    MouseMsg position ->  checkPosition { model | mPositionX = (position.x - 350)
                                        , mPositionY = ( (-1 * position.y) + 300 )  }

checkPosition model =
    { model | droplist = List.filter (deletClickedDrops model) model.droplist }

deletClickedDrops model drop =
    not ( ( abs ((toFloat model.mPositionX) - drop.x) < 30 ) &&
          ( abs ((toFloat model.mPositionY) - drop.y) < 30 ) )

createCircle model = 
    creatMoreCircles 5 -200.0 model

creatMoreCircles numC xOffset model =
    case numC of 
        0 -> model
        _ -> let (randomValue, newSeed) = Random.step (Random.int 0 5) model.seed in     
                creatMoreCircles 
                    (numC - 1 ) 
                    (xOffset + 100)  
                    { model | seed = newSeed
                        , droplist = ({ x = xOffset
                                        , y = 350.0
                                        , name = getName3 randomValue
                                        , dropColor = getColor randomValue
                                        , dropColorIndex = randomValue } :: model.droplist) }
getName position =
    case Array.get position names of
        Just s -> s
        _ -> "No"

getName1 i = Maybe.withDefault "" (Array.get i names)

getName2 i = Maybe.withDefault "" (Array.get i names1)

getName3 i = Maybe.withDefault "" (Array.get i names2)

getPointColor i =
    case Array.get i pointColors of
        Just s -> s
        _ -> black
getColor i =
    case Array.get i colors of
        Just s -> s
        _ -> red
tick model =
    model
        |> updateSingleCircle
        |> updateDrops
        |> updateTime
        |> deleteDrops

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
                        , singleCircle = ({ x = 305.0
                                        , y = 270.0
                                        , name = getName2 randomValue
                                        , dropColor = black
                                        , dropColorIndex = randomValue } :: model.singleCircle) }

updateDrops model =
    { model | droplist = List.map fall model.droplist }

fall drop = 
  { drop | y = drop.y - 0.4 }

deleteDrops model =
    { model | droplist = List.filter deleteDropsOffPage model.droplist}

deleteDropsOffPage drop =
    drop.y > -350

updateTime model =
    case model.time of
        300 -> addCircles { model | time = 0 }
        _ -> { model | time = model.time + 1 }

addCircles model =
    creatMoreCircles 5 -200.0 model

drawCanvas : Model -> Html Msg
drawCanvas model =
  List.concat [ [ backDropBox blue
                , drawPointsBox
                , drawSinglePointHelper model ] 
                , (List.map drawCircle model.droplist)
                , (List.map drawRect (List.reverse model.singleCircle) ) ]
    |> collage 700 600
    |> toHtml

drawPointsBox =
    group [ rect 30 530
                |> filled (hsl (degrees 256) 0.61 0.5) ]
                |> move (310, -35)

drawSinglePointHelper model =
    drawSinglePoint (getPointColor model.arrayPosition) { model | arrayPosition = (model.arrayPosition + 1)% Array.length pointColors }

drawSinglePoint color model =
    group [ rect 30 10
                |> filled color ]
                |> move (310, -295)

drawCircle model =
    group [ circle 30
            |> filled model.dropColor
          , model.name
            |> fromString
            |> Collage.text]
            |> move (model.x, model.y)

drawRect model =
    group [ rect 85 60
            |> filled model.dropColor
          , model.name
            |> fromString
            |> Text.color white
            |> Text.height 20
            |> Collage.text]
            |> move (model.x, model.y)

backDropBox color =
    group [ square 600
            |> filled color]

view : Model -> Html Msg
view model =
  div []
    [ drawCanvas model
    , button [ onClick Begin ] [ Html.text "Begin" ] ]

