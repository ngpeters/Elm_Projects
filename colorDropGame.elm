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
                    , singleCircle : Int
                    , mPositionX : Int
                    , mPositionY : Int 
                    , arrayPosition : Int 
                    , points : Int
                    , pointsColor : List PointsC 
                    , isPlaying : Bool 
                    , youWon : Bool }

initialModel = { droplist = []
               , time = 0
               , singleCircleTime = 0
               , seed = Random.initialSeed 0
               , singleCircle = 0
               , mPositionX = 0
               , mPositionY = 0 
               , arrayPosition = 0
               , points = 0 
               , pointsColor = [] 
               , isPlaying = False 
               , youWon = False }

names1 = Array.fromList ["青", "緑", "橙色", "赤", "桃色", "紫", "黄色", "白", "灰色", "茶色"]
names = Array.fromList ["blue", "green", "orange", "red", "pink", "purple", "yello", "white", "gray", "brown"]
names2 = Array.fromList ["あお", "みどり", "だいだいいろ", "あか", "ももいろ", "むらさき", "きいろ", "しろ", "はいいろ", "ちゃいろ"]

colors = Array.fromList [ (hsl (degrees 190) 0.77 0.5)
                        , (hsl (degrees 121) 0.74 0.5)
                        , (hsl (degrees 41) 0.90 0.5)
                        , (hsl (degrees 16) 0.82 0.5)
                        , (hsl (degrees 337) 0.88 0.77)
                        , (hsl (degrees 285) 0.57 0.5)
                        , yellow
                        , white
                        , (hsl (degrees 0) 0 0.5)
                        , brown]

pointColors = Array.fromList [ (hsl (degrees 171) 0.86 0.42)
                            , (hsl (degrees 157) 0.86 0.47)
                            , (hsl (degrees 131) 0.43 0.47)
                            , (hsl (degrees 195) 0.70 0.51)
                            , (hsl (degrees 208) 0.68 0.51)
                            --, (hsl (degrees 252) 0.39 0.51)
                            , (hsl (degrees 278) 0.55 0.66)]

type alias Drop = { x : Float
                  , y : Float 
                  , name : String 
                  , dropColor : Color
                  , dropColorIndex : Int }

type alias PointsC = { y : Float
                     , pColor : Color
                     , pointIndex : Int }

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
    Begin -> createCircle { initialModel | isPlaying = True }
    Tick _ -> onlyUpdateIf tick model
    InitialSeed val -> { model | seed = Random.initialSeed val }
    MouseMsg position ->  checkPosition { model | mPositionX = (position.x - 350)
                                        , mPositionY = ( (-1 * position.y) + 300 )  }

onlyUpdateIf tick model = 
    if model.isPlaying then
        tick model
    else
        model

checkPosition model =
    updatePointsCounter { model | droplist = List.filter (deletClickedDrops model) model.droplist
                        , points = if List.all (deletClickedDrops model) model.droplist then
                                        model.points
                                    else
                                        model.points + 1 }

deletClickedDrops model drop =
    not ( ( abs ((toFloat model.mPositionX) - drop.x) < 35 ) &&
          ( abs ((toFloat model.mPositionY) - drop.y) < 35 ) &&
          ( isRightColor model.singleCircle drop ) )

isRightColor singleDrop clickedDrop =
    singleDrop == clickedDrop.dropColorIndex

createCircle model = 
    createMoreCircles 5 -200.0 model

createMoreCircles numC xOffset model =
    case numC of 
        0 -> model
        _ -> let (randomValue, newSeed) = Random.step (Random.int 0 (Array.length names1 - 1)) model.seed in     
                createMoreCircles 
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

tick : Model -> Model
tick model =
    model
        |> updateSingleCircle
        |> updateDrops
        |> updateTime
        |> deleteDrops
        |> deleteExtraPoints
        |> checkIfWon

updateSingleCircle model =
    case model.singleCircleTime of
        150 -> { model | singleCircleTime =  0 } 
        1 -> createSingleCircleHelper { model | singleCircleTime = model.singleCircleTime + 1 }
        _ -> { model | singleCircleTime = model.singleCircleTime + 1 }

createSingleCircleHelper model =
    let (randomValue, newSeed) = Random.step (Random.int 0 (Array.length names1 - 1)) model.seed in 
        { model | seed = newSeed
                , singleCircle = randomValue }

updateDrops model =
    { model | droplist = List.map fall model.droplist }

fall drop = 
  { drop | y = drop.y - 1.0 }

deleteDrops model =
    { model | droplist = List.filter deleteDropsOffPage model.droplist}

deleteDropsOffPage drop =
    drop.y > -350

deleteExtraPoints model =
    { model | pointsColor = List.take model.points model.pointsColor }

checkIfWon model = 
    if model.points == 53 then
        { model | youWon = True
                , isPlaying = False
                , droplist = [] }
    else
        model

updateTime model =
    case model.time of
        100 -> addCircles { model | time = 0 }
        _ -> { model | time = model.time + 1 }

addCircles model =
    createMoreCircles 5 -200.0 model

updatePointsCounter : Model -> Model
updatePointsCounter model =
    addUpPoints model.points -295.0 model

addUpPoints : Int -> Float -> Model -> Model
addUpPoints pointsCount yOffset model =
    case pointsCount of
        0 -> model
        _ -> addUpPoints
                (pointsCount - 1)
                (yOffset + 10)
                { model | pointsColor = ({ y = yOffset
                                         , pColor = ( getPointColor model.arrayPosition)
                                         , pointIndex = model.points } 
                                         :: model.pointsColor)
                        , arrayPosition = (model.arrayPosition + 1) % Array.length pointColors }

drawCanvas : Model -> Html Msg
drawCanvas model =
    List.concat [ [ backDropBox blue
                , drawPointsBox
                , drawRect model ] 
                , (List.map drawCircle model.droplist)
                , (List.map drawSinglePoint model.pointsColor)
                , if model.youWon then
                    [ winBox ]
                else 
                    [] ]
        |> collage 700 600
        |> toHtml

drawPointsBox =
    group [ rect 30 530
                |> filled (hsl (degrees 256) 0.61 0.5) ]
                |> move (310, -35)

drawSinglePoint pointColor =
    group [ rect 30 10
                |> filled pointColor.pColor ]
                |> move (310, pointColor.y )

drawCircle model =
    group [ circle 35
            |> filled model.dropColor
          , model.name
            |> fromString
            |> Collage.text ]
            |> move (model.x, model.y)

drawRect model =
    group [ rect 85 60
            |> filled black
          , getName2 model.singleCircle
            |> fromString
            |> Text.color white
            |> Text.height 20
            |> Collage.text]
            |> move (305.0, 270.0)

backDropBox color =
    group [ square 600
            |> filled color]

winBox =
    group [ rect 300 100 
            |> filled yellow
          , "You Won"
            |> fromString
            |> Text.height 40
            |> Collage.text ]

view : Model -> Html Msg
view model =
  div []
    [ drawCanvas model
    , button [ onClick Begin ] [ Html.text "Begin" ] 
    , div [] [ Html.text " Rules: Click on the circle that matches the text in the upper right corner." ]
    , div [] [ Html.text " If you get 53 points, you win and the game will clear."]
    ]
