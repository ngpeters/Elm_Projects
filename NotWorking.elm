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

initialModel = {  x = 0, vx = 0,
                  y = 0, vy = 0 }

subscriptions model = 
  Sub.batch 
  [ AnimationFrame.diffs Tick ]

type Msg = Begin | Tick Time

updateWithCommand msg model =
    (update msg model, Cmd.none)

update msg model = 
  case msg of
    Begin -> (model, Cmd.none)
    Tick time -> (tick model, Cmd.none)

tick model =
  model
      |>fall

fall model = 
  { model | vy = model.vy - 0.4}

drawCanvas = --put model
  [ circleMaker "Hello"
      |> move (300, 100)
  , circleMaker "Goodbye" 
      |> move (50, 70)]
    |> collage 500 500
    |> toHtml

circleMaker kanji = 
  group [ circle 30
            |> filled green
        , kanji 
            |> fromString
            |> Collage.text ]

view model =
  div []
    [ button [ onClick Begin ] [ Html.text "Begin" ]
    , drawCanvas]


