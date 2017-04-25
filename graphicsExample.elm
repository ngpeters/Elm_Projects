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

initialModel = {  x = 0,
                  y = 0}

subscriptions model = 
  Sub.batch 
  [ AnimationFrame.diffs Tick ]

type Msg = Begin | Tick Time

updateWithCommand msg model =
    (update msg model, Cmd.none)

update msg model = 
  case msg of
    Begin -> model
    Tick time -> tick model

tick model =
  model
      |>fall

fall model = 
  { model | y = model.y - 0.4}

drawCanvas model =
  [ circleMaker "Hello"
      |> move (100, model.y)
  , circleMaker "Goodbye" 
      |> move (50, model.y)]
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
    , drawCanvas model]


