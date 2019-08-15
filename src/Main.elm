port module Main exposing (..)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time
import Json.Decode exposing (..)

import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (..)
import Collage exposing (..)
import Color exposing (..)


port audio : Model -> Cmd msg
port visualization : (List Int -> msg) -> Sub msg

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = 
  { gainValue : Float
  , frequencyValue: Float
  , windowWidth: Int
  , windowHeight: Int
  , visualizationData: List Int
  }

init : ({windowWidth: Int, windowHeight: Int}) -> (Model, Cmd Msg)
init windowDimensions =
  ( Model 0.001 3000 windowDimensions.windowWidth windowDimensions.windowHeight [],
  Cmd.none
  )

-- UPDATE
type Msg
  = IncrementGain
  | DecrementGain
  | IncrementFrequency
  | DecrementFrequency
  | UpdateDimensions Int Int
  | UpdateMouse (Float, Float)
  | Visualization (List Int)
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    IncrementGain ->
      let
          newModel = { model | gainValue = model.gainValue + 0.001 }
      in
      ( newModel, audio newModel )

    DecrementGain ->
      let
          newModel = { model | gainValue = model.gainValue - 0.001 }
      in
      ( newModel, audio newModel )
    
    IncrementFrequency ->
      let
          newModel = { model | frequencyValue = model.frequencyValue + 100 }
      in
      ( newModel, audio newModel )
    
    DecrementFrequency ->
      let
          newModel = { model | frequencyValue = model.frequencyValue - 100 }
      in
      ( newModel, audio newModel )

    UpdateDimensions width height ->
      let
          newModel = { model | windowWidth = width, windowHeight = height}
      in
      ( newModel, audio newModel )
    
    UpdateMouse ( x, y ) ->
      let
        -- gain is the percentage you are across the screen, from left to right, mapped from 0 to 0.03
        newGain = ( x / (toFloat model.windowWidth)) * 0.03
        -- frequency is the percentage you are vertically down the screen, mapped from 0 to 6000
        newFrequency = ( y / (toFloat model.windowHeight)) * 6000.0
        newModel = { model | frequencyValue = newFrequency, gainValue = newGain }
      in
         ( newModel, audio newModel)
    
    Visualization data ->
      ( {model | visualizationData = data}, Cmd.none)

    NoOp ->
      ( model, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize UpdateDimensions
    , Sub.map UpdateMouse (Browser.Events.onMouseMove mousePositionDecoder)
    , visualization Visualization
    ]

mousePositionDecoder : Decoder (Float, Float)
mousePositionDecoder =
    map2 Tuple.pair
        (field "clientX" float)
        (field "clientY" float)

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ section [] 
      [ h1 [] [ text "Gain" ]
      , button [ onClick DecrementGain ] [ text "-"]
      , div [] [ text (String.fromFloat model.gainValue)]
      , button [ onClick IncrementGain ] [ text "+" ]
      ]
    , section [] 
      [ h1 [] [ text "Frequency" ]
      , button [ onClick DecrementFrequency ] [ text "-"]
      , div [] [ text (String.fromFloat model.frequencyValue)]
      , button [ onClick IncrementFrequency ] [ text "+" ]
      ]
    , section []
      [ h1 [] [ text "Dimensions" ]
      , div [] [ text (String.fromInt model.windowHeight) ]
      , div [] [ text (String.fromInt model.windowWidth) ]
      ]
    , div []
      [ (visualizationGraph model)]
    -- , section []
    --   [ h1 [] [ text "Visualization Data" ] 
    --   , div [] [ text (List.foldr (\(x, y) -> \acc -> acc ++ " (" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")" ) "" (toPoints model))]
    --   ]
    ]

visualizationGraph : Model -> Html msg
visualizationGraph model =
  let
    points = toPoints model
  in
    path points 
    |> traced (solid thin (uniform red)) 
    |> shift ( (toFloat model.windowWidth) / -2, (toFloat model.windowHeight) / -2) 
    |> svgBox (toFloat model.windowWidth, toFloat model.windowHeight)
    

toPoints : Model -> List (Float, Float)
toPoints model =
  let
    sliceWidth = (toFloat model.windowWidth) / (toFloat (List.length model.visualizationData))
    indexedDatumToPoint n datum =
      let
        v = (toFloat datum) / 128
        y = (v * (toFloat model.windowHeight)) / 2
        x = sliceWidth * (toFloat n)
      in
        ( x, y )
  in
    model.visualizationData |> List.indexedMap indexedDatumToPoint