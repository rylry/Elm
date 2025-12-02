module Main exposing (main)

import Grid exposing (Grid)
import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import WebGL
import Array
import World exposing (Block)
import Render exposing (uniforms, vertexShader, fragmentShader, meshFromChunk)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  Grid Block

init : () -> (Model, Cmd Msg)
init () =
  (fullChunk, Cmd.none)

type Msg
  = TimeDelta Float

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (fullChunk, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta

fullBlock : Block
fullBlock = { id = 1 }
    
fullChunk : Grid Block
fullChunk = { elements = Array.repeat 1 fullBlock, shape = [16, 16, 16] }
view : Model -> Html Msg
view chunk =
  let
    uniforms_ = uniforms 0
  in
  WebGL.toHtml
    [ width 700, height 700, style "display" "block" ]
    [ WebGL.entity vertexShader fragmentShader (meshFromChunk chunk) uniforms_ ]