module Main exposing (main)

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (style, width, height)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Json.Decode exposing (Decoder)
import WebGL

import Grid exposing (Grid)
import Array
import World exposing (Block)
import Render exposing (uniforms, vertexShader, fragmentShader, meshFromChunk)
import Json.Decode as Decode

-- MAIN -----------------------------------------------------------------------

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL ----------------------------------------------------------------------

type alias Model =
    { chunk : Grid Block
    , dir : Vec2      -- (yaw, pitch)
    , pos : Vec3      -- camera position
    , keys : List String
    }

init : () -> ( Model, Cmd Msg )
init () =
    ( { chunk = fullChunk
      , dir = vec2 0 0
      , pos = vec3 10 0 0
      , keys = []
      }
    , Cmd.none
    )

-- MESSAGES -------------------------------------------------------------------

type Msg
    = TimeDelta Float
    | MouseMove (Float, Float)
    | KeyDown String
    | KeyUp String

-- SUBSCRIPTIONS --------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ E.onAnimationFrameDelta TimeDelta
        , E.onMouseMove mouseDecoder
        , E.onKeyDown (keyDecoder KeyDown)
        , E.onKeyUp (keyDecoder KeyUp)
        ]

-- DECODERS --------------------------------------------------------------------

mouseDecoder : Decoder Msg
mouseDecoder =
    Decode.map2 (\dx dy -> MouseMove (dx, dy))
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)

keyDecoder : (String -> Msg) -> Decoder Msg
keyDecoder wrap =
    Decode.field "key" Decode.string |> Decode.map wrap

-- CAMERA MATH ----------------------------------------------------------------

forwardVector : Vec2 -> Vec3
forwardVector dir =
    let
        yaw = Math.Vector2.getX dir
        pitch = Math.Vector2.getY dir
    in
    vec3
        (cos pitch * cos yaw)
        (sin pitch)
        (cos pitch * sin yaw)


rightVector : Vec2 -> Vec3
rightVector dir =
    let
        yaw = Math.Vector2.getX dir + (pi / 2)
    in
    vec3 (cos yaw) 0 (sin yaw)


scale3 : Float -> Vec3 -> Vec3
scale3 k v =
    vec3 (k * Math.Vector3.getX v)
         (k * Math.Vector3.getY v)
         (k * Math.Vector3.getZ v)


add3 : Vec3 -> Vec3 -> Vec3
add3 a b =
    vec3
        (Math.Vector3.getX a + Math.Vector3.getX b)
        (Math.Vector3.getY a + Math.Vector3.getY b)
        (Math.Vector3.getZ a + Math.Vector3.getZ b)


sub3 : Vec3 -> Vec3 -> Vec3
sub3 a b =
    vec3
        (Math.Vector3.getX a - Math.Vector3.getX b)
        (Math.Vector3.getY a - Math.Vector3.getY b)
        (Math.Vector3.getZ a - Math.Vector3.getZ b)


-- UPDATE ---------------------------------------------------------------------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove ( dx, dy ) ->
            let
                sens = 0.002
                newYaw = Math.Vector2.getX model.dir + dx * sens
                newPitch =
                    clamp (-1.2) 1.2 (Math.Vector2.getY model.dir + dy * sens)
            in
            ( { model | dir = vec2 newYaw newPitch }, Cmd.none )


        KeyDown key ->
            if List.member key model.keys then
                ( model, Cmd.none )
            else
                ( { model | keys = key :: model.keys }, Cmd.none )


        KeyUp key ->
            ( { model | keys = List.filter ((/=) key) model.keys }, Cmd.none )


        TimeDelta dt ->
            let
                speed =
                    if List.member "Shift" model.keys then
                        10
                    else
                        5

                f = scale3 (speed * dt) (forwardVector model.dir)
                r = scale3 (speed * dt) (rightVector model.dir)
                up = scale3 (speed * dt) (vec3 0 1 0)

                pos1 =
                    if List.member "w" model.keys then
                        add3 model.pos f
                    else
                        model.pos

                pos2 =
                    if List.member "s" model.keys then
                        sub3 pos1 f
                    else
                        pos1

                pos3 =
                    if List.member "d" model.keys then
                        add3 pos2 r
                    else
                        pos2

                pos4 =
                    if List.member "a" model.keys then
                        sub3 pos3 r
                    else
                        pos3

                pos5 =
                    if List.member " " model.keys then
                        add3 pos4 up
                    else
                        pos4

                posFinal =
                    if List.member "Control" model.keys then
                        sub3 pos5 up
                    else
                        pos5
            in
            ( { model | pos = posFinal }, Cmd.none )


-- WORLD ----------------------------------------------------------------------

fullBlock : Block
fullBlock =
    { id = 1 }


fullChunk : Grid Block
fullChunk =
    { elements = Array.repeat 2000 fullBlock
    , shape = [ 16, 16, 16 ]
    }


-- VIEW -----------------------------------------------------------------------

view : Model -> Html Msg
view model =
    let
        _ = Debug.log "Model" model
        uniforms_ =
            uniforms model.dir model.pos
    in
    WebGL.toHtml
        [ width 1400
        , height 700
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (meshFromChunk model.chunk)
            uniforms_
        ]
