port module Main exposing (main)

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
import World exposing (Block, World)
import Render exposing (uniforms, vertexShader, fragmentShader, meshFromWorld)
import Json.Decode as Decode
import Render exposing (raycastVoxel)
import World exposing (worldFromHeightMap)
import Render exposing (meshFromChunk)
import Random

port requestPointerLock : () -> Cmd msg

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
    { world : World
    , dir : Vec2      -- (yaw, pitch)
    , pos : Vec3      -- camera position
    , keys : List String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { world = worldFromHeightMap {elements = Array.repeat (100 * 5) 2, shape = [100, 5] }
      , dir = vec2 0 0
      , pos = vec3 0 0 0
      , keys = []
      }
    , Random.generate GotHeightMap (Random.list (mapSize) (Random.int 1 4))
    )

-- MESSAGES -------------------------------------------------------------------

type Msg
    = TimeDelta Float
    | MouseMove (Float, Float)
    | KeyDown String
    | KeyUp String
    | GotHeightMap (List Int)

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
        cp = cos pitch
    in
    vec3 (cp * cos yaw) (sin pitch) (cp * sin yaw)

forwardVectorFlattened : Vec2 -> Vec3
forwardVectorFlattened dir =
    let
        yaw = Math.Vector2.getX dir
        pitch = Math.Vector2.getY dir
    in
    vec3 (cos yaw) 0 (sin yaw)



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
        GotHeightMap heightList ->
            let

                heightMap =
                    { elements = Array.fromList heightList
                    , shape = mapShape
                    }
                newWorld = worldFromHeightMap heightMap
            in
            ( { model | world = newWorld }, Cmd.none )
        MouseMove ( dx, dy ) ->
            let
                sens = 0.002
                newYaw = Math.Vector2.getX model.dir + dx * sens
                newPitch = clamp (-1.2) 1.2 (Math.Vector2.getY model.dir - dy * sens)
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
                    if List.member "Control" model.keys then
                        0.01
                    else
                        0.005

                f = scale3 (speed * dt) (forwardVectorFlattened model.dir)
                r = scale3 (speed * dt) (rightVector model.dir)
                up = scale3 (speed * dt) (vec3 0 1 0)

                withBrokenBlock = 
                    let
                        _ = Debug.log "Removing block"
                        reach = 60
                        looking = forwardVector model.dir
                        isSolid = \coords ->
                            case coords of 
                              [x, y, z] ->
                                 let
                                     block = World.getBlock (x, y, z) model.world
                                 in
                                 block.id /= 0
                              _ ->
                                False
                    in
                    case raycastVoxel model.pos looking reach isSolid of
                        Just ( [x, y, z], face ) ->
                            let
                                coords = [x, y, z]
                                air = { id = 0 }
                            in
                            { model | world = (World.setBlock (x, y, z) air model.world) }
                        _ ->
                            model

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
                    if List.member "Shift" model.keys then
                        sub3 pos5 up
                    else
                        pos5
                modelTranslated = 
                    if List.member "r" model.keys then
                        {withBrokenBlock | pos = posFinal }
                    else
                        {model | pos = posFinal }
                
            in
            ( modelTranslated, Cmd.none )


-- WORLD ----------------------------------------------------------------------
sizeX : Int
sizeX = 35

sizeZ : Int
sizeZ = 35

mapShape : List Int
mapShape = [ sizeX, sizeZ ]

mapSize : Int
mapSize = sizeX * sizeZ

fullBlock : Block
fullBlock =
    { id = 1 }


fullChunk : Grid Block
fullChunk =
    { elements = Array.repeat (16 * 16 * 16) fullBlock
    , shape = [ 16, 16, 16 ]
    }


-- VIEW -----------------------------------------------------------------------

view : Model -> Html Msg
view model =
    let
        uniforms_ =
            uniforms model.dir model.pos
    in
    WebGL.toHtml
        [ width 700
        , height 700
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (meshFromWorld model.world)
            uniforms_
        ]
