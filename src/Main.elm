module Main exposing (main)

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  Float

init : () -> (Model, Cmd Msg)
init () =
  ( 0, Cmd.none )

type Msg
  = TimeDelta Float

update : Msg -> Model -> (Model, Cmd Msg)
update msg angle =
  case msg of
    TimeDelta dt ->
      ( angle + dt / 1000, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta

view : Model -> Html Msg
view angle =
  let
    uniforms_ = uniforms angle
  in
  WebGL.toHtml
    [ width 700, height 700, style "display" "block" ]
    [ WebGL.entity vertexShader fragmentShader cubeMesh uniforms_
    , WebGL.entity lightVertexShader lightFragmentShader lightCubeMesh uniforms_
    ]

-- UNIFORMS

type alias Uniforms =
  { rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  , light : Vec3
  }

uniforms : Float -> Uniforms
uniforms angle =
  { rotation =
      Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
  , perspective = Mat4.makePerspective 45 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  , light = vec3 0 2 2
  }

-- VERTEX FORMAT

type alias Vertex =
  { color : Vec3
  , position : Vec3
  , normal : Vec3
  }

-- CUBE MESH (RED CUBE)

cubeMesh : WebGL.Mesh Vertex
cubeMesh =
  let
    rft = vec3 1 1 1
    lft = vec3 -1 1 1
    lbt = vec3 -1 -1 1
    rbt = vec3 1 -1 1
    rbb = vec3 1 -1 -1
    rfb = vec3 1 1 -1
    lfb = vec3 -1 1 -1
    lbb = vec3 -1 -1 -1

    red = vec3 255 255 255
  in
  WebGL.triangles <| List.concat <|
    [ face red rft rfb rbb rbt
    , face red rft rfb lfb lft
    , face red rft lft lbt rbt
    , face red rfb lfb lbb rbb
    , face red lft lfb lbb lbt
    , face red rbt rbb lbb lbt
    ]

-- FACE: compute normal, force outward relative to cube center
face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
  let
    -- raw normal from triangle winding
    raw =
      Vec3.normalize (Vec3.cross (Vec3.sub b a) (Vec3.sub c a))

    -- centroid of the quad (approx center of this face)
    centroid =
      Vec3.scale (1 / 4) (Vec3.add a (Vec3.add b (Vec3.add c d)))

    -- ensure normal points outward (away from cube center at origin)
    n =
      if Vec3.dot raw centroid < 0 then
        Vec3.scale -1 raw
      else
        raw

    vertex position =
      Vertex (Vec3.scale (1 / 255) color) position n
  in
  [ ( vertex a, vertex b, vertex c )
  , ( vertex c, vertex d, vertex a )
  ]

-- LIGHT CUBE (SMALL MARKER)

lightCubeMesh : WebGL.Mesh Vertex
lightCubeMesh =
  let
    s = 0.35
    rft = vec3 s s s
    lft = vec3 -s s s
    lbt = vec3 -s -s s
    rbt = vec3 s -s s
    rbb = vec3 s -s -s
    rfb = vec3 s s -s
    lfb = vec3 -s s -s
    lbb = vec3 -s -s -s

    yellow = vec3 255 255 0
  in
  WebGL.triangles <| List.concat <|
    [ face yellow rft rfb rbb rbt
    , face yellow rft rfb lfb lft
    , face yellow rft lft lbt rbt
    , face yellow rfb lfb lbb rbb
    , face yellow lft lfb lbb lbt
    , face yellow rbt rbb lbb lbt
    ]

-- SHADERS

vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3, vnormal : Vec3, vpos : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    attribute vec3 normal;

    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;

    varying vec3 vcolor;
    varying vec3 vnormal;
    varying vec3 vpos;

    void main () {
        vec4 worldPos = rotation * vec4(position, 1.0);
        gl_Position = perspective * camera * worldPos;
        vcolor = color;
        vnormal = mat3(rotation) * normal;
        vpos = worldPos.xyz;
    }
  |]

-- FRAGMENT: simple Phong, specular scaled by diffuse so it won't show on back faces.
fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3, vnormal : Vec3, vpos : Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;

    uniform vec3 light;
    varying vec3 vcolor;
    varying vec3 vnormal;
    varying vec3 vpos;

    void main () {
        vec3 n = normalize(vnormal);
        vec3 l = normalize(light - vpos);
        vec3 v = normalize(-vpos);
        vec3 r = reflect(-l, n);

        float diff = max(dot(n, l), 0.0);

        // multiply spec by diff to prevent highlights on faces that face away
        float spec = pow(max(-dot(r, v), 0.0), 16.0);

        vec3 ambient = 0.25 * vcolor;
        vec3 diffuse = diff * vcolor;
        vec3 specular = spec * vec3(1.0);

        vec3 color = ambient + diffuse + specular;
        gl_FragColor = vec4(color, 1.0);
    }
  |]

-- Light cube shaders (no lighting, positioned by uniform light)
lightVertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
lightVertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;

    uniform mat4 perspective;
    uniform mat4 camera;
    uniform vec3 light;

    varying vec3 vcolor;

    void main () {
        vec4 worldPos = vec4(light + position, 1.0);
        gl_Position = perspective * camera * worldPos;
        vcolor = color;
    }
  |]

lightFragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
lightFragmentShader =
  [glsl|
    precision mediump float;
    varying vec3 vcolor;
    void main () {
        gl_FragColor = vec4(vcolor, 1.0);
    }
  |]