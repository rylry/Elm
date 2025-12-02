module Render exposing (..)
import Grid exposing (Grid)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Random exposing (Generator, list, map)
import Array
import Grid exposing (foldGrid)
import World exposing (Block)

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
  , perspective = Mat4.makePerspective 50 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 7 7 20) (vec3 0 0 0) (vec3 0 1 0)
  , light = vec3 0 2 2
  }

-- VERTEX FORMAT

type alias Vertex =
  { color : Vec3
  , position : Vec3
  , normal : Vec3
  }

-- FACE BUFFER

-- flat array: 16*16*17 per channel, 3 channels for X/Y/Z
emptyFaceBuffer : Grid Int
emptyFaceBuffer =
    { elements = Array.repeat (17 * 17 * 17 * 3) 0
    , shape = [17, 17, 17, 3]
    }


setFacesFromCube : (Int, Int, Int) -> Maybe (Grid Int) -> Maybe (Grid Int)
setFacesFromCube (x, y, z) buffer =
           Grid.setElement [x    , y, z, 0] 1
        <| Grid.setElement [x + 1, y, z, 0] 1
        <| Grid.setElement [x, y    , z, 1] 1
        <| Grid.setElement [x, y + 1, z, 1] 1
        <| Grid.setElement [x, y, z    , 2] 1
        <| Grid.setElement [x, y, z + 1, 2] 1
        <| buffer

setFacesFromChunk : (Grid Block) -> Maybe (Grid Int)
setFacesFromChunk chunk =
    let
        coordsFromIndex i =
            let
                x = modBy 16 i
                y = modBy 16 (i // 16)
                z = i // (16 * 16)
            in
            (x, y, z)

        allIndexedBlocks = Array.indexedMap Tuple.pair chunk.elements

        folder (i, block) buffer =
            if block.id /= 0 then
                let pos = coordsFromIndex i in
                setFacesFromCube pos buffer
            else
                buffer
    in -- Direction 0 = -X
    Array.foldl folder (Just emptyFaceBuffer) allIndexedBlocks -- Return faces buffer

-- MESH FROM FACES
trianglesFromFace : (Int, Int, Int) -> Int -> List (Vertex, Vertex, Vertex)
trianglesFromFace (x, y, z) dir =
    let
      p000 = vec3 (toFloat x)     (toFloat y)     (toFloat z)
      p100 = vec3 (toFloat (x+1)) (toFloat y)     (toFloat z)
      p010 = vec3 (toFloat x)     (toFloat (y+1)) (toFloat z)
      p110 = vec3 (toFloat (x+1)) (toFloat (y+1)) (toFloat z)
      p001 = vec3 (toFloat x)     (toFloat y)     (toFloat (z+1))
      p101 = vec3 (toFloat (x+1)) (toFloat y)     (toFloat (z+1))
      p011 = vec3 (toFloat x)     (toFloat (y+1)) (toFloat (z+1))
      normal =
        case dir of
            0 -> vec3 1 0 0
            1 -> vec3 0 1 0
            2 -> vec3 0 0 1
            _ -> vec3 0 0 0
      vertex pos =
        Vertex (vec3 0.5 0.5 0.5) pos normal
   in
    case dir of
        0 -> [ ( vertex p000, vertex p001, vertex p010 ), ( vertex p001, vertex p010, vertex p011 ) ] -- X normal
        1 -> [ ( vertex p000, vertex p001, vertex p100 ), ( vertex p001, vertex p100, vertex p101 ) ] -- Y normal
        2 -> [ ( vertex p000, vertex p010, vertex p100 ), ( vertex p010, vertex p100, vertex p110 ) ] -- Z normal
        _ -> []
-- mesh generator
listFromChunk : Grid Block -> List (Vertex, Vertex, Vertex)
listFromChunk chunk =
    let
        buffer = Maybe.withDefault emptyFaceBuffer (setFacesFromChunk chunk)

        folder : List Int -> Int -> List (Vertex, Vertex, Vertex) -> List (Vertex, Vertex, Vertex)
        folder coords value acc =
            if value /= 0 then
                let
                    tris =
                      case coords of
                        [ x, y, z, dir ] ->
                          trianglesFromFace (x, y, z) dir
                        _ ->
                          []
                in
                List.append tris acc
            else
                acc
    in
    foldGrid folder [] buffer
meshFromChunk : Grid Block -> WebGL.Mesh Vertex
meshFromChunk chunk =
    WebGL.triangles (listFromChunk chunk)

randomBlock : Generator Block
randomBlock =
    Random.int 0 1
        |> map (\v -> { id = v })
-- CUBE MESH (RED CUBE)
randomChunkGenerator : Generator (Grid Block)
randomChunkGenerator =
    list (16*16*16) randomBlock
        |> map (\lst -> { elements = Array.fromList lst, shape = [16,16,16] })


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
lightCubeMesh = WebGL.triangles <| []

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