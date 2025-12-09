module Render exposing (..)

import Grid exposing (Grid)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL
import Random exposing (Generator, list, map)
import Array
import Grid
import World exposing (Block, emptyChunk)
import Math.Vector2 exposing (getX)
import Math.Vector2 exposing (getY)

type alias Uniforms =
  { rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  , light : Vec3
  }

-- Converts yaw/pitch to a forward vector
forwardVector : Vec2 -> Vec3
forwardVector dir =
    let
        yaw = getX dir
        pitch = getY dir
    in
    vec3
        (cos pitch * cos yaw)   -- X
        (sin pitch)             -- Y
        (cos pitch * sin yaw)   -- Z

-- Builds a look-at camera matrix from position + view direction
cameraMatrix : Vec2 -> Vec3 -> Mat4.Mat4
cameraMatrix dir pos =
    let
        fwd = forwardVector dir
        target = Vec3.add pos fwd
        up = vec3 0 1 0
    in
    Mat4.makeLookAt pos target up

uniforms : Vec2 -> Vec3 -> Uniforms
uniforms viewAngle pos = 
    let
        yaw = getX viewAngle
        pitch = getY viewAngle
        fwd = vec3 (cos pitch * cos yaw) (sin pitch) (cos pitch * sin yaw)
        
    in
    { 
      rotation = Mat4.identity,
      perspective = Mat4.makePerspective 50 1 0.01 100 , 
      camera = cameraMatrix viewAngle pos, 
      light = pos
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

setOrDeleteFace : List Int -> Int -> Maybe (Grid Int) -> Maybe (Grid Int)
setOrDeleteFace index value buffer =
    let
      existing = Maybe.withDefault 0 (Grid.getElement index buffer)
    in
      if existing /= 0 then
          Grid.setElement index 0 buffer
      else
          Grid.setElement index value buffer

setFacesFromCube : (Int, Int, Int) -> Maybe (Grid Int) -> Maybe (Grid Int)
setFacesFromCube (x, y, z) buffer =
           setOrDeleteFace [x    , y, z, 0] -1
        <| setOrDeleteFace [x + 1, y, z, 0] 1
        <| setOrDeleteFace [x, y    , z, 1] -1
        <| setOrDeleteFace [x, y + 1, z, 1] 1
        <| setOrDeleteFace [x, y, z    , 2] -1
        <| setOrDeleteFace [x, y, z + 1, 2] 1
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
trianglesFromFace : (Int, Int, Int) -> Int -> Int -> List (Vertex, Vertex, Vertex)
trianglesFromFace (x, y, z) dir value =
    let
      p000 = vec3 (toFloat x)     (toFloat y)     (toFloat z)
      p100 = vec3 (toFloat (x+1)) (toFloat y)     (toFloat z)
      p010 = vec3 (toFloat x)     (toFloat (y+1)) (toFloat z)
      p110 = vec3 (toFloat (x+1)) (toFloat (y+1)) (toFloat z)
      p001 = vec3 (toFloat x)     (toFloat y)     (toFloat (z+1))
      p101 = vec3 (toFloat (x+1)) (toFloat y)     (toFloat (z+1))
      p011 = vec3 (toFloat x)     (toFloat (y+1)) (toFloat (z+1))
      preNormal =
        case dir of
            0 -> vec3 1 0 0
            1 -> vec3 0 1 0
            2 -> vec3 0 0 1
            _ -> vec3 0 0 0
      normal =
        if value < 0 then
          Vec3.negate preNormal
        else
          preNormal
      vertex pos =
        Vertex (vec3 0.5 0.5 0.5) pos normal
   in
    case dir of
        0 -> [ ( vertex p000, vertex p001, vertex p010 ), ( vertex p001, vertex p010, vertex p011 ) ] -- X normal
        1 -> [ ( vertex p000, vertex p001, vertex p100 ), ( vertex p001, vertex p100, vertex p101 ) ] -- Y normal
        2 -> [ ( vertex p000, vertex p010, vertex p100 ), ( vertex p010, vertex p100, vertex p110 ) ] -- Z normal
        _ -> []
-- mesh generator
listFromChunk : Grid Block -> (Int, Int, Int) -> List (Vertex, Vertex, Vertex)
listFromChunk chunk (dx, dy, dz) =
    let
        buffer = Maybe.withDefault emptyFaceBuffer (setFacesFromChunk chunk)

        folder : List Int -> Int -> List (Vertex, Vertex, Vertex) -> List (Vertex, Vertex, Vertex)
        folder coords value acc =
            if value /= 0 then
                let
                    tris =
                      case coords of
                        [ x, y, z, dir ] ->
                          trianglesFromFace (x + dx, y + dy, z + dz) dir value
                        _ ->
                          []
                in
                List.append tris acc
            else
                acc
    in
    Grid.fold folder [] buffer
listFromWorld : Grid (Grid Block) -> List (Vertex, Vertex, Vertex)
listFromWorld world =
     let
        folder : List Int -> Grid Block -> List (Vertex, Vertex, Vertex) -> List (Vertex, Vertex, Vertex)
        folder coords value acc =
                let
                    tris =
                      case coords of
                        [ x, y, z ] ->
                          let chunk = Maybe.withDefault emptyChunk (Grid.getElement [x, y, z] (Just world))
                          in
                          listFromChunk chunk (x * 16, y * 16, z * 16)
                        _ ->
                          []
                in
                List.append tris acc
    in
    Grid.fold folder [] world

meshFromChunk : Grid Block -> WebGL.Mesh Vertex
meshFromChunk chunk =
    WebGL.triangles (listFromChunk chunk (0, 0, 0))

meshFromWorld : Grid (Grid Block) -> WebGL.Mesh Vertex
meshFromWorld world =
    WebGL.triangles (listFromWorld world)

randomBlock : Generator Block
randomBlock =
    Random.int 0 1
        |> map (\v -> { id = v })
-- CUBE MESH (RED CUBE)
randomChunkGenerator : Generator (Grid Block)
randomChunkGenerator =
    list (16*16*16) randomBlock
        |> map (\lst -> { elements = Array.fromList lst, shape = [16,16,16] })

-- Returns (targetVoxel, previousVoxel) or Nothing if no hit
raycastVoxel : Vec3 -> Vec3 -> Float -> (List Int -> Bool)
    -> Maybe ( List Int, List Int )
raycastVoxel origin direction reach isSolid  =
    let
        -- Extract floats
        ox = Vec3.getX origin
        oy = Vec3.getY origin
        oz = Vec3.getZ origin

        dirRawX = Vec3.getX direction
        dirRawY = Vec3.getY direction
        dirRawZ = Vec3.getZ direction

        -- Normalize direction
        mag =
            sqrt (dirRawX ^ 2 + dirRawY ^ 2 + dirRawZ ^ 2)

        dirX = dirRawX / mag
        dirY = dirRawY / mag
        dirZ = dirRawZ / mag

        -- Convert float → voxel index list
        voxelFrom x y z =
            [ floor x, floor y, floor z ]

        -- Update a 3-vector (List Int)
        setX dx voxel =
          case voxel of
              [ x, y, z ] ->
                  [ x + dx, y, z ]

              _ ->
                  voxel -- invalid list, do nothing

        setY dy voxel =
          case voxel of
              [ x, y, z ] ->
                  [ x, y + dy, z ]

              _ ->
                  voxel

        setZ dz voxel =
          case voxel of
              [ x, y, z ] ->
                  [ x, y, z + dz ]

              _ ->
                  voxel


        -- Distance to next voxel boundary
        boundaryDistance originCoord dirComp step =
            if dirComp == 0 then
                1/0
            else
                let
                    nextBoundary =
                        if step > 0 then
                            toFloat (floor originCoord + 1)
                        else
                            toFloat (ceiling originCoord - 1)
                in
                (nextBoundary - originCoord) / dirComp

        -- Starting voxel
        startVoxel =
            voxelFrom ox oy oz

        -- DDA step direction
        stepX = if dirX >= 0 then 1 else -1
        stepY = if dirY >= 0 then 1 else -1
        stepZ = if dirZ >= 0 then 1 else -1

        -- Initial distance to first boundary
        tMax0X = boundaryDistance ox dirX stepX
        tMax0Y = boundaryDistance oy dirY stepY
        tMax0Z = boundaryDistance oz dirZ stepZ

        -- Distance between crossings
        tDeltaX = if dirX == 0 then 1/0 else abs (1 / dirX)
        tDeltaY = if dirY == 0 then 1/0 else abs (1 / dirY)
        tDeltaZ = if dirZ == 0 then 1/0 else abs (1 / dirZ)

        -- Recursive march function
        march pos prev tx ty tz =
            if isSolid pos then
                Just ( pos, prev )

            else
                let
                    nextT = min tx (min ty tz)
                in
                if nextT > reach then
                    Nothing

                else if tx < ty && tx < tz then
                    -- Step X
                    let
                        newPos = setX stepX pos
                    in
                    march newPos pos (tx + tDeltaX) ty tz

                else if ty < tz then
                    -- Step Y
                    let
                        newPos = setY stepY pos
                    in
                    march newPos pos tx (ty + tDeltaY) tz

                else
                    -- Step Z
                    let
                        newPos = setZ stepZ pos
                    in
                    march newPos pos tx ty (tz + tDeltaZ)
    in
    march startVoxel startVoxel tMax0X tMax0Y tMax0Z


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