module World exposing (..)
import Array
import Grid exposing (Grid)

type alias Block = 
    { id : Int
    }

type alias Chunk = Grid Block
type alias World = Grid Chunk
fullBlock : Block
fullBlock =
    { id = 1 }
emptyBlock : Block
emptyBlock =
    { id = 0 }

fullChunk : Chunk
fullChunk =
    { elements = Array.repeat (16 * 16 * 16) fullBlock
    , shape = [ 16, 16, 16 ]
    }

emptyChunk : Chunk
emptyChunk =
    { elements = Array.repeat (16 * 16 * 16) emptyBlock
    , shape = [ 16, 16, 16 ]
    }

fromHeightMap : Grid Int -> (Int, Int, Int) -> Chunk
fromHeightMap heightMap (dx, dy, dz) =
    let
        folder: List Int -> Block -> Chunk -> Chunk
        folder coords value acc =
                let
                    operation =
                      case coords of
                        [ x, y, z ] ->
                            if (y + dy) < Maybe.withDefault 0 (Grid.getElement [x + dx, z + dz] (Just heightMap)) then
                                Grid.setElement coords fullBlock
                            else
                                Grid.setElement coords emptyBlock
                        _ ->
                            identity
                in
                Maybe.withDefault acc (operation (Just acc))
    in
    Grid.fold folder emptyChunk emptyChunk

worldFromHeightMap : Grid Int -> World
worldFromHeightMap heightMap =
    let
        worldShape =
            case heightMap.shape of
                [ x, z ] ->
                    [ x // 16 + 1, 1, z // 16 + 1 ]
                _ ->
                    [1, 1, 1]
        emptyWorld =
            { elements = Array.repeat (List.product worldShape) emptyChunk
            , shape = worldShape
            }
        folder : List Int -> Chunk -> World -> World 
        folder coords value acc =
                      case coords of
                        [ x, y, z ] ->
                            let
                                chunk = fromHeightMap heightMap (x * 16, y * 16, z * 16)
                                world = Grid.setElement coords chunk (Just acc)
                            in
                            Maybe.withDefault acc world
                          
                        _ ->
                          emptyWorld
    in
    Grid.fold folder emptyWorld emptyWorld
        



getBlock : (Int, Int, Int) -> World -> Block
getBlock (x, y, z) world = 
    let
      chunkPos = 
          [ x // 16, y // 16, z // 16 ]
      localPos = 
          [ modBy 16 x, modBy 16 y, modBy 16 z ]
      chunk = 
          Maybe.withDefault emptyChunk (Grid.getElement chunkPos (Just world))
      block = 
          Maybe.withDefault emptyBlock (Grid.getElement localPos (Just chunk))
    in
    block
setBlock : (Int, Int, Int) -> Block -> World -> World
setBlock (x, y, z) block world =
    let
      chunkPos = 
          [ x // 16, y // 16, z // 16 ]
      localPos = 
          [ modBy 16 x, modBy 16 y, modBy 16 z ]
      chunk = 
          Maybe.withDefault emptyChunk (Grid.getElement chunkPos (Just world))
      newChunk =
          Maybe.withDefault chunk (Grid.setElement localPos block (Just chunk))
      updatedWorld = 
          Maybe.withDefault world (Grid.setElement chunkPos newChunk (Just world))
    in
    updatedWorld