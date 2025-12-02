module Grid exposing (
    Grid,
    Block,
    BlockPos,
    getReference,
    setElement,
    iterGrid,
    foldGrid)

import Array exposing (Array)

-- VOXELS
type alias BlockPos = (Int, Int, Int)

type alias Block = 
    { id : Int
    }

type alias Grid a = 
    { elements : Array a,
      shape: List Int
    }

getReference : Grid a -> List Int -> Maybe Int
getReference grid indices = 
    let
        zipped = 
            List.map2 
                Tuple.pair
                indices 
                grid.shape
    in
    Maybe.map Tuple.first <|
        if List.length indices /= List.length grid.shape then
                Nothing
            else
                List.foldl 
                    (\(i, dim) acc ->
                        case acc of
                            Just (idx, mult) -> 
                                if i < 0 || i >= dim then
                                    Nothing
                                else
                                    Just (idx + i * mult, mult * dim)
                            _ ->
                                Nothing
                    )
                    (Just (0, 1))
                    zipped

setElement : List Int -> a -> Maybe (Grid a) -> Maybe (Grid a)
setElement indices newElement grid = 
    Maybe.andThen
        (\gridd ->
            let
                index = getReference gridd indices
            in
            Maybe.map 
                (\i -> 
                    { gridd | elements = Array.set i newElement gridd.elements }
                )
                index
        )
        grid



-- Iterator for Grid

-- cartesianProduct [[0,1],[0,1,2]] => [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]
cartesianProduct : List (List a) -> List (List a)
cartesianProduct lists =
    case lists of
        [] ->
            [[]]
        xs :: rest ->
            let
                tail = cartesianProduct rest
            in
            List.concatMap (\x -> List.map (\t -> x :: t) tail) xs

-- iterate ranges 0..dim-1 for each dimension
rangesFromShape : List Int -> List (List Int)
rangesFromShape shape =
    List.map (\dim -> if dim <= 0 then [] else List.range 0 (dim - 1)) shape

-- produce list of (coords, value) for every valid coordinate in the grid
iterGrid : Grid a -> List (List Int, a)
iterGrid grid =
    let
        ranges = rangesFromShape grid.shape
        coordsList = cartesianProduct ranges
        gather coords =
            case getReference grid coords of
                Just idx ->
                    case Array.get idx grid.elements of
                        Just v -> [(coords, v)]
                        Nothing -> []
                Nothing -> []
    in
    List.concatMap gather coordsList

-- fold over grid coordinates and values
foldGrid : (List Int -> a -> b -> b) -> b -> Grid a -> b
foldGrid f init grid =
    List.foldl (\(coords, v) acc -> f coords v acc) init (iterGrid grid)
