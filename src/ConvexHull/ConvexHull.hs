module ConvexHull.ConvexHull
  where
import           Control.Arrow          ((***))
import           Control.Monad          (unless, when)
import           ConvexHull.CConvexHull
import           ConvexHull.Types
import qualified Data.IntMap.Strict     as IM
import           Data.List
import qualified Data.HashMap.Strict    as H
import Data.Maybe
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Tuple             (swap)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Marshal.Array  (pokeArray)
import           Foreign.Storable       (peek, sizeOf)

convexHull :: [[Double]] -> Bool -> IO ConvexHull
convexHull points triangulate = do
  let n     = length points
      dim   = length (head points)
      check = all (== dim) (map length (tail points))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  pointsPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray pointsPtr (concatMap (map realToFrac) points)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_convexhull pointsPtr
               (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum triangulate) exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free pointsPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- peekConvexHull resultPtr
      free resultPtr
      return result

-- | whether a pair of vertices form an edge
isEdge :: ConvexHull -> (Index, Index) -> Bool
isEdge hull (i,j) = Pair i j `H.member` _alledges hull

-- | edge as pair of points
toPoints :: ConvexHull -> (Index, Index) -> Maybe ([Double], [Double])
toPoints hull (i,j) = H.lookup (Pair i j) (_alledges hull)

-- | edge as pair of points, no check
toPoints' :: ConvexHull -> (Index, Index) -> ([Double], [Double])
toPoints' hull (i,j) = (H.!) (_alledges hull) (Pair i j)

-- | get vertices of a convex hull
hullVertices :: ConvexHull -> [[Double]]
hullVertices hull = map _point (IM.elems (_allvertices hull))

-- -- | get ridges of a convex hull
-- hullEdges :: ConvexHull -> [([Double], [Double])]
-- hullEdges chull = map ((\x -> (x!!0, x!!1)) . IM.elems)
--                       (IM.elems (_allridges chull))
xxx :: ConvexHull -> [[Int]]
xxx chull = map (IM.keys . _rvertices) (IM.elems (_allridges chull))

-- | get vertices of a face
faceVertices :: Face -> [[Double]]
faceVertices = IM.elems . _fvertices

-- -- | get edges of a face as a map ; now it is in _edges face
-- faceEdges :: ConvexHull -> Face -> EdgeMap
-- faceEdges hull face = H.filterWithKey (\x _ -> x `elem` pairs) (_alledges hull)
--   where
--     pairs = [Pair i j | i <- faceVerticesIndices,
--                         j <- faceVerticesIndices, j > i]
--     faceVerticesIndices = IM.keys (_fvertices face)


-- | get faces ids an edge belongs to
edgeOf :: ConvexHull -> (Index, Index) -> Maybe [Int]
edgeOf hull v1v2@(v1, v2) =
  if not (isEdge hull v1v2)
    then Nothing
    else Just $ IM.keys (IM.filter (elem v1v2') facesEdges)
  where
    facesEdges = IM.map (H.keys . _edges) (_faces hull)
    v1v2' = if v1<v2 then Pair v1 v2 else Pair v2 v1

-- | group faces of the same family
groupedFaces :: ConvexHull -> [(Maybe Int, IndexMap [Double], EdgeMap)]
groupedFaces hull =
  zip3 (map head families) (map (foldr IM.union IM.empty) verticesGroups) (map (foldr delta H.empty) edgesGroups)
  where
    faces          = IM.elems (_faces hull)
    facesGroups    = groupBy (\f1 f2 -> isJust (_family f1) && (_family f1 == _family f2)) faces
    edgesGroups    = map (map _edges) facesGroups
    verticesGroups = map (map _fvertices) facesGroups
    families       = map (map _family) facesGroups
    delta :: EdgeMap -> EdgeMap -> EdgeMap
    delta e1 e2 = H.difference (H.union e1 e2) (H.intersection e1 e2)
