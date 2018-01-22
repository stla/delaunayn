module ConvexHull.ConvexHull
  where
import           Control.Monad          (unless, when)
import           ConvexHull.CConvexHull
import           ConvexHull.Types
import qualified Data.IntMap.Strict     as IM
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

-- | get vertices of a convex hull
hullVertices :: ConvexHull -> [[Double]]
hullVertices chull = map _point (IM.elems (_allvertices chull))

-- -- | get ridges of a convex hull
-- hullEdges :: ConvexHull -> [([Double], [Double])]
-- hullEdges chull = map ((\x -> (x!!0, x!!1)) . IM.elems)
--                       (IM.elems (_allridges chull))
xxx :: ConvexHull -> [[Int]]
xxx chull = map (IM.keys . _rvertices) (IM.elems (_allridges chull))

-- | get vertices of a face
faceVertices :: Face -> [[Double]]
faceVertices = IM.elems . _fvertices

-- -- | get ridges of a face
-- faceEdges :: Face -> [([Double], [Double])]
-- faceEdges face = map ((\x -> (x!!0, x!!1)) . IM.elems) (_ridges face)
--
-- -- | whether a pair of vertices form an ridge
-- isEdge :: ConvexHull -> (Index, Index) -> Bool
-- isEdge hull ridge = (ridge `elem` ridges) || (swap ridge `elem` ridges)
--   where
--     ridges = map ((\x -> (x!!0, x!!1)) . IM.keys) (IM.elems (_allridges hull))
--
-- -- | get faces ids an ridge belongs to
-- ridgeOf :: ConvexHull -> (Index, Index) -> Maybe [Int] -- c'est dans ridge->top et ridge->bottom
-- ridgeOf hull v1v2@(v1, v2) =
--   if not (isEdge hull v1v2)
--     then Nothing
--     else Just $ IM.keys (IM.filter (elem v1v2') facesEdges)
--   where
--     ridgeIds :: Edge -> (Index, Index)
--     ridgeIds ridge = (\x -> (x!!0, x!!1)) (IM.keys ridge)
--     facesEdges = IM.map (map ridgeIds . _ridges) (_faces hull)
--     v1v2' = if v1<v2 then v1v2 else (v2,v1)
