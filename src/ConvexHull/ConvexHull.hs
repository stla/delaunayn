module ConvexHull.ConvexHull
  where
import           Control.Monad          (when, unless)
import           ConvexHull.CConvexHull
import qualified Data.IntMap.Strict     as IM
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

-- | get edges of a convex hull
hullEdges :: ConvexHull -> [([Double], [Double])]
hullEdges chull = map ((\x -> (x!!0, x!!1)) . IM.elems)
                      (IM.elems (_alledges chull))

-- | get vertices of a face
faceVertices :: Face -> [[Double]]
faceVertices = IM.elems . _fvertices

-- | get edges of a face
faceEdges :: Face -> [([Double], [Double])]
faceEdges face = map ((\x -> (x!!0, x!!1)) . IM.elems) (_edges face)
