module ConvexHull.ConvexHull
  where
import           Control.Monad          (unless, when)
import           ConvexHull.CConvexHull
import           ConvexHull.Types
import qualified Data.HashMap.Strict    as H
import qualified Data.IntMap.Strict     as IM
import           Data.List
import           Data.Maybe
import Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc  (free, mallocBytes)
import           Foreign.Marshal.Array  (pokeArray)
import           Foreign.Storable       (peek, sizeOf)
import System.IO (writeFile) -- for rgl
import Data.List.Index (iconcatMap) -- for rgl
import Data.Tuple.Extra (fst3, snd3, thd3) -- for rgl

convexHull :: [[Double]]     -- vertices
           -> Bool           -- triangulate
           -> Bool           -- print output to stdout
           -> Maybe FilePath -- write summary to a file
           -> IO ConvexHull
convexHull points triangulate stdout file = do
  let n     = length points
      dim   = length (head points)
      check = all (== dim) (map length (tail points))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim) $
    error "insufficient number of points"
  pointsPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray pointsPtr (concatMap (map realToFrac) points)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  summaryFile <- maybe (newCString []) newCString file
  resultPtr <- c_convexhull pointsPtr (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum triangulate)
               (fromIntegral $ fromEnum stdout) summaryFile exitcodePtr
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
groupedFaces :: ConvexHull -> [(Maybe Int, [IndexMap [Double]], [EdgeMap])]
groupedFaces hull =
  zip3 (map head families) verticesGroups edgesGroups
  where
    faces          = IM.elems (_faces hull)
    facesGroups    = groupBy (\f1 f2 -> isJust (_family f1) &&
                                        (_family f1 == _family f2)) faces
    edgesGroups    = map (map _edges) facesGroups
    verticesGroups = map (map _fvertices) facesGroups
    families       = map (map _family) facesGroups

-- | group faces of the same family and merge vertices and edges
groupedFaces' :: ConvexHull -> [(Maybe Int, IndexMap [Double], EdgeMap)]
groupedFaces' hull =
  zip3 (map head families) (map (foldr IM.union IM.empty) verticesGroups)
       (map (foldr delta H.empty) edgesGroups)
  where
    faces          = IM.elems (_faces hull)
    facesGroups    = groupBy (\f1 f2 -> isJust (_family f1) &&
                                        (_family f1 == _family f2)) faces
    edgesGroups    = map (map _edges) facesGroups
    verticesGroups = map (map _fvertices) facesGroups
    families       = map (map _family) facesGroups
    delta :: EdgeMap -> EdgeMap -> EdgeMap
    delta e1 e2 = H.difference (H.union e1 e2) (H.intersection e1 e2)


convexHull3DrglCode :: [[Double]] -> FilePath -> IO ()
convexHull3DrglCode points file = do
  -- get edges --
  hull1 <- convexHull points False False Nothing
  let edges = H.elems (_alledges hull1)
  -- get triangles --
  hull2 <- convexHull points True False Nothing
  let families = map _family (IM.elems $ _faces hull2)
  print families
  let grpFaces = groupedFaces hull2
  let triangles = map (map IM.elems . snd3) grpFaces
  mapM_ (mapM_ (print . length)) triangles
  -- code for edges --
  let code1 = concatMap rglSegment edges
  -- color palette --
  let code_colors = "colors <- rainbow(" ++ show (length grpFaces + 1) ++
                    ", alpha=0.5)\n"
  -- code for triangles --
  let code2 = iconcatMap (\i x -> concatMap (rglTriangle i) x) triangles
  -- write file --
  writeFile file (code_colors ++ code1 ++ code2)
  -- auxiliary functions --
  where
    asTriplet p = (p!!0, p!!1, p!!2)
    rglSegment :: ([Double], [Double]) -> String
    rglSegment (p1', p2') =
      let p1 = asTriplet p1' in
      let p2 = asTriplet p2' in
      "segments3d(rbind(c" ++ show p1 ++ ", c" ++ show p2 ++
        "), color=\"black\")\n"
    rglTriangle :: Int -> [[Double]] -> String
    rglTriangle i threepoints =
      "triangles3d(rbind(c" ++ show p1 ++ ", c" ++ show p2 ++
      ", c" ++ show p3 ++ "), color=colors[" ++ show (i+1) ++ "]" ++
      ", alpha=0.75)\n"
      where
        p1 = asTriplet $ threepoints!!0
        p2 = asTriplet $ threepoints!!1
        p3 = asTriplet $ threepoints!!2
  --
  -- let ridges = _ridges tess in
  -- (if colors
  --   then "colors <- topo.colors(" ++ show (length ridges +1) ++ ", alpha=0.5)\n"
  --   else "\n") ++
  -- concatMap rglRidge (if onlyinterior
  --                       then ridges
  --                       else filter (not . sandwichedRidge) ridges)
  -- where
  --   rglRidge :: Ridge -> String
  --   rglRidge ridge =
  --     let i = 1 + head (IS.elems $ _ridgeOf ridge) in
  --     "triangles3d(rbind(c" ++ show (pts!!0) ++
  --     ", c" ++ show (pts!!1) ++
  --     ", c" ++ show (pts!!2) ++
  --     (if colors
  --       then
  --         "), color=colors[" ++ show i ++ "]"
  --       else
  --         "), color=\"blue\"") ++
  --     (if isJust alpha
  --       then ", alpha=" ++ show (fromJust alpha) ++ ")\n"
  --       else ")\n")
  --     ++
  --       -- else "") ++
  --     if segments
  --       then
  --         "segments3d(rbind(c" ++ show (pts!!0) ++
  --         ", c" ++ show (pts!!1) ++
  --         "), color=\"black\")\n" ++
  --         "segments3d(rbind(c" ++ show (pts!!1) ++
  --         ", c" ++ show (pts!!2) ++
  --         "), color=\"black\")\n" ++
  --         "segments3d(rbind(c" ++ show (pts!!2) ++
  --         ", c" ++ show (pts!!0) ++
  --         "), color=\"black\")\n"
  --       else "\n"
  --     where
  --       pts = map (\p -> (p!!0,p!!1,p!!2))
  --                 (IM.elems $ _points $ _subsimplex ridge)
