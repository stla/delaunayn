module Delaunay.Delaunay
  where
import           Control.Monad         (when, unless)
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Delaunay.CDelaunay
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)
import           System.IO             (readFile)
import           TemporaryFile


delaunay :: [[Double]] -> Bool -> IO Delaunay
delaunay sites deg = do
  let n     = length sites
      dim   = length (head sites)
      check = all (== dim) (map length (tail sites))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  sitesPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray sitesPtr (concatMap (map realToFrac) sites)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  tmpFile <- getTemporaryFile "tmp.txt"
  tmpFile' <- newCString tmpFile
  resultPtr <- c_delaunay sitesPtr
               (fromIntegral dim) (fromIntegral n) (fromIntegral $ fromEnum deg)
               exitcodePtr tmpFile'
  (>>=) (readFile tmpFile) putStrLn -- print summary
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free sitesPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      out <- cDelaunayPtrToDelaunay resultPtr sites
      free resultPtr
      return out

-- | vertices ids of a ridge
ridgeVertices :: Ridge -> IndexSet
ridgeVertices = IS.fromAscList . IM.keys . _points . _subsimplex

-- | the ridges a vertex belongs to
vertexNeighborRidges :: Delaunay -> Index -> [Ridge]
vertexNeighborRidges tess i =
  filter (\r -> ridgeVertices r `elem` _neighRidges (_sites tess IM.! i))
         (_ridges tess)

-- | whether a ridge is sandwiched between two facets
sandwichedRidge :: Ridge -> Bool
sandwichedRidge ridge = IS.size (_ridgeOf ridge) == 2

-- | the ridges as a map
ridgesMap :: Delaunay -> Map IndexSet Ridge -- not used now
ridgesMap tess = M.fromList ridges'
  where
    ridges' :: [(IndexSet, Ridge)]
    ridges' =  map (\ridge -> let vertices = ridgeVertices ridge in
                                  (vertices, ridge))
                   (_ridges tess)


-- _facetVertices :: Facet -> IndexSet
-- _facetVertices = IS.fromAscList . IM.keys . _points . _simplex
--
-- allFacets :: Delaunay -> [IndexSet]
-- allFacets d = map _facetVertices (IM.elems $ _facets d)

-- R code to plot a 2D Delaunay tesselation
delaunay2ForR :: Delaunay -> Bool -> String
delaunay2ForR tess colors =
  let facets = IM.elems (_facets tess) in
  (if colors
    then "colors <- heat.colors(" ++ show (length facets +1) ++ ", alpha=0.5)\n"
    else "\n") ++
  concatMap triangle (zip [1 .. length facets] facets)
  where
    triangle :: (Int, Facet) -> String
    triangle (i, facet) =
      let pts = map (\p -> [p!!0,p!!1,p!!2])
                (IM.elems $ _points $ _simplex facet)
      in
      "polygon(c(" ++ show (pts!!0!!0) ++ ", " ++ show (pts!!1!!0) ++
                      ", " ++ show (pts!!2!!0) ++ "), "
                   ++ "c(" ++ show (pts!!0!!1) ++ ", " ++ show (pts!!1!!1) ++
                      ", " ++ show (pts!!2!!1) ++
                   "), border=\"black\", " ++
                   (if colors
                     then "col=colors[" ++ show i ++ "])\n"
                     else "col=\"lightblue\")\n")

-- R code to plot a 3D Delaunay tesselation
delaunay3rgl :: Delaunay -> Bool -> Bool -> Bool -> Maybe Double -> String
delaunay3rgl tess onlyinterior segments colors alpha =
  let ridges = _ridges tess in
  (if colors
    then "colors <- topo.colors(" ++ show (length ridges +1) ++ ", alpha=0.5)\n"
    else "\n") ++
  concatMap rglRidge (if onlyinterior
                        then ridges
                        else filter (not . sandwichedRidge) ridges)
  where
    rglRidge :: Ridge -> String
    rglRidge ridge =
      let i = 1 + head (IS.elems $ _ridgeOf ridge) in
      "triangles3d(rbind(c" ++ show (pts!!0) ++
      ", c" ++ show (pts!!1) ++
      ", c" ++ show (pts!!2) ++
      (if colors
        then
          "), color=colors[" ++ show i ++ "]"
        else
          "), color=\"blue\"") ++
      (if isJust alpha
        then ", alpha=" ++ show (fromJust alpha) ++ ")\n"
        else ")\n")
      ++
        -- else "") ++
      if segments
        then
          "segments3d(rbind(c" ++ show (pts!!0) ++
          ", c" ++ show (pts!!1) ++
          "), color=\"black\")\n" ++
          "segments3d(rbind(c" ++ show (pts!!1) ++
          ", c" ++ show (pts!!2) ++
          "), color=\"black\")\n" ++
          "segments3d(rbind(c" ++ show (pts!!2) ++
          ", c" ++ show (pts!!0) ++
          "), color=\"black\")\n"
        else "\n"
      where
        pts = map (\p -> (p!!0,p!!1,p!!2))
                  (IM.elems $ _points $ _subsimplex ridge)


test :: IO Delaunay
test =
  delaunay [[-5,-5, 16], [-5, 8, 3 ], [ 4,-1, 3 ], [ 4,-5, 7], [ 4,-1,-10],
            [ 4,-5,-10], [-5, 8,-10], [-5,-5,-10]] False
-- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]

test2 :: IO Delaunay
test2 = delaunay [[-1,-1,-1],[-1,-1, 1],[-1, 1,-1],[-1, 1, 1],[ 1,-1,-1],
                  [ 1,-1, 1],[ 1, 1,-1],[ 1, 1, 1],[ 0, 0, 0]] False

test3 :: IO Delaunay
test3 = delaunay [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]] False

test4 :: IO Delaunay
test4 = delaunay [[0,0],[0,2],[2,0],[2,2],[1,1]] False

square :: [[Double]] -- TODO test point at infinity (Qz)
square = [[0,0],[0,1],[1,0],[1,1]]

centricCuboctahedron :: [[Double]]
centricCuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                       [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,i,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,0,0]]

rhombicDodecahedron :: [[Double]]
rhombicDodecahedron = [[-1.0, 0.0, 0.0], [-0.5,-0.5,-0.5], [-0.5,-0.5, 0.5],
                       [ 0.0,-1.0, 0.0], [-0.5, 0.5,-0.5], [-0.5, 0.5, 0.5],
                       [ 0.0, 1.0, 0.0], [ 1.0, 0.0, 0.0], [ 0.5,-0.5,-0.5],
                       [ 0.5,-0.5, 0.5], [ 0.5, 0.5,-0.5], [ 0.5, 0.5, 0.5],
                       [ 0.0, 0.0,-1.0], [ 0.0, 0.0, 1.0]]

faceCenteredCubic :: [[Double]]
faceCenteredCubic = [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1]
                    ,[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1]
                    ,[1,0,0],[-1,0,0]
                    ,[0,1,0],[0,-1,0]
                    ,[0,0,1],[0,0,-1]]
