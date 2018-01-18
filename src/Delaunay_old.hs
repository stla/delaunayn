--{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay
  where
import           Control.Monad         (when, (<$!>))
import           Data.Function         (on)
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS
import           Data.List
import           Data.List.Split       (chunksOf, splitPlaces)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as S
--import           Data.Tuple.Extra      ((&&&))
import           Foreign.C.String
import           Foreign.C.Types
--import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek, sizeOf)
import           Result
import           System.IO             (readFile)
import           TemporaryFile


-- data Facet = Facet {
--     _simplex    :: CentredPolytope
--   , _neighbours :: [Int]
--   , _ridges     :: [Ridge]
--   , _volume     :: Double
-- --  , _owner      :: Maybe Int
--   , _top        :: Bool
-- } deriving Show
--
-- data Ridge = Ridge {
--     _simplex  :: CentredPolytope
--   , _area     :: Double
--   , _ridgeOf  :: [Int]
--   , _distance :: Double -- bof...
-- } deriving Show
--
-- data CentredPolytope = CentredPolytope {
--     _vertices :: IntSet
--   , _points   :: [[Double]]
--   , _center   :: [Double]
--   , _normal   :: [Double]
-- } deriving Show
--
-- data Delaunay = Delaunay {
--     _sites       :: [[Double]]
--   , _facets      :: IntMap Facet
--   , _vrneighbors :: [[IntSet]]
--   , _vfneighbors :: [IntSet] -- sites, vrneighbors et vfneighbors ont la même taile...
-- } deriving Show

type IndexSet = IntSet
type IndexMap = IntMap

data Facet = Facet {
    _simplex    :: Polytope
  , _neighbours :: IntSet
} deriving Show

data Vertex = Vertex {
    _coordinates :: [Double]
  , _neighRidges :: Set IndexSet
  , _neighFacets :: IntSet
} deriving Show

data Polytope = Polytope {
    _points :: IndexMap [Double]
  , _center :: [Double]
  , _normal :: [Double]
  , _volume :: Double
} deriving Show

data Ridge = Ridge {
    _polytope :: Polytope
  , _ridgeOf  :: IntSet
} deriving Show

data Delaunay = Delaunay {
    _vertices :: IndexMap Vertex
  , _ridges   :: [Ridge]
  , _facets   :: IntMap Facet
} deriving Show


-- _fsimplex :: Facet -> CentredPolytope
-- _fsimplex = _simplex
--
-- _rsimplex :: Ridge -> CentredPolytope
-- _rsimplex = _simplex
--
-- facetVertices :: Facet -> IntSet
-- facetVertices = _vertices . _fsimplex
--
-- facetCenter :: Facet -> [Double]
-- facetCenter = _center . _fsimplex

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -> CUInt -> CUInt -> CUInt -> Ptr CUInt -> Ptr CUInt -> CString
  -> IO (Ptr Result)

-- zipWith8 ::
--   (a -> b -> c -> d -> e -> f -> g -> h -> i)
--   -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h] -> [i]
-- zipWith8 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs)
--          =  z a b c d e f g h : zipWith8 z as bs cs ds es fs gs hs
-- zipWith8 _ _ _ _ _ _ _ _ _ = []

cdbl2dbl :: CDouble -> Double
cdbl2dbl x = if isNaN x
                then 0/0
                else realToFrac x

delaunay :: [[Double]] -> Bool -> IO Delaunay
delaunay sites deg = do
  let n = length sites
      dim = length (head sites)
  when (n <= dim+1) $
    error "insufficient number of points"
  sitesPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray sitesPtr (concatMap (map realToFrac) sites)
  nfPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  tmpFile <- getTemporaryFile "tmp.txt"
  tmpFile' <- newCString tmpFile
  resultPtr <- c_delaunay sitesPtr
               (fromIntegral dim) (fromIntegral n) (fromIntegral $ fromEnum deg)
               nfPtr exitcodePtr tmpFile'
  -- sitesFptr <- newForeignPtr_ sitesPtr
  -- nfFPtr <- newForeignPtr_ nfPtr
  -- exitcodeFPtr <- newForeignPtr_ exitcodePtr
  -- resultPtr <- withForeignPtr sitesFptr $
  --               \si -> withForeignPtr nfFPtr $
  --                 \nf -> withForeignPtr exitcodeFPtr $
  --                  \ex -> c_delaunay
  --                               si (fromIntegral dim) (fromIntegral n) nf
  --                               ex tmpFile'
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free sitesPtr
  if exitcode /= 0
    then do
      free nfPtr
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      nf <- (<$!>) fromIntegral (peek nfPtr)
      free nfPtr
      result <- peek resultPtr
      indices <- (<$!>) (map fromIntegral)
                        (peekArray (nf * (dim+1)) (_indices result))
      centers <- (<$!>) (map cdbl2dbl)
                        (peekArray (nf * dim) (_centers result))
      normals <- (<$!>) (map realToFrac)
                        (peekArray (nf * (dim+1)) (_fnormals result))
      volumes <- (<$!>) (map realToFrac) (peekArray nf (_volumes result))
      neighbors <- (<$!>) (map fromIntegral)
                          (peekArray (nf * (dim+1)) (__neighbors result))
      let neighbors' = map (map (subtract 1) . filter (/=0)) $
                           chunksOf (dim+1) neighbors
          n_ridges = nf * (dim+1);
--      toporient <- (<$!>) (map (==1)) (peekArray nf (_toporient result))
      -- owners <- (<$!>) (map (\i -> if i==0 then Nothing else Just $ fromIntegral i-1))
      --                  (peekArray nf (_owners result))
      ridges'' <- (<$!>) (chunksOf (2+dim) . map fromIntegral)
                         (peekArray (n_ridges * (2+dim)) (__ridges result))
      ridgesCenters <- (<$!>) (chunksOf dim . map cdbl2dbl)
                              (peekArray (n_ridges * dim) (_rcenters result))
      ridgesNormals <- (<$!>) (chunksOf dim . map realToFrac)
                              (peekArray (n_ridges * dim) (_rnormals result))
      -- rdistances <- (<$!>) (map realToFrac)
      --                      (peekArray n_ridges (_rdistances result))
      areas <- (<$!>) (map realToFrac)
                      (peekArray n_ridges (_areas result))
      vrnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vrnsizes result))
      vrneighbors <- (<$!>) (map (map IS.fromList) . splitPlaces vrnsizes .
                            chunksOf dim . map fromIntegral)
                            (peekArray (sum vrnsizes * dim)
                                       (__vrneighbors result))
      vfnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vfnsizes result))
      vfneighbors <- (<$!>) (map IS.fromList . splitPlaces vfnsizes .
                             map fromIntegral)
                            (peekArray (sum vfnsizes) (__vfneighbors result))
      free resultPtr
      -- let ridges' = map (\((a,b),c,d) -> (head a, [doCPolytope b c d]))
      --                   (zip3 (map (splitAt 1) ridges'') ridgesCenters ridgesNormals)
      --     ridges = fromListWith (++) ridges'
      --     (_, _ridges'') = unzip $ sortOn (head . fst) $ map (splitAt 1) ridges''
      --     _ridges' = map (\(b,c,d) -> (b, [doCPolytope b c d]))
      --                       (zip3 _ridges'' ridgesCenters ridgesNormals)
      --     _ridges = M.fromListWith (++) _ridges'
      -- let (ids', ridges') = sortOn (head . fst) $ map (splitAt 1) ridges''
      --     ids = map head ids'
      --
      -- let ridges_rdistances = map (map (snd)) $
      --       groupBy ((==) `on` fst) $ sortOn fst $
      --         map (\((a,b),c,d,e,f) -> (head a, doRidge (filter (<nf) a) b c d e f))
      --             (zip5 (map (splitAt 2) ridges'')
      --                   ridgesCenters
      --                   ridgesNormals
      --                   rdistances
      --                   areas)
      let ridges = map (\((a,b),c,d,e) -> doRidge (filter (<nf) a) b c d e)
                  (zip4 (map (splitAt 2) ridges'')
                        ridgesCenters ridgesNormals areas)
      (>>=) (readFile tmpFile) putStrLn -- print summary
      return Delaunay { _vertices = IM.fromList $ zip [0 .. n]
                                    (zipWith3 toVertex
                                     sites vrneighbors vfneighbors)
                      , _facets = IM.fromList $ zip [0 .. nf]
                                  (zipWith5 toFacet
                                  (chunksOf (dim+1) indices)
                                  (chunksOf (dim+1) normals)
                                  neighbors' (chunksOf dim centers) volumes)
                      , _ridges = nubBy ((==) `on` _ridgeVertices) ridges}
  where
    toVertex :: [Double] -> [IndexSet] -> IntSet -> Vertex
    toVertex coords nridges nfacets =
      Vertex {  _coordinates = coords
              , _neighRidges = S.fromList nridges
              , _neighFacets = nfacets }
    toFacet :: [Int] -> [Double] -> [Int] -> [Double] -> Double -> Facet
    toFacet verts normal neighs center vol =
      Facet { _simplex   = doPolytope verts center normal vol
            , _neighbours = IS.fromList neighs }
    doPolytope :: [Int] -> [Double] -> [Double] -> Double -> Polytope
    doPolytope indices center normal volume =
      Polytope { _points  = IM.fromList $ zip indices (map (sites !!) indices)
               , _center  = center
               , _normal  = normal
               , _volume  = volume }
    doRidge :: [Int] -> [Int] -> [Double] -> [Double] -> Double -> Ridge
    doRidge facets is center norm vol =
      Ridge { _polytope = doPolytope is center norm vol
            , _ridgeOf = IS.fromList facets }
    -- toFacet :: [Int] -> [Double] -> [Int] -> [Ridge] -> [Double] -> Double -> Bool -> Facet
    -- toFacet verts normal neighs r center vol top =
    --   Facet { _simplex   = doCPolytope verts center normal
    --         , _neighbours = neighs
    --         , _ridges     = r
    --         , _volume     = vol
    --         , _top        = top }
    -- doCPolytope :: [Int] -> [Double] -> [Double] -> CentredPolytope
    -- doCPolytope indices center normal =
    --   CentredPolytope { _vertices = S.fromList indices
    --                   , _points   = map ((!!) sites) indices
    --                   , _center   = center
    --                   , _normal   = normal }
    -- doRidge :: [Int] -> [Int] -> [Double] -> [Double] -> Double -> Double -> Ridge
    -- doRidge a b c d e f =
    --   Ridge { _simplex = doCPolytope b c d
    --         , _ridgeOf = a
    --         , _distance = e
    --         , _area = f }

sandwichedRidge :: Ridge -> Bool
sandwichedRidge ridge = IS.size (_ridgeOf ridge) == 2

_ridgeVertices :: Ridge -> IndexSet
_ridgeVertices = IS.fromList . IM.keys . _points . _polytope

ridgesMap :: Delaunay -> Map IndexSet Ridge
ridgesMap tess = M.fromList ridges'
  where
    ridges' :: [(IndexSet, Ridge)]
    ridges' =  map (\ridge -> let vertices = _ridgeVertices ridge in
                                    (vertices, ridge))
                   (_ridges tess)

_facetVertices :: Facet -> IndexSet
_facetVertices = IS.fromList . IM.keys . _points . _simplex

allFacets :: Delaunay -> [IndexSet]
allFacets d = map _facetVertices (IM.elems $ _facets d)

sandwichedRidge' :: Delaunay -> Ridge -> Bool
sandwichedRidge' d ridge = length (filter (\iiii -> IS.size (IS.intersection iiii (_ridgeVertices ridge)) == 3) (allFacets d)) == 2


-- dim 3 uniquement
-- data Edgee = Egdee {
--     _verts :: (Int, Int)
--   , _isEdgeOf :: [Ridge] }
--
-- exteriorRidges :: Delaunay -> [Ridge] -- un exterior edge n'est pas forcément sur un exterior ridge! si ?
-- exteriorRidges d = nubBy ((==) `on` _ridgeVertices) (filter (\r -> IS.size (_ridgeOf r) == 1) (_ridges d))
--
-- exteriorEdges :: Delaunay -> [IndexSet]
-- exteriorEdges tess = nub $ filter (\inter -> IS.size inter == 2)
--                             ([IS.intersection (_ridgeVertices r1) (_ridgeVertices r2) |
--                               r1 <- exteriorRidges tess,
--                               r2 <- exteriorRidges tess])
--
-- ridgeEdges :: Ridge -> [IndexSet]
-- ridgeEdges ridge = [IS.fromList [i1,i2], IS.fromList [i2,i3], IS.fromList [i3,i1]]
--   where [i1,i2,i3] = IS.toList $ _ridgeVertices ridge
--
-- exteriorRidges' :: Delaunay -> [Ridge]
-- exteriorRidges' d = filter (\r -> all (`elem` (exteriorEdges d)) (ridgeEdges r)) (_ridges d)
--
-- isExteriorRidge :: Delaunay -> Ridge -> Bool
-- isExteriorRidge d r = all (`elem` (exteriorEdges d)) (ridgeEdges r)
--
-- connectedVertices :: Delaunay -> [IndexSet]
-- connectedVertices d = nub $ concatMap ridgeEdges (exteriorRidges d)
--
-- isEdgeOf :: Delaunay -> IndexSet -> [Ridge]
-- isEdgeOf d i1i2 = filter (\r -> IS.size (IS.intersection i1i2 (_ridgeVertices r)) == 2) (exteriorRidges d)
--
-- numberOfNeighbourRidges :: Delaunay -> [Int]
-- numberOfNeighbourRidges d = map (length . isEdgeOf d) (exteriorEdges d)
--
-- exteriorEdges :: Delaunay -> [((Int, Int), [Ridge])]
-- exteriorEdges tess = map (\ridge -> map (\ii -> (ii, ridge)) (ridgeEdges ridge))
--                          (exteriorRidges tess)

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
        pts = map (\p -> (p!!0,p!!1,p!!2)) (IM.elems $ _points $ _polytope ridge)


test :: IO Delaunay
test =
  delaunay [[-5,-5,16], [-5,8,3], [4,-1,3], [4,-5,7], [4,-1,-10], [4,-5,-10], [-5,8,-10], [-5,-5,-10]] False
-- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]

test2 :: IO Delaunay
test2 = delaunay [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1],[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1],[0,0,0]] False

test3 :: IO Delaunay
test3 = delaunay [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]] False

test4 :: IO Delaunay
test4 = delaunay [[0,0],[0,2],[2,0],[2,2],[1,1]] False

cuboctahedron :: [[Double]]
cuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                [[0,i,j] | i <- [-1,1], j <- [-1,1]] ++
                [[0,0,0]]

rhombicDodecahedron :: [[Double]]
rhombicDodecahedron = [[-1.0,0.0,0.0], [-0.5,-0.5,-0.5], [-0.5,-0.5,0.5],
                       [0.0,-1.0,0.0], [-0.5,0.5,-0.5] , [-0.5,0.5,0.5] ,
                       [0.0,1.0,0.0] , [1.0,0.0,0.0]   , [0.5,-0.5,-0.5],
                       [0.5,-0.5,0.5], [0.5,0.5,-0.5]  , [0.5,0.5,0.5]  ,
                       [0.0,0.0,-1.0], [0.0,0.0,1.0]]

faceCenteredCubic :: [[Double]]
faceCenteredCubic = [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1]
                    ,[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1]
                    ,[1,0,0],[-1,0,0]
                    ,[0,1,0],[0,-1,0]
                    ,[0,0,1],[0,0,-1]]
