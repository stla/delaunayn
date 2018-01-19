{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay
  where
import           Control.Monad         (when, (<$!>))
--import           Data.Function         (on)
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS
import           Data.List
import           Data.List.Split       (chunksOf, splitPlaces)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Maybe
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek, sizeOf)
import           CDelaunay
import           System.IO             (readFile)
import           TemporaryFile

type Index = Int
type IndexSet = IntSet
type IndexMap = IntMap

data Facet = Facet {
    _simplex   :: Polytope
  , _neighbors :: IntSet
} deriving Show

data Vertex = Vertex {
    _coordinates   :: [Double]
  , _neighVertices :: IntSet
  , _neighRidges   :: Set IndexSet
  , _neighFacets   :: IntSet
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

ridgeVertices :: Ridge -> IndexSet
ridgeVertices = IS.fromAscList . IM.keys . _points . _polytope


foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -> CUInt -> CUInt -> CUInt -> Ptr CUInt -> Ptr CUInt -> CString
  -> IO (Ptr CDelaunay)


cdbl2dbl :: CDouble -> Double
cdbl2dbl x = if isNaN x
                then 0/0
                else realToFrac x


delaunay :: [[Double]] -> Bool -> IO Delaunay
delaunay sites deg = do -- on pourrait définir tout ça dans le peek ; oui mais le poke ?
  let n = length sites
      dim = length (head sites) -- TODO check même longueur
  when (dim < 2) $
    error "dimension must be at least 2"
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
          n_ridges = fromIntegral (_nridges result)
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
      vrneighbors <- (<$!>) (map (map IS.fromAscList) . splitPlaces vrnsizes .
                            chunksOf dim . map fromIntegral)
                            (peekArray (sum vrnsizes * dim)
                                       (__vrneighbors result))
      vfnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vfnsizes result))
      vfneighbors <- (<$!>) (map IS.fromAscList . splitPlaces vfnsizes .
                             map fromIntegral)
                            (peekArray (sum vfnsizes) (__vfneighbors result))
      vvnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vvnsizes result))
      vvneighbors <- (<$!>) (map IS.fromAscList . splitPlaces vvnsizes .
                             map fromIntegral)
                            (peekArray (sum vvnsizes) (_vvneighbors result))
      free resultPtr
      let ridges = map (\((a,b),c,d,e) -> doRidge (filter (<nf) a) b c d e)
                  (zip4 (map (splitAt 2) ridges'')
                        ridgesCenters ridgesNormals areas)
      (>>=) (readFile tmpFile) putStrLn -- print summary
      return Delaunay { _vertices = IM.fromList $ zip [0 .. n]
                                    (zipWith4 toVertex
                                     sites vrneighbors vfneighbors vvneighbors)
                      , _facets = IM.fromList $ zip [0 .. nf]
                                  (zipWith5 toFacet
                                  (chunksOf (dim+1) indices)
                                  (chunksOf (dim+1) normals)
                                  neighbors' (chunksOf dim centers) volumes)
                      , _ridges = ridges } --nubBy ((==) `on` ridgeVertices) ridges}
  where
    toVertex :: [Double] -> [IndexSet] -> IntSet -> IntSet -> Vertex
    toVertex coords nridges nfacets nvertices =
      Vertex {  _coordinates   = coords
              , _neighRidges   = S.fromList nridges
              , _neighFacets   = nfacets
              , _neighVertices = nvertices}
    toFacet :: [Int] -> [Double] -> [Int] -> [Double] -> Double -> Facet
    toFacet verts normal neighs center vol =
      Facet { _simplex   = doPolytope verts center normal vol
            , _neighbors = IS.fromList neighs }
    doPolytope :: [Int] -> [Double] -> [Double] -> Double -> Polytope
    doPolytope is center normal volume =
      Polytope { _points  = IM.fromAscList $ zip is (map (sites !!) is)
               , _center  = center
               , _normal  = normal
               , _volume  = volume }
    doRidge :: [Int] -> [Int] -> [Double] -> [Double] -> Double -> Ridge
    doRidge facets is center norm vol =
      Ridge { _polytope = doPolytope is center norm vol
            , _ridgeOf = IS.fromAscList facets }


-- | the ridges a vertex belongs to
vertexNeighborRidges :: Delaunay -> Index -> [Ridge]
vertexNeighborRidges tess i =
  filter (\r -> ridgeVertices r `elem` _neighRidges (_vertices tess IM.! i))
         (_ridges tess)

sandwichedRidge :: Ridge -> Bool
sandwichedRidge ridge = IS.size (_ridgeOf ridge) == 2

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

cuboctahedron :: [[Double]]
cuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
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
