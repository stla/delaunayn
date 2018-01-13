{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay
  where
import           Control.Monad         (when, (<$!>))
import           Data.Function         (on)
--import           Data.IntMap.Strict    (IntMap, fromListWith)
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as S
import           Data.List             (groupBy, sortOn, zip4, zipWith7)
import           Data.List.Split       (chunksOf, splitPlaces)
--import qualified Data.Map.Strict       as M
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek, sizeOf)
import           Result
import           System.IO             (readFile)
import           TemporaryFile

data Facet = Facet {
    _simplex    :: CentredPolytope
  , _neighbours :: [Int]
  , _ridges     :: [(CentredPolytope, [Int], Double)]
  , _volume     :: Double
  , _top        :: Bool
} deriving Show

data CentredPolytope = CentredPolytope {
    _vertices :: (IntSet, [[Double]])
--  , _indices  :: [[]]
  , _center   :: [Double]
  , _normal   :: [Double]
} deriving Show

-- type Ridges = IntMap [CentredPolytope]
-- type Ridges' = [Map [Int] [CentredPolytope]]

data Delaunay = Delaunay {
    _sites       :: [[Double]]
  , _facets      :: [Facet]
  , _vrneighbors :: [[IntSet]]
  , _vfneighbors :: [IntSet]
} deriving Show



foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -> CUInt -> CUInt -> Ptr CUInt -> Ptr CUInt -> CString
  -> IO (Ptr Result)

delaunay :: [[Double]] -> IO Delaunay
delaunay sites = do
  let n = length sites
      dim = length (head sites)
  when (n <= dim+1) $
    error "insufficient number of points"
  sitesPtr <- mallocBytes (n * dim * (sizeOf (undefined :: CDouble)))
  pokeArray sitesPtr (concat (map (map realToFrac) sites))
  nfPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  tmpFile <- getTemporaryFile "tmp.txt"
  tmpFile' <- newCString tmpFile
  resultPtr <- c_delaunay
               sitesPtr (fromIntegral dim) (fromIntegral n) nfPtr
               exitcodePtr tmpFile'
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
      centers <- (<$!>) (map realToFrac)
                        (peekArray (nf * dim) (_centers result))
      normals <- (<$!>) (map realToFrac)
                        (peekArray (nf * dim) (_fnormals result))
      areas <- (<$!>) (map realToFrac) (peekArray nf (_areas result))
      neighbors <- (<$!>) (map fromIntegral)
                          (peekArray (nf * (dim+1)) (__neighbors result))
      let neighbors' = map ((map (subtract 1)).(filter (/=0))) $
                           chunksOf (dim+1) neighbors
          n_ridges = nf * (dim+1);
      toporient <- (<$!>) (map (==1)) (peekArray nf (_toporient result))
      ridges'' <- (<$!>) ((chunksOf (2+dim)) . (map fromIntegral))
                         (peekArray (n_ridges * (2+dim)) (__ridges result))
      ridgesCenters <- (<$!>) ((chunksOf dim) . (map realToFrac))
                              (peekArray (n_ridges * dim) (_rcenters result))
      ridgesNormals <- (<$!>) ((chunksOf dim) . (map realToFrac))
                              (peekArray (n_ridges * dim) (_rnormals result))
      rdistances <- (<$!>) (map realToFrac)
                           (peekArray n_ridges (_rdistances result))
      vrnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vrnsizes result))
      vrneighbors <- (<$!>) (map (map S.fromList) . (splitPlaces vrnsizes) .
                            (chunksOf dim) . (map fromIntegral))
                            (peekArray (sum vrnsizes * dim)
                                       (__vrneighbors result))
      vfnsizes <- (<$!>) (map fromIntegral)
                         (peekArray n (_vfnsizes result))
      vfneighbors <- (<$!>) (map S.fromList . (splitPlaces vfnsizes) .
                            (map fromIntegral))
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
      let ridges_rdistances = map (map (snd)) $
            groupBy ((==) `on` fst) $ sortOn fst $
              map (\((a,b),c,d,e) -> (head a, (doCPolytope b c d, filter (<nf) a, e)))
                  (zip4 (map (splitAt 2) ridges'')
                        ridgesCenters
                        ridgesNormals
                        rdistances)
--      putStrLn $ show ridges_rdistances
          -- ridges = map (\(b,c,d) -> doCPolytope b c d)
          --              (zip3 ridges' ridgesCenters ridgesNormals)
          --
      (>>=) (readFile tmpFile) putStrLn -- print summary
      return $ Delaunay { _sites = sites
                        , _facets = zipWith7 toFacet
                                    (chunksOf (dim+1) indices)
                                    (chunksOf dim normals)
                                    neighbors' ridges_rdistances
                                    (chunksOf dim centers) areas toporient
                        , _vrneighbors = vrneighbors
                        , _vfneighbors = vfneighbors }
  where
    toFacet :: [Int] -> [Double] -> [Int] -> [(CentredPolytope,[Int],Double)] -> [Double] -> Double -> Bool -> Facet
    toFacet verts normal neighs r center vol top =
      Facet { _simplex   = doCPolytope verts center normal
            , _neighbours = neighs
            , _ridges     = r
            , _volume     = vol
            , _top        = top }
    doCPolytope :: [Int] -> [Double] -> [Double] -> CentredPolytope
    doCPolytope indices center normal =
      CentredPolytope { _vertices = (S.fromList indices, map ((!!) sites) indices),
                        _center   = center,
                        _normal   = normal }


-- test :: IO Delaunay
-- test =
--   delaunay [[-5,-5,16], [-5,8,3], [4,-1,3], [4,-5,7], [4,-1,-10], [4,-5,-10], [-5,8,-10], [-5,-5,-10]]
-- -- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]
--
-- test2 :: IO Delaunay
test2 = delaunay [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1],[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1],[0,0,0]]
--
-- test3 :: IO Delaunay
-- test3 = delaunay [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]
--
-- test4 :: IO Delaunay
-- test4 = delaunay [[0,0],[0,2],[2,0],[2,2],[1,1]]
