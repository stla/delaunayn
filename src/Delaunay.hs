{-# LANGUAGE ForeignFunctionInterface, DuplicateRecordFields #-}
module Delaunay
  where
import           Control.Monad         (when, (<$!>))
import           Data.IntMap.Strict    (IntMap, fromListWith)
import qualified Data.IntMap.Strict    as M
import           Data.List             (union, zipWith5)
import           Data.List.Split       (chunksOf)
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
    _vertices   :: [Int]
  , _neighbours :: [Int]
  , _center     :: [Double]
  , _volume     :: Double
  , _top        :: Bool
} deriving Show

data CentredPolytope = CentredPolytope {
    _vertices :: [[Double]]
  , _center   :: [Double]
} deriving Show

type Ridges = IntMap [CentredPolytope]

data Delaunay = Delaunay {
    _sites  ::  [[Double]]
  , _facets ::  [Facet]
  , _ridges ::  Ridges
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
      areas <- (<$!>) (map realToFrac) (peekArray nf (_areas result))
      neighbors <- (<$!>) (map fromIntegral)
                          (peekArray (nf * (dim+1)) (_neighbors result))
      let neighbors' = map ((map (subtract 1)).(filter (/=0))) $
                           chunksOf (dim+1) neighbors
          n_neighbors = sum $ map length neighbors'
      putStrLn $ "*********" ++ show n_neighbors ++ "************"
      toporient <- (<$!>) (map (==1)) (peekArray nf (_toporient result))
      ridges'' <- (<$!>) ((chunksOf (1+dim)) . (map fromIntegral))
                         (peekArray (n_neighbors * (1+dim)) (__ridges result))
      ridgesCenters <- (<$!>) ((chunksOf dim) . (map realToFrac))
                              (peekArray (n_neighbors * dim) (_rcenters result))
      free resultPtr
      let ridges' = map (\((a,b),c) -> (head a, [doCPolytope b c]))
                        (zip (map (splitAt 1) ridges'') ridgesCenters)
          ridges = fromListWith (++) ridges'
      -- let ridges' = fromListWith union
      --               (map ((\(a,b) -> (head a, [b])) . (splitAt 1)) ridges'')
      (>>=) (readFile tmpFile) putStrLn -- print summary
      return $ Delaunay { _sites = sites
                        , _facets = zipWith5 toFacet
                                    (chunksOf (dim+1) indices) neighbors'
                                    (chunksOf dim centers) areas toporient
                        , _ridges = ridges }
  where
    toFacet :: [Int] -> [Int] -> [Double] -> Double -> Bool -> Facet
    toFacet verts neighs center vol top = Facet { _vertices   = verts
                                                , _neighbours = neighs
                                                , _center     = center
                                                , _volume     = vol
                                                , _top        = top }
    doCPolytope :: [Int] -> [Double] -> CentredPolytope
    doCPolytope indices center =
      CentredPolytope { _vertices = map ((!!) sites) indices,
                        _center   = center }


-- test :: IO Delaunay
-- test =
--   delaunay [[-5,-5,16], [-5,8,3], [4,-1,3], [4,-5,7], [4,-1,-10], [4,-5,-10], [-5,8,-10], [-5,-5,-10]]
-- -- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]
--
test2 :: IO Delaunay
test2 = delaunay [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1],[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1],[0,0,0]]
--
-- test3 :: IO Delaunay
-- test3 = delaunay [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]
--
-- test4 :: IO Delaunay
-- test4 = delaunay [[0,0],[0,2],[2,0],[2,2],[1,1]]
