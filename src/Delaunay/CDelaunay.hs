{-# LINE 1 "delaunay.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay.CDelaunay
  where
import           Control.Monad         ((<$!>))
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS
import           Data.List
import           Data.List.Split       (chunksOf, splitPlaces)
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable

type Index = Int
type IndexSet = IntSet
type IndexMap = IntMap

data Facet = Facet {
    _simplex   :: Simplex
  , _neighbors :: IntSet
} deriving Show

data Site = Site {
    _coordinates   :: [Double]
  , _neighSites :: IntSet
  , _neighRidges   :: Set IndexSet
  , _neighFacets   :: IntSet
} deriving Show

data Simplex = Simplex {
    _points :: IndexMap [Double]
  , _circumcenter :: [Double]
  , _normal :: [Double]
  , _volume :: Double
} deriving Show

data Ridge = Ridge {
    _subsimplex :: Simplex
  , _ridgeOf  :: IntSet
} deriving Show

data Delaunay = Delaunay {
    _vertices :: IndexMap Site
  , _ridges   :: [Ridge]
  , _facets   :: IntMap Facet
} deriving Show

data CDelaunay = CDelaunay {
  _dim          :: CUInt,
  _nfaces       :: CUInt,
  _indices      :: Ptr CUInt,
  _volumes      :: Ptr CDouble,
  _owners       :: Ptr CUInt,
  __neighbors   :: Ptr CUInt,
  _centers      :: Ptr CDouble,
  _toporient    :: Ptr CUInt,
  __ridges      :: Ptr CUInt,
  _nridges      :: CUInt,
  _areas        :: Ptr CDouble,
  _rcenters     :: Ptr CDouble,
  _rnormals     :: Ptr CDouble,
  _fnormals     :: Ptr CDouble,
  _rdistances   :: Ptr CDouble,
  __vrneighbors :: Ptr CUInt,
  _vrnsizes     :: Ptr CUInt,
  __vfneighbors :: Ptr CUInt,
  _vfnsizes     :: Ptr CUInt,
  _vvneighbors  :: Ptr CUInt,
  _vvnsizes     :: Ptr CUInt
} -- deriving (Show, Eq)

instance Storable CDelaunay where
    sizeOf    _ = (160)
{-# LINE 82 "delaunay.hsc" #-}
    alignment _ = 8
{-# LINE 83 "delaunay.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 85 "delaunay.hsc" #-}
      nfaces'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 86 "delaunay.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 87 "delaunay.hsc" #-}
      volumes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 88 "delaunay.hsc" #-}
      owners'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 89 "delaunay.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 90 "delaunay.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 91 "delaunay.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 92 "delaunay.hsc" #-}
      ridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 93 "delaunay.hsc" #-}
      nridges'   <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 94 "delaunay.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 95 "delaunay.hsc" #-}
      rcenters'  <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 96 "delaunay.hsc" #-}
      rnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 97 "delaunay.hsc" #-}
      fnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 98 "delaunay.hsc" #-}
      rdistances'  <- (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 99 "delaunay.hsc" #-}
      vrneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 100 "delaunay.hsc" #-}
      vrnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 101 "delaunay.hsc" #-}
      vfneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 128) ptr
{-# LINE 102 "delaunay.hsc" #-}
      vfnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 136) ptr
{-# LINE 103 "delaunay.hsc" #-}
      vvneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 144) ptr
{-# LINE 104 "delaunay.hsc" #-}
      vvnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 152) ptr
{-# LINE 105 "delaunay.hsc" #-}
      return CDelaunay { _dim = dim'
                       , _nfaces = nfaces'
                       , _indices = indices'
                       , _volumes = volumes'
                       , _owners = owners'
                       , __neighbors = neighbors'
                       , _centers = centers'
                       , _toporient = toporient'
                       , __ridges    = ridges'
                       , _nridges = nridges'
                       , _areas    = areas'
                       , _rcenters = rcenters'
                       , _rnormals = rnormals'
                       , _fnormals = fnormals'
                       , _rdistances = rdistances'
                       , __vrneighbors = vrneighbors'
                       , _vrnsizes = vrnsizes'
                       , __vfneighbors = vfneighbors'
                       , _vfnsizes = vfnsizes'
                       , _vvneighbors = vvneighbors'
                       , _vvnsizes = vvnsizes'
                     }
    poke ptr (CDelaunay r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)            ptr r1
{-# LINE 130 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 4)         ptr r2
{-# LINE 131 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r3
{-# LINE 132 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r4
{-# LINE 133 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)         ptr r5
{-# LINE 134 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r6
{-# LINE 135 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)        ptr r7
{-# LINE 136 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)      ptr r8
{-# LINE 137 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)         ptr r9
{-# LINE 138 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72)       ptr r10
{-# LINE 139 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80)       ptr r11
{-# LINE 140 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88)       ptr r12
{-# LINE 141 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 96)       ptr r13
{-# LINE 142 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 104)     ptr r14
{-# LINE 143 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 112)    ptr r15
{-# LINE 144 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 120)       ptr r16
{-# LINE 145 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 128)    ptr r17
{-# LINE 146 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 136)       ptr r18
{-# LINE 147 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 144)    ptr r19
{-# LINE 148 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 152)       ptr r20
{-# LINE 149 "delaunay.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)        ptr r21
{-# LINE 150 "delaunay.hsc" #-}

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -- sites
  -> CUInt       -- dim
  -> CUInt       -- nsites
  -> CUInt       -- degenerate ? (0/1)
  -> Ptr CUInt   -- exitcode
  -> CString     -- file
  -> IO (Ptr CDelaunay)

cDelaunayPtrToDelaunay :: Ptr CDelaunay -> [[Double]] -> IO Delaunay
cDelaunayPtrToDelaunay cdelaunayPtr sites = do
  result <- peek cdelaunayPtr
  let nf  = fromIntegral $ _nfaces result
      dim = fromIntegral $ _dim result
      n   = length sites
  indices   <- (<$!>) (map fromIntegral)
                      (peekArray (nf * (dim+1)) (_indices result))
  centers   <- (<$!>) (map cdbl2dbl)
                      (peekArray (nf * dim) (_centers result))
  normals   <- (<$!>) (map realToFrac)
                      (peekArray (nf * (dim+1)) (_fnormals result))
  volumes   <- (<$!>) (map realToFrac) (peekArray nf (_volumes result))
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
  let ridges = map (\((a,b),c,d,e) -> doRidge (filter (<nf) a) b c d e)
              (zip4 (map (splitAt 2) ridges'')
                    ridgesCenters ridgesNormals areas)
  -- (>>=) (readFile tmpFile) putStrLn -- print summary
  return Delaunay { _vertices = IM.fromList $ zip [0 .. n]
                                (zipWith4 toSite
                                 sites vrneighbors vfneighbors vvneighbors)
                  , _facets = IM.fromList $ zip [0 .. nf-1]
                              (zipWith5 toFacet
                              (chunksOf (dim+1) indices)
                              (chunksOf (dim+1) normals)
                              neighbors' (chunksOf dim centers) volumes)
                  , _ridges = ridges } --nubBy ((==) `on` ridgeVertices) ridges}
  where
    toSite :: [Double] -> [IndexSet] -> IntSet -> IntSet -> Site
    toSite coords nridges nfacets nvertices =
      Site {  _coordinates   = coords
            , _neighRidges   = S.fromList nridges
            , _neighFacets   = nfacets
            , _neighSites    = nvertices }
    toFacet :: [Int] -> [Double] -> [Int] -> [Double] -> Double -> Facet
    toFacet verts normal neighs center vol =
      Facet { _simplex   = doSimplex verts center normal vol
            , _neighbors = IS.fromList neighs }
    doSimplex :: [Int] -> [Double] -> [Double] -> Double -> Simplex
    doSimplex is center normal volume =
      Simplex { _points        = IM.fromAscList $ zip is (map (sites !!) is)
              , _circumcenter  = center
              , _normal        = normal
              , _volume        = volume }
    doRidge :: [Int] -> [Int] -> [Double] -> [Double] -> Double -> Ridge
    doRidge facets is center norm vol =
      Ridge { _subsimplex = doSimplex is center norm vol
            , _ridgeOf = IS.fromAscList facets }
    cdbl2dbl :: CDouble -> Double
    cdbl2dbl x = if isNaN x then 0/0 else realToFrac x
