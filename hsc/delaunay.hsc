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

#include "delaunay.h"

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
  _dim :: CUInt,
  _nfaces :: CUInt,
  _indices :: Ptr CUInt,
  _volumes :: Ptr CDouble,
  _owners  :: Ptr CUInt,
  __neighbors :: Ptr CUInt,
  _centers :: Ptr CDouble,
  _toporient :: Ptr CUInt,
  __ridges :: Ptr CUInt,
  _nridges :: CUInt,
  _areas  :: Ptr CDouble,
  _rcenters :: Ptr CDouble,
  _rnormals :: Ptr CDouble,
  _fnormals :: Ptr CDouble,
  _rdistances :: Ptr CDouble,
  __vrneighbors :: Ptr CUInt,
  _vrnsizes :: Ptr CUInt,
  __vfneighbors :: Ptr CUInt,
  _vfnsizes :: Ptr CUInt,
  _vvneighbors :: Ptr CUInt,
  _vvnsizes :: Ptr CUInt
} -- deriving (Show, Eq)

instance Storable CDelaunay where
    sizeOf    _ = #{size DelaunayT}
    alignment _ = #{alignment DelaunayT}
    peek ptr = do
      dim'       <- #{peek DelaunayT, dim} ptr
      nfaces'    <- #{peek DelaunayT, nfaces} ptr
      indices'   <- #{peek DelaunayT, indices} ptr
      volumes'   <- #{peek DelaunayT, fvolumes} ptr
      owners'    <- #{peek DelaunayT, owners} ptr
      neighbors' <- #{peek DelaunayT, neighbors} ptr
      centers'   <- #{peek DelaunayT, centers} ptr
      toporient' <- #{peek DelaunayT, toporient} ptr
      ridges'    <- #{peek DelaunayT, ridges} ptr
      nridges'   <- #{peek DelaunayT, nridges} ptr
      areas'     <- #{peek DelaunayT, rvolumes} ptr
      rcenters'  <- #{peek DelaunayT, rcenters} ptr
      rnormals'  <- #{peek DelaunayT, rnormals} ptr
      fnormals'  <- #{peek DelaunayT, fnormals} ptr
      rdistances'  <- #{peek DelaunayT, rdistances} ptr
      vrneighbors'  <- #{peek DelaunayT, vrneighbors} ptr
      vrnsizes' <- #{peek DelaunayT, vrnsizes} ptr
      vfneighbors'  <- #{peek DelaunayT, vfneighbors} ptr
      vfnsizes' <- #{peek DelaunayT, vfnsizes} ptr
      vvneighbors'  <- #{peek DelaunayT, vvneighbors} ptr
      vvnsizes' <- #{peek DelaunayT, vvnsizes} ptr
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
          #{poke DelaunayT, dim}            ptr r1
          #{poke DelaunayT, nfaces}         ptr r2
          #{poke DelaunayT, indices}        ptr r3
          #{poke DelaunayT, fvolumes}       ptr r4
          #{poke DelaunayT, owners}         ptr r5
          #{poke DelaunayT, neighbors}      ptr r6
          #{poke DelaunayT, centers}        ptr r7
          #{poke DelaunayT, toporient}      ptr r8
          #{poke DelaunayT, ridges}         ptr r9
          #{poke DelaunayT, rvolumes}       ptr r10
          #{poke DelaunayT, rcenters}       ptr r11
          #{poke DelaunayT, rnormals}       ptr r12
          #{poke DelaunayT, fnormals}       ptr r13
          #{poke DelaunayT, rdistances}     ptr r14
          #{poke DelaunayT, vrneighbors}    ptr r15
          #{poke DelaunayT, vrnsizes}       ptr r16
          #{poke DelaunayT, vfneighbors}    ptr r17
          #{poke DelaunayT, vfnsizes}       ptr r18
          #{poke DelaunayT, vvneighbors}    ptr r19
          #{poke DelaunayT, vvnsizes}       ptr r20
          #{poke DelaunayT, nridges}        ptr r21

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
            , _neighSites    = nvertices}
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
