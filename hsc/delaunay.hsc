{-# LANGUAGE ForeignFunctionInterface #-}
module CDelaunay
  where
import Foreign
import Foreign.C.Types

#include "delaunay.h"

data CDelaunay = CDelaunay {
  _dim :: CUInt,
  _length :: CUInt,
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
      length'    <- #{peek DelaunayT, length} ptr
      indices'   <- #{peek DelaunayT, indices} ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr DelaunayT, indices} ptr
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
                       , _length = length'
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
          #{poke DelaunayT, length}         ptr r2
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
