{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import Foreign
import Foreign.C.Types

#include "result.h"

data Result = Result {
  _dim :: CUInt,
  _length :: CUInt,
  _indices :: Ptr CUInt,
  _volumes :: Ptr CDouble,
  _owners  :: Ptr CUInt,
  __neighbors :: Ptr CUInt,
  _centers :: Ptr CDouble,
  _toporient :: Ptr CUInt,
  __ridges :: Ptr CUInt,
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

instance Storable Result where
    sizeOf    _ = #{size ResultT}
    alignment _ = #{alignment ResultT}
    peek ptr = do
      dim'       <- #{peek ResultT, dim} ptr
      length'    <- #{peek ResultT, length} ptr
      indices'   <- #{peek ResultT, indices} ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
      volumes'   <- #{peek ResultT, fvolumes} ptr
      owners'   <- #{peek ResultT, owners} ptr
      neighbors' <- #{peek ResultT, neighbors} ptr
      centers'   <- #{peek ResultT, centers} ptr
      toporient' <- #{peek ResultT, toporient} ptr
      ridges'    <- #{peek ResultT, ridges} ptr
      areas'     <- #{peek ResultT, rvolumes} ptr
      rcenters'  <- #{peek ResultT, rcenters} ptr
      rnormals'  <- #{peek ResultT, rnormals} ptr
      fnormals'  <- #{peek ResultT, fnormals} ptr
      rdistances'  <- #{peek ResultT, rdistances} ptr
      vrneighbors'  <- #{peek ResultT, vrneighbors} ptr
      vrnsizes' <- #{peek ResultT, vrnsizes} ptr
      vfneighbors'  <- #{peek ResultT, vfneighbors} ptr
      vfnsizes' <- #{peek ResultT, vfnsizes} ptr
      vvneighbors'  <- #{peek ResultT, vvneighbors} ptr
      vvnsizes' <- #{peek ResultT, vvnsizes} ptr
      return Result { _dim = dim'
                    , _length = length'
                    , _indices = indices'
                    , _volumes = volumes'
                    , _owners = owners'
                    , __neighbors = neighbors'
                    , _centers = centers'
                    , _toporient = toporient'
                    , __ridges    = ridges'
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
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20)
      = do
          #{poke ResultT, dim}            ptr r1
          #{poke ResultT, length}         ptr r2
          #{poke ResultT, indices}        ptr r3
          #{poke ResultT, fvolumes}       ptr r4
          #{poke ResultT, owners}         ptr r5
          #{poke ResultT, neighbors}      ptr r6
          #{poke ResultT, centers}        ptr r7
          #{poke ResultT, toporient}      ptr r8
          #{poke ResultT, ridges}         ptr r9
          #{poke ResultT, rvolumes}       ptr r10
          #{poke ResultT, rcenters}       ptr r11
          #{poke ResultT, rnormals}       ptr r12
          #{poke ResultT, fnormals}       ptr r13
          #{poke ResultT, rdistances}     ptr r14
          #{poke ResultT, vrneighbors}    ptr r15
          #{poke ResultT, vrnsizes}       ptr r16
          #{poke ResultT, vfneighbors}    ptr r17
          #{poke ResultT, vfnsizes}       ptr r18
          #{poke ResultT, vvneighbors}    ptr r19
          #{poke ResultT, vvnsizes}       ptr r20
