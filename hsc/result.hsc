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
  _areas :: Ptr CDouble,
  __neighbors :: Ptr CUInt,
  _centers :: Ptr CDouble,
  _toporient :: Ptr CUInt,
  __ridges :: Ptr CUInt,
  _rcenters :: Ptr CDouble,
  _rnormals :: Ptr CDouble,
  _fnormals :: Ptr CDouble,
  _rdistances :: Ptr CDouble,
  _vrneighbors :: Ptr CUInt,
  _vrnsizes :: Ptr CUInt
} deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = #{size ResultT}
    alignment _ = #{alignment ResultT}
    peek ptr = do
      dim'       <- #{peek ResultT, dim} ptr
      length'    <- #{peek ResultT, length} ptr
      indices'   <- #{peek ResultT, indices} ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
      areas'     <- #{peek ResultT, areas} ptr
      neighbors' <- #{peek ResultT, neighbors} ptr
      centers'   <- #{peek ResultT, centers} ptr
      toporient' <- #{peek ResultT, toporient} ptr
      ridges'    <- #{peek ResultT, ridges} ptr
      rcenters'  <- #{peek ResultT, rcenters} ptr
      rnormals'  <- #{peek ResultT, rnormals} ptr
      fnormals'  <- #{peek ResultT, fnormals} ptr
      rdistances'  <- #{peek ResultT, rdistances} ptr
      vrneighbors'  <- #{peek ResultT, vrneighbors} ptr
      vrnsizes' <- #{peek ResultT, vrnsizes} ptr
      return Result { _dim = dim'
                    , _length = length'
                    , _indices = indices'
                    , _areas = areas'
                    , __neighbors = neighbors'
                    , _centers = centers'
                    , _toporient = toporient'
                    , __ridges    = ridges'
                    , _rcenters = rcenters'
                    , _rnormals = rnormals'
                    , _fnormals = fnormals'
                    , _rdistances = rdistances'
                    , _vrneighbors = vrneighbors'
                    , _vrnsizes = vrnsizes' }
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14) = do
      #{poke ResultT, dim}            ptr r1
      #{poke ResultT, length}         ptr r2
      #{poke ResultT, indices}        ptr r3
      #{poke ResultT, areas}          ptr r4
      #{poke ResultT, neighbors}      ptr r5
      #{poke ResultT, centers}        ptr r6
      #{poke ResultT, toporient}      ptr r7
      #{poke ResultT, ridges}         ptr r8
      #{poke ResultT, rcenters}       ptr r9
      #{poke ResultT, rnormals}       ptr r10
      #{poke ResultT, fnormals}       ptr r11
      #{poke ResultT, rdistances}     ptr r12
      #{poke ResultT, vrneighbors}    ptr r13
      #{poke ResultT, vrnsizes}       ptr r14
