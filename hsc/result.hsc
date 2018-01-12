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
  _neighbors :: Ptr CUInt,
  _centers :: Ptr CDouble,
  _toporient :: Ptr CUInt,
  __ridges :: Ptr CUInt,
  _rcenters :: Ptr CDouble
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
      return Result { _dim = dim'
                    , _length = length'
                    , _indices = indices'
                    , _areas = areas'
                    , _neighbors = neighbors'
                    , _centers = centers'
                    , _toporient = toporient'
                    , __ridges    = ridges'
                    , _rcenters = rcenters' }
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7 r8 r9) = do
      #{poke ResultT, dim}         ptr r1
      #{poke ResultT, length}      ptr r2
      #{poke ResultT, indices}     ptr r3
      #{poke ResultT, areas}       ptr r4
      #{poke ResultT, neighbors}   ptr r5
      #{poke ResultT, centers}     ptr r6
      #{poke ResultT, toporient}   ptr r7
      #{poke ResultT, ridges}      ptr r8
      #{poke ResultT, rcenters}    ptr r9
