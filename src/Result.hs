{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import           Foreign
import           Foreign.C.Types
import           Foreign.Ptr

data Result = Result {
  _dim     :: CUInt,
  _length  :: CUInt,
  _indices :: Ptr CUInt,
  _areas   :: Ptr CDouble
} deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (24)
{-# LINE 19 "result.hsc" #-}
    alignment _ = 8
{-# LINE 20 "result.hsc" #-}
    peek ptr = do
      dim' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 22 "result.hsc" #-}
      length' <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 23 "result.hsc" #-}
      indices' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
{-# LINE 24 "result.hsc" #-}
      areas' <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 25 "result.hsc" #-}
      return Result { _dim = dim', _length = length', _indices = indices', _areas = areas' }
    poke ptr (Result r1 r2 r3 r4) = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 28 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr r2
{-# LINE 29 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r3
{-# LINE 30 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r4
{-# LINE 31 "result.hsc" #-}
--      pokeArray (#{ptr ResultT, indices} ptr) r3
