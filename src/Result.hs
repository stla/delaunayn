{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import           Foreign
import           Foreign.C.Types

data Result = Result {
  _dim       :: CUInt,
  _length    :: CUInt,
  _indices   :: Ptr CUInt,
  _areas     :: Ptr CDouble,
  _neighbors :: Ptr CUInt,
  _centers   :: Ptr CDouble,
  _toporient :: Ptr CUInt
} deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (48)
{-# LINE 21 "result.hsc" #-}
    alignment _ = 8
{-# LINE 22 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 24 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 25 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
{-# LINE 26 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 27 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 28 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 29 "result.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 30 "result.hsc" #-}
      return Result { _dim = dim'
                    , _length = length'
                    , _indices = indices'
                    , _areas = areas'
                    , _neighbors = neighbors'
                    , _centers = centers'
                    , _toporient = toporient' }
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7) = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 39 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4)      ptr r2
{-# LINE 40 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 8)     ptr r3
{-# LINE 41 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r4
{-# LINE 42 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 24)   ptr r5
{-# LINE 43 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 32)     ptr r6
{-# LINE 44 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 40)   ptr r7
{-# LINE 45 "result.hsc" #-}
