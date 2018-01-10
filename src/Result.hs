{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import           Foreign
import           Foreign.C.Types
--import           Foreign.Ptr

data Result = Result {
  _dim       :: CUInt,
  _length    :: CUInt,
  _indices   :: Ptr CUInt,
  _areas     :: Ptr CDouble,
  _neighbors :: Ptr CUInt,
  _centers   :: Ptr CDouble
} deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (40)
{-# LINE 21 "result.hsc" #-}
    alignment _ = 8
{-# LINE 22 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 24 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 25 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 26 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 27 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 28 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 29 "result.hsc" #-}
      return Result { _dim = dim'
                    , _length = length'
                    , _indices = indices'
                    , _areas = areas'
                    , _neighbors = neighbors'
                    , _centers = centers'  }
    poke ptr (Result r1 r2 r3 r4 r5 r6) = do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0)       ptr r1
{-# LINE 37 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4)    ptr r2
{-# LINE 38 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 8)   ptr r3
{-# LINE 39 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 16)     ptr r4
{-# LINE 40 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r5
{-# LINE 41 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r6
{-# LINE 42 "result.hsc" #-}
--      pokeArray (#{ptr ResultT, indices} ptr) r3
