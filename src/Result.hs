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
  _toporient :: Ptr CUInt,
  __ridges   :: Ptr CUInt,
  _rcenters  :: Ptr CDouble
} deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (64)
{-# LINE 23 "result.hsc" #-}
    alignment _ = 8
{-# LINE 24 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 26 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 27 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
{-# LINE 28 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 29 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 30 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 31 "result.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 32 "result.hsc" #-}
      ridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 33 "result.hsc" #-}
      rcenters'  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 34 "result.hsc" #-}
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
      (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 45 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4)      ptr r2
{-# LINE 46 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 8)     ptr r3
{-# LINE 47 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r4
{-# LINE 48 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 24)   ptr r5
{-# LINE 49 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 32)     ptr r6
{-# LINE 50 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 40)   ptr r7
{-# LINE 51 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 48)      ptr r8
{-# LINE 52 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 56)    ptr r9
{-# LINE 53 "result.hsc" #-}
