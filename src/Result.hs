{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import           Foreign
import           Foreign.C.Types

data Result = Result {
  _dim          :: CUInt,
  _length       :: CUInt,
  _indices      :: Ptr CUInt,
  _volumes      :: Ptr CDouble,
  _owners       :: Ptr CUInt,
  __neighbors   :: Ptr CUInt,
  _centers      :: Ptr CDouble,
  _toporient    :: Ptr CUInt,
  __ridges      :: Ptr CUInt,
  _areas        :: Ptr CDouble,
  _rcenters     :: Ptr CDouble,
  _rnormals     :: Ptr CDouble,
  _fnormals     :: Ptr CDouble,
  _rdistances   :: Ptr CDouble,
  __vrneighbors :: Ptr CUInt,
  _vrnsizes     :: Ptr CUInt,
  __vfneighbors  :: Ptr CUInt,
  _vfnsizes     :: Ptr CUInt
} -- deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (136)
{-# LINE 32 "result.hsc" #-}
    alignment _ = 8
{-# LINE 33 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 35 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 36 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
{-# LINE 37 "result.hsc" #-}
      volumes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 38 "result.hsc" #-}
      owners'   <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 39 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 40 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 41 "result.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 42 "result.hsc" #-}
      ridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 43 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 44 "result.hsc" #-}
      rcenters'  <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 45 "result.hsc" #-}
      rnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 46 "result.hsc" #-}
      fnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 47 "result.hsc" #-}
      rdistances'  <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 48 "result.hsc" #-}
      vrneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 49 "result.hsc" #-}
      vrnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 50 "result.hsc" #-}
      vfneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 51 "result.hsc" #-}
      vfnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 128) ptr
{-# LINE 52 "result.hsc" #-}
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
                  }
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)            ptr r1
{-# LINE 74 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 4)         ptr r2
{-# LINE 75 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r3
{-# LINE 76 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r4
{-# LINE 77 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)         ptr r5
{-# LINE 78 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r6
{-# LINE 79 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)        ptr r7
{-# LINE 80 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)      ptr r8
{-# LINE 81 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)         ptr r9
{-# LINE 82 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)       ptr r10
{-# LINE 83 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72)       ptr r11
{-# LINE 84 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80)       ptr r12
{-# LINE 85 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88)       ptr r13
{-# LINE 86 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 96)     ptr r14
{-# LINE 87 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 104)    ptr r15
{-# LINE 88 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 112)       ptr r16
{-# LINE 89 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 120)    ptr r17
{-# LINE 90 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 128)       ptr r18
{-# LINE 91 "result.hsc" #-}
