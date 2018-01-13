{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Result
  where
import Foreign
import Foreign.C.Types



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
  __vrneighbors :: Ptr CUInt,
  _vrnsizes :: Ptr CUInt,
  _vfneighbors :: Ptr CUInt,
  _vfnsizes :: Ptr CUInt
} -- deriving (Show, Eq)

instance Storable Result where
    sizeOf    _ = (120)
{-# LINE 30 "result.hsc" #-}
    alignment _ = 8
{-# LINE 31 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 33 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 34 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr ResultT, indices} ptr
{-# LINE 35 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 36 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 37 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 38 "result.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 39 "result.hsc" #-}
      ridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 40 "result.hsc" #-}
      rcenters'  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 41 "result.hsc" #-}
      rnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 42 "result.hsc" #-}
      fnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 43 "result.hsc" #-}
      rdistances'  <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 44 "result.hsc" #-}
      vrneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 45 "result.hsc" #-}
      vrnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 46 "result.hsc" #-}
      vfneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 47 "result.hsc" #-}
      vfnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 48 "result.hsc" #-}
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
                    , __vrneighbors = vrneighbors'
                    , _vrnsizes = vrnsizes'
                    , __vfneighbors = vfneighbors'
                    , _vfnsizes = vfnsizes'
                  }
    poke ptr (Result r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16) =
    do
      (\hsc_ptr -> pokeByteOff hsc_ptr 0)            ptr r1
{-# LINE 68 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 4)         ptr r2
{-# LINE 69 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r3
{-# LINE 70 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 16)          ptr r4
{-# LINE 71 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 24)      ptr r5
{-# LINE 72 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 32)        ptr r6
{-# LINE 73 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r7
{-# LINE 74 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 48)         ptr r8
{-# LINE 75 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 56)       ptr r9
{-# LINE 76 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 64)       ptr r10
{-# LINE 77 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 72)       ptr r11
{-# LINE 78 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 80)     ptr r12
{-# LINE 79 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 88)    ptr r13
{-# LINE 80 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 96)       ptr r14
{-# LINE 81 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 104)    ptr r15
{-# LINE 82 "result.hsc" #-}
      (\hsc_ptr -> pokeByteOff hsc_ptr 112)       ptr r16
{-# LINE 83 "result.hsc" #-}
