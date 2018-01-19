{-# LINE 1 "result.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module CDelaunay
  where
import           Foreign
import           Foreign.C.Types

data CDelaunay = CDelaunay {
  _dim          :: CUInt,
  _length       :: CUInt,
  _indices      :: Ptr CUInt,
  _volumes      :: Ptr CDouble,
  _owners       :: Ptr CUInt,
  __neighbors   :: Ptr CUInt,
  _centers      :: Ptr CDouble,
  _toporient    :: Ptr CUInt,
  __ridges      :: Ptr CUInt,
  _nridges      :: CUInt,
  _areas        :: Ptr CDouble,
  _rcenters     :: Ptr CDouble,
  _rnormals     :: Ptr CDouble,
  _fnormals     :: Ptr CDouble,
  _rdistances   :: Ptr CDouble,
  __vrneighbors :: Ptr CUInt,
  _vrnsizes     :: Ptr CUInt,
  __vfneighbors :: Ptr CUInt,
  _vfnsizes     :: Ptr CUInt,
  _vvneighbors  :: Ptr CUInt,
  _vvnsizes     :: Ptr CUInt
} -- deriving (Show, Eq)

instance Storable CDelaunay where
    sizeOf    _ = (160)
{-# LINE 35 "result.hsc" #-}
    alignment _ = 8
{-# LINE 36 "result.hsc" #-}
    peek ptr = do
      dim'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 38 "result.hsc" #-}
      length'    <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 39 "result.hsc" #-}
      indices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr -- peekArray (fromIntegral $ length' * (dim'+1)) $ #{ptr CDelaunayT, indices} ptr
{-# LINE 40 "result.hsc" #-}
      volumes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 41 "result.hsc" #-}
      owners'   <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 42 "result.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 43 "result.hsc" #-}
      centers'   <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 44 "result.hsc" #-}
      toporient' <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 45 "result.hsc" #-}
      ridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 46 "result.hsc" #-}
      nridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 152) ptr
{-# LINE 47 "result.hsc" #-}
      areas'     <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 48 "result.hsc" #-}
      rcenters'  <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 49 "result.hsc" #-}
      rnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 50 "result.hsc" #-}
      fnormals'  <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 51 "result.hsc" #-}
      rdistances'  <- (\hsc_ptr -> peekByteOff hsc_ptr 96) ptr
{-# LINE 52 "result.hsc" #-}
      vrneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 104) ptr
{-# LINE 53 "result.hsc" #-}
      vrnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 112) ptr
{-# LINE 54 "result.hsc" #-}
      vfneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 120) ptr
{-# LINE 55 "result.hsc" #-}
      vfnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 128) ptr
{-# LINE 56 "result.hsc" #-}
      vvneighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 136) ptr
{-# LINE 57 "result.hsc" #-}
      vvnsizes' <- (\hsc_ptr -> peekByteOff hsc_ptr 144) ptr
{-# LINE 58 "result.hsc" #-}
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
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)            ptr r1
{-# LINE 83 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 4)         ptr r2
{-# LINE 84 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r3
{-# LINE 85 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r4
{-# LINE 86 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)         ptr r5
{-# LINE 87 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r6
{-# LINE 88 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)        ptr r7
{-# LINE 89 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)      ptr r8
{-# LINE 90 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)         ptr r9
{-# LINE 91 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)       ptr r10
{-# LINE 92 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72)       ptr r11
{-# LINE 93 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80)       ptr r12
{-# LINE 94 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88)       ptr r13
{-# LINE 95 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 96)     ptr r14
{-# LINE 96 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 104)    ptr r15
{-# LINE 97 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 112)       ptr r16
{-# LINE 98 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 120)    ptr r17
{-# LINE 99 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 128)       ptr r18
{-# LINE 100 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 136)    ptr r19
{-# LINE 101 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 144)       ptr r20
{-# LINE 102 "result.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 152)       ptr r21
{-# LINE 103 "result.hsc" #-}
