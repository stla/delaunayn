{-# LINE 1 "convexhull.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( ConvexHull(..)
  , peekConvexHull
  , c_convexhull )
  where
import           Control.Monad   ((<$!>))
import           Foreign
import           Foreign.C.Types

data CConvexHull = CConvexHull {
    __dim       :: CUInt
  , __vertices  :: Ptr CUInt
  , __nvertices :: CUInt
  , __faces     :: Ptr (Ptr CUInt)
  , __facesizes :: Ptr CUInt
  , __nfaces    :: CUInt
}

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

data ConvexHull = ConvexHull {
    _vertices  :: [Int]
  , _faces     :: [[Int]]
} deriving Show

instance Storable CConvexHull where
    sizeOf    __ = (48)
{-# LINE 40 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 41 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 43 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 44 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 45 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 46 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 47 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 48 "convexhull.hsc" #-}
      return CConvexHull { __dim = dim'
                         , __vertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
                         , __facesizes = facesizes'
                         , __nfaces = nfaces'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 58 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 59 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 60 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 61 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 62 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 63 "convexhull.hsc" #-}

peekConvexHull :: Ptr CConvexHull -> IO ConvexHull
peekConvexHull ptr = do
  cconvexhull <- peek ptr
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
  facesizes <- (<$!>) (map fromIntegral) (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (<$!>) (map fromIntegral) (peekArray nvertices (__vertices cconvexhull))
  faces' <- peekArray nfaces (__faces cconvexhull)
  faces <- mapM (\i -> (<$!>) (map fromIntegral)
                              (peekArray (facesizes !! i) (faces' !! i)))
                                         [0 .. nfaces-1]
  return ConvexHull { _vertices = vertices
                    , _faces = faces
                  }
