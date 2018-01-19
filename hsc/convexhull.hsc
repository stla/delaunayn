{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( ConvexHull(..)
  , peekConvexHull
  , c_convexhull )
  where
import           Control.Monad         ((<$!>))
import Foreign
import Foreign.C.Types

#include "convexhull.h"

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __vertices :: Ptr CUInt
  , __nvertices :: CUInt
  , __faces :: Ptr (Ptr CUInt)
  , __facesizes :: Ptr CUInt
  , __nfaces :: CUInt
}

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

data ConvexHull = ConvexHull {
    _vertices :: [Int]
  , _faces :: [[Int]]
} deriving Show

instance Storable CConvexHull where
    sizeOf    __ = #{size ConvexHullT}
    alignment __ = #{alignment ConvexHullT}
    peek ptr = do
      dim'         <- #{peek ConvexHullT, dim}       ptr
      vertices'    <- #{peek ConvexHullT, vertices}  ptr
      nvertices'   <- #{peek ConvexHullT, nvertices} ptr
      faces'       <- #{peek ConvexHullT, faces}     ptr
      facesizes'   <- #{peek ConvexHullT, facesizes} ptr
      nfaces'      <- #{peek ConvexHullT, nfaces}    ptr
      return CConvexHull { __dim = dim'
                         , __vertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
                         , __facesizes = facesizes'
                         , __nfaces = nfaces'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6)
      = do
          #{poke ConvexHullT, dim}         ptr r1
          #{poke ConvexHullT, vertices}    ptr r2
          #{poke ConvexHullT, nvertices}   ptr r3
          #{poke ConvexHullT, faces}       ptr r4
          #{poke ConvexHullT, facesizes}   ptr r5
          #{poke ConvexHullT, nfaces}      ptr r6

peekConvexHull :: Ptr CConvexHull -> IO ConvexHull
peekConvexHull ptr = do
  cconvexhull <- peek ptr
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
  facesizes <- (<$!>) (map fromIntegral) (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (<$!>) (map fromIntegral) (peekArray nvertices (__vertices cconvexhull))
  faces' <- peekArray nfaces (__faces cconvexhull)
  faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  return ConvexHull { _vertices = vertices
                    , _faces = faces
                  }
