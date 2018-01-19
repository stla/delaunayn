{-# LINE 1 "convexhull.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( ConvexHull(..)
  , peekConvexHull
  , c_convexhull )
  where
import           Control.Monad         ((<$!>), (=<<))
import Foreign
import Foreign.C.Types
import Data.List.Index (imapM)
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IS



type IndexMap = IntMap

data CVertex = CVertex {
    __id :: CUInt,
    __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (16)
{-# LINE 27 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 28 "convexhull.hsc" #-}
    peek ptr = do
      id'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 30 "convexhull.hsc" #-}
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8)    ptr
{-# LINE 31 "convexhull.hsc" #-}
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 36 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)      ptr r2
{-# LINE 37 "convexhull.hsc" #-}

data Vertex = Vertex {
    _id :: Int,
    _point :: [Double]
} deriving Show

cVertexToVertex :: Int -> CVertex -> IO Vertex
cVertexToVertex dim cvertex = do
  let id' = fromIntegral (__id cvertex)
  point <- (<$!>) (map realToFrac) (peekArray dim (__point cvertex))
  return Vertex { _id = id'
                , _point = point }

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble -- & offset
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (48)
{-# LINE 61 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 62 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0)  ptr
{-# LINE 64 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)    ptr
{-# LINE 65 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16)    ptr
{-# LINE 66 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 24)      ptr
{-# LINE 67 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 68 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 69 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __center       = center'
                   , __normal       = normal'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize }
    poke ptr (CFace r1 r2 r3 r4 r5 r6)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 78 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)       ptr r2
{-# LINE 79 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 80 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)         ptr r4
{-# LINE 81 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)    ptr r5
{-# LINE 82 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 83 "convexhull.hsc" #-}

data Face = Face {
    _fvertices :: [Vertex]
  , _center :: [Double]
  , _normal :: [Double]
  , _area :: Double
} deriving Show

cFaceToFace :: Int -> Int -> CFace -> IO Face
cFaceToFace dim nvertices cface = do
  let area = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (mapM (cVertexToVertex dim))
                    (peekArray nvertices (__fvertices cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  return Face { _fvertices = vertices
              , _center = center
              , _normal = normal
              , _area = area}

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __hvertices :: Ptr CVertex
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
  , __facesizes :: Ptr CUInt
  , __nfaces :: CUInt
}

instance Storable CConvexHull where
    sizeOf    __ = (48)
{-# LINE 117 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 118 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 120 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 121 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 122 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 123 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 124 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 125 "convexhull.hsc" #-}
      return CConvexHull { __dim = dim'
                         , __hvertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
                         , __facesizes = facesizes'
                         , __nfaces = nfaces'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 135 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 136 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 137 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 138 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 139 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 140 "convexhull.hsc" #-}

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

data ConvexHull = ConvexHull {
    _hvertices :: [Vertex]
  , _faces :: [Face]
} deriving Show


peekConvexHull :: Ptr CConvexHull -> IO ConvexHull
peekConvexHull ptr = do
  cconvexhull <- peek ptr
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
  facesizes <- (<$!>) (map fromIntegral)
                      (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (=<<) (mapM (cVertexToVertex dim))
                    (peekArray nvertices (__hvertices cconvexhull))
  faces <- (=<<) (imapM (\i cface -> cFaceToFace dim (facesizes !! i) cface))
                        (peekArray nfaces (__faces cconvexhull))
  --faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  return ConvexHull { _hvertices = vertices
                    , _faces = faces
                    }
