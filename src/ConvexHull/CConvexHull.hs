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
import Data.Tuple.Extra ((&&&))



type IndexMap = IntMap

data CVertex = CVertex {
    __id :: CUInt,
    __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (16)
{-# LINE 28 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 29 "convexhull.hsc" #-}
    peek ptr = do
      id'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 31 "convexhull.hsc" #-}
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8)    ptr
{-# LINE 32 "convexhull.hsc" #-}
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 37 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)      ptr r2
{-# LINE 38 "convexhull.hsc" #-}

data Vertex = Vertex {
    _id :: Int
  , _point :: [Double]
} deriving Show

cVertexToVertex :: Int -> CVertex -> IO Vertex
cVertexToVertex dim cvertex = do
  let id' = fromIntegral (__id cvertex)
  point <- (<$!>) (map realToFrac) (peekArray dim (__point cvertex))
  return Vertex { _id = id', _point = point }

data CEdge = CEdge {
    __v1 :: CVertex
  , __v2 :: CVertex
}

instance Storable CEdge where
    sizeOf    __ = (32)
{-# LINE 57 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 58 "convexhull.hsc" #-}
    peek ptr = do
      v1'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)    ptr
{-# LINE 60 "convexhull.hsc" #-}
      v2'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16)    ptr
{-# LINE 61 "convexhull.hsc" #-}
      return CEdge { __v1 = v1'
                    , __v2 = v2' }
    poke ptr (CEdge r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)      ptr r1
{-# LINE 66 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr r2
{-# LINE 67 "convexhull.hsc" #-}

type Edge = (Vertex, Vertex)

cEdgeToEdge :: Int -> CEdge -> IO Edge
cEdgeToEdge dim cridge = do
  v1 <- cVertexToVertex dim (__v1 cridge)
  v2 <- cVertexToVertex dim (__v2 cridge)
  return (v1, v2)

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __edges :: Ptr CEdge
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble -- & offset
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (56)
{-# LINE 89 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 90 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0)     ptr
{-# LINE 92 "convexhull.hsc" #-}
      edges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)       ptr
{-# LINE 93 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16)       ptr
{-# LINE 94 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24)       ptr
{-# LINE 95 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 32)         ptr
{-# LINE 96 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 97 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 98 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __center       = center'
                   , __normal       = normal'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 107 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)       ptr r2
{-# LINE 108 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 109 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 110 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)         ptr r5
{-# LINE 111 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)    ptr r6
{-# LINE 112 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
{-# LINE 113 "convexhull.hsc" #-}

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _edges :: [Edge]
  , _center :: [Double]
  , _normal :: [Double]
  , _area :: Double
  , _neighbors :: IntSet
} deriving Show

cFaceToFace :: Int -> Int -> CFace -> IO Face
cFaceToFace dim nvertices cface = do
  let area = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (mapM (cVertexToVertex dim))
                    (peekArray nvertices (__fvertices cface))
  edges  <- (=<<) (mapM (cEdgeToEdge dim))
                   (peekArray nvertices (__edges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  return Face { _fvertices = IM.fromAscList (map (_id &&& _point) vertices)
              , _edges    = edges
              , _center    = center
              , _normal    = normal
              , _area      = area
              , _neighbors = IS.fromAscList neighbors}

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
{-# LINE 153 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 154 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 156 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 157 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 158 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 159 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 160 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 161 "convexhull.hsc" #-}
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
{-# LINE 171 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 172 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 173 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 174 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 175 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 176 "convexhull.hsc" #-}

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

data ConvexHull = ConvexHull {
    _hvertices :: IndexMap [Double]
  , _faces :: IntMap Face
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
  return ConvexHull { _hvertices = IM.fromAscList (map (_id &&& _point) vertices)
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    }
