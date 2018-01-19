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

#include "convexhull.h"

type IndexMap = IntMap

data CVertex = CVertex {
    __id :: CUInt,
    __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = #{size VertexT}
    alignment __ = #{alignment VertexT}
    peek ptr = do
      id'     <- #{peek VertexT, id}       ptr
      point'  <- #{peek VertexT, point}    ptr
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          #{poke VertexT, id}         ptr r1
          #{poke VertexT, point}      ptr r2

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
    sizeOf    __ = #{size EdgeT}
    alignment __ = #{alignment EdgeT}
    peek ptr = do
      v1'     <- #{peek EdgeT, v1}    ptr
      v2'     <- #{peek EdgeT, v2}    ptr
      return CEdge { __v1 = v1'
                    , __v2 = v2' }
    poke ptr (CEdge r1 r2)
      = do
          #{poke EdgeT, v1}      ptr r1
          #{poke EdgeT, v2}      ptr r2

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
    sizeOf    __ = #{size FaceT}
    alignment __ = #{alignment FaceT}
    peek ptr = do
      fvertices' <- #{peek FaceT, vertices}     ptr
      edges'     <- #{peek FaceT, edges}       ptr
      center'    <- #{peek FaceT, center}       ptr
      normal'    <- #{peek FaceT, normal}       ptr
      area'      <- #{peek FaceT, area}         ptr
      neighbors' <- #{peek FaceT, neighbors}    ptr
      neighsize  <- #{peek FaceT, neighborsize} ptr
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __center       = center'
                   , __normal       = normal'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7)
      = do
          #{poke FaceT, vertices}     ptr r1
          #{poke FaceT, edges}        ptr r2
          #{poke FaceT, center}       ptr r3
          #{poke FaceT, normal}       ptr r4
          #{poke FaceT, area}         ptr r5
          #{poke FaceT, neighbors}    ptr r6
          #{poke FaceT, neighborsize} ptr r7

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
              , _edges     = edges
              , _center    = center
              , _normal    = normal
              , _area      = area
              , _neighbors = IS.fromAscList neighbors}

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
  , __facesizes :: Ptr CUInt
  , __nfaces :: CUInt
  , __alledges :: Ptr CEdge
  , __nedges :: CUInt
}

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
      alledges'    <- #{peek ConvexHullT, edges}     ptr
      nedges'      <- #{peek ConvexHullT, nedges}    ptr
      return CConvexHull { __dim = dim'
                         , __allvertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
                         , __facesizes = facesizes'
                         , __nfaces = nfaces'
                         , __alledges = alledges'
                         , __nedges = nedges'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          #{poke ConvexHullT, dim}         ptr r1
          #{poke ConvexHullT, vertices}    ptr r2
          #{poke ConvexHullT, nvertices}   ptr r3
          #{poke ConvexHullT, faces}       ptr r4
          #{poke ConvexHullT, facesizes}   ptr r5
          #{poke ConvexHullT, nfaces}      ptr r6
          #{poke ConvexHullT, edges}       ptr r7
          #{poke ConvexHullT, nedges}      ptr r8

foreign import ccall unsafe "convexHull" c_convexhull
  :: Ptr CDouble -- points
  -> CUInt -- dim
  -> CUInt -- npoints
  -> CUInt -- triangulate
  -> Ptr CUInt -- exitcode
  -> IO (Ptr CConvexHull)

data ConvexHull = ConvexHull {
    _allvertices :: IndexMap [Double]
  , _faces :: IntMap Face
  , _alledges :: [Edge]
} deriving Show


peekConvexHull :: Ptr CConvexHull -> IO ConvexHull
peekConvexHull ptr = do
  cconvexhull <- peek ptr
  let dim       = fromIntegral (__dim cconvexhull)
      nvertices = fromIntegral (__nvertices cconvexhull)
      nfaces    = fromIntegral (__nfaces cconvexhull)
      nedges    = fromIntegral (__nedges cconvexhull)
  facesizes <- (<$!>) (map fromIntegral)
                      (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (=<<) (mapM (cVertexToVertex dim))
                    (peekArray nvertices (__allvertices cconvexhull))
  faces <- (=<<) (imapM (\i cface -> cFaceToFace dim (facesizes !! i) cface))
                        (peekArray nfaces (__faces cconvexhull))
  --faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  alledges <- (=<<) (mapM (cEdgeToEdge dim))
                          (peekArray nedges (__alledges cconvexhull))
  return ConvexHull { _allvertices = IM.fromAscList
                                     (map (_id &&& _point) vertices)
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _alledges = alledges
                    }
