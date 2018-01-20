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

--data Vertex = Vertex {
--    _id :: Int
--  , _point :: [Double]
--} deriving Show

cVerticesToMap :: Int -> [CVertex] -> IO (IntMap [Double])
cVerticesToMap dim cvertices = do
  let ids = map (fromIntegral . __id) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point cv)))
                 cvertices
  return $ IM.fromAscList (zip ids points)
--
data CVertex' = CVertex' {
    __id' :: CUInt
  , __point':: Ptr CDouble
  , __neighfacets :: Ptr CUInt
  , __nneighfacets :: CUInt
}

instance Storable CVertex' where
    sizeOf    __ = #{size FullVertexT}
    alignment __ = #{alignment FullVertexT}
    peek ptr = do
      id'           <- #{peek FullVertexT, id}         ptr
      point'        <- #{peek FullVertexT, point}      ptr
      neighfacets'  <- #{peek FullVertexT, neighfacets} ptr
      nneighfacets' <- #{peek FullVertexT, nneighfacets} ptr
      return CVertex' { __id' = id'
                     , __point' = point'
                     , __neighfacets = neighfacets'
                     , __nneighfacets = nneighfacets' }
    poke ptr (CVertex' r1 r2 r3 r4)
      = do
          #{poke FullVertexT, id}            ptr r1
          #{poke FullVertexT, point}         ptr r2
          #{poke FullVertexT, neighfacets}   ptr r3
          #{poke FullVertexT, nneighfacets}  ptr r4

data Vertex = Vertex {
    _point :: [Double]
  , _neighfacets :: IntSet
} deriving Show

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids          = map (fromIntegral . __id') cvertices
      nneighfacets = map (fromIntegral . __nneighfacets) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point' cv)))
                 cvertices
  neighfacets <- mapM (\(i, cv) -> (<$!>) (map fromIntegral)
                                          (peekArray i (__neighfacets cv)))
                       (zip nneighfacets cvertices)
  return $ IM.fromList (zip ids
                            (map (\(pt, neighs) ->
                                  Vertex { _point = pt
                                         , _neighfacets =
                                            IS.fromAscList neighs})
                                  (zip points neighfacets)))

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

type Edge = IntMap [Double]

cEdgeToEdge :: Int -> CEdge -> IO Edge
cEdgeToEdge dim cridge = do
  let v1 = __v1 cridge
      v2 = __v2 cridge
  cVerticesToMap dim [v1,v2]

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __edges :: Ptr CEdge
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
}

instance Storable CFace where
    sizeOf    __ = #{size FaceT}
    alignment __ = #{alignment FaceT}
    peek ptr = do
      fvertices' <- #{peek FaceT, vertices}     ptr
      edges'     <- #{peek FaceT, edges}        ptr
      center'    <- #{peek FaceT, center}       ptr
      normal'    <- #{peek FaceT, normal}       ptr
      offset'    <- #{peek FaceT, offset}       ptr
      area'      <- #{peek FaceT, area}         ptr
      neighbors' <- #{peek FaceT, neighbors}    ptr
      neighsize  <- #{peek FaceT, neighborsize} ptr
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          #{poke FaceT, vertices}     ptr r1
          #{poke FaceT, edges}        ptr r2
          #{poke FaceT, center}       ptr r3
          #{poke FaceT, normal}       ptr r4
          #{poke FaceT, offset}       ptr r5
          #{poke FaceT, area}         ptr r6
          #{poke FaceT, neighbors}    ptr r7
          #{poke FaceT, neighborsize} ptr r8

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _edges :: [Edge]
  , _center :: [Double]
  , _normal :: [Double]
  , _offset :: Double
  , _area :: Double
  , _neighbors :: IntSet
} deriving Show

cFaceToFace :: Int -> Int -> CFace -> IO Face
cFaceToFace dim nvertices cface = do
  let area      = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
      offset    = realToFrac (__offset cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  edges  <- (=<<) (mapM (cEdgeToEdge dim))
                   (peekArray nvertices (__edges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  return Face { _fvertices = vertices
              , _edges     = edges
              , _center    = center
              , _normal    = normal
              , _offset    = offset
              , _area      = area
              , _neighbors = IS.fromAscList neighbors}

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex'
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
    _allvertices :: IndexMap Vertex
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
  vertices <- (=<<) (cVerticesToVertexMap dim)
                    (peekArray nvertices (__allvertices cconvexhull))
  faces <- (=<<) (imapM (\i cface -> cFaceToFace dim (facesizes !! i) cface))
                        (peekArray nfaces (__faces cconvexhull))
  --faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  alledges <- (=<<) (mapM (cEdgeToEdge dim))
                          (peekArray nedges (__alledges cconvexhull))
  return ConvexHull { _allvertices = vertices
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _alledges = alledges
                    }
