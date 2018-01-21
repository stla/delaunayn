{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( peekConvexHull
  , c_convexhull )
  where
import           Control.Monad         ((<$!>), (=<<))
import ConvexHull.Types
import Foreign
import Foreign.C.Types
import Data.List
import Data.List.Index (imapM)
import           Data.IntMap.Strict    (IntMap)
import qualified Data.IntMap.Strict    as IM
import qualified Data.IntSet           as IS

#include "convexhull.h"

data CVertex = CVertex {
    __id :: CUInt
  , __point :: Ptr CDouble
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

data CVertex' = CVertex' {
    __id' :: CUInt
  , __point' :: Ptr CDouble
  , __neighfacets :: Ptr CUInt
  , __nneighfacets :: CUInt
  , __neighvertices :: Ptr CUInt
  , __nneighvertices :: CUInt
  , __neighedges :: Ptr CUInt
  , __nneighedges :: CUInt
}

instance Storable CVertex' where
    sizeOf    __ = #{size FullVertexT}
    alignment __ = #{alignment FullVertexT}
    peek ptr = do
      id'              <- #{peek FullVertexT, id}              ptr
      point'           <- #{peek FullVertexT, point}           ptr
      neighfacets'     <- #{peek FullVertexT, neighfacets}     ptr
      nneighfacets'    <- #{peek FullVertexT, nneighfacets}    ptr
      neighvertices'   <- #{peek FullVertexT, neighvertices}   ptr
      nneighsvertices' <- #{peek FullVertexT, nneighsvertices} ptr
      neighedges'      <- #{peek FullVertexT, neighedges}      ptr
      nneighedges'     <- #{peek FullVertexT, nneighedges}     ptr
      return CVertex' { __id' = id'
                      , __point' = point'
                      , __neighfacets = neighfacets'
                      , __nneighfacets = nneighfacets'
                      , __neighvertices = neighvertices'
                      , __nneighvertices = nneighsvertices'
                      , __neighedges = neighedges'
                      , __nneighedges = nneighedges'
                      }
    poke ptr (CVertex' r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          #{poke FullVertexT, id}               ptr r1
          #{poke FullVertexT, point}            ptr r2
          #{poke FullVertexT, neighfacets}      ptr r3
          #{poke FullVertexT, nneighfacets}     ptr r4
          #{poke FullVertexT, neighvertices}    ptr r5
          #{poke FullVertexT, nneighsvertices}  ptr r6
          #{poke FullVertexT, neighedges}       ptr r7
          #{poke FullVertexT, nneighedges}      ptr r8

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids             = map (fromIntegral . __id') cvertices
      nneighfacets    = map (fromIntegral . __nneighfacets) cvertices
      nneighsvertices = map (fromIntegral . __nneighvertices) cvertices
      nneighedges     = map (fromIntegral . __nneighedges) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point' cv)))
                 cvertices
  neighfacets <- mapM (\(i, cv) -> (<$!>) (map fromIntegral)
                                          (peekArray i (__neighfacets cv)))
                       (zip nneighfacets cvertices)
  neighvertices <- mapM (\(i, cv) ->
                          (<$!>) (map fromIntegral)
                                 (peekArray i (__neighvertices cv)))
                        (zip nneighsvertices cvertices)
  neighedges <- mapM (\(i, cv) ->
                       (<$!>) (map fromIntegral)
                              (peekArray i (__neighedges cv)))
                     (zip nneighedges cvertices)
  return $ IM.fromList (zip ids
                            (map (\(pt, fneighs, vneighs, eneighs) ->
                                  Vertex { _point = pt
                                         , _neighfaces =
                                              IS.fromAscList fneighs
                                         , _neighvertices =
                                              IS.fromAscList vneighs
                                         , _neighedges =
                                              IS.fromAscList eneighs
                                         })
                                  (zip4 points neighfacets neighvertices
                                        neighedges)))

data CRidge = CRidge {
    __rvertices :: Ptr CVertex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CUInt
}

instance Storable CRidge where
    sizeOf    __ = #{size RidgeT}
    alignment __ = #{alignment RidgeT}
    peek ptr = do
      rvertices <- #{peek RidgeT, vertices} ptr
      ridgeOf1' <- #{peek RidgeT, ridgeOf1} ptr
      ridgeOf2' <- #{peek RidgeT, ridgeOf2} ptr
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1 = ridgeOf1'
                    , __ridgeOf2 = ridgeOf2' }
    poke ptr (CRidge r1 r2 r3)
      = do
          #{poke RidgeT, vertices} ptr r1
          #{poke RidgeT, ridgeOf1} ptr r2
          #{poke RidgeT, ridgeOf2} ptr r3

cRidgeToRidge :: Int -> CRidge -> IO Ridge
cRidgeToRidge dim cridge = do
  let f1 = fromIntegral $ __ridgeOf1 cridge
      f2 = fromIntegral $ __ridgeOf2 cridge
  vertices <- peekArray (dim-1) (__rvertices cridge)
  rvertices <- cVerticesToMap dim vertices
  return Ridge { _rvertices = rvertices
               , _ridgeOf = IS.fromAscList [f1,f2] }

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __edges :: Ptr CRidge
  , __nedges' :: CUInt
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
  , __family :: CInt
}

instance Storable CFace where
    sizeOf    __ = #{size FaceT}
    alignment __ = #{alignment FaceT}
    peek ptr = do
      fvertices' <- #{peek FaceT, vertices} ptr
      edges'     <- #{peek FaceT, edges} ptr
      nedges'    <- #{peek FaceT, nedges} ptr
      center'    <- #{peek FaceT, center} ptr
      normal'    <- #{peek FaceT, normal} ptr
      offset'    <- #{peek FaceT, offset} ptr
      area'      <- #{peek FaceT, area} ptr
      neighbors' <- #{peek FaceT, neighbors} ptr
      neighsize  <- #{peek FaceT, neighborsize} ptr
      family'    <- #{peek FaceT, family} ptr
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __nedges'      = nedges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize
                   , __family       = family' }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8 r9 r10)
      = do
          #{poke FaceT, vertices}     ptr r1
          #{poke FaceT, edges}        ptr r2
          #{poke FaceT, nedges}       ptr r3
          #{poke FaceT, center}       ptr r4
          #{poke FaceT, normal}       ptr r5
          #{poke FaceT, offset}       ptr r6
          #{poke FaceT, area}         ptr r7
          #{poke FaceT, neighbors}    ptr r8
          #{poke FaceT, neighborsize} ptr r9
          #{poke FaceT, family}       ptr r10

cFaceToFace :: Int -> Int -> CFace -> IO Face
cFaceToFace dim nvertices cface = do
  let area      = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
      offset    = realToFrac (__offset cface)
      family    = fromIntegral (__family cface)
      nedges    = fromIntegral (__nedges' cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  edges  <- (=<<) (mapM (cRidgeToRidge dim))
                  (peekArray nedges (__edges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  return Face { _fvertices = vertices
              , _edges     = edges
              , _centroid  = center
              , _normal    = normal
              , _offset    = offset
              , _area      = area
              , _neighbors = IS.fromAscList neighbors
              , _family    = if family == -1 then Nothing else Just family }

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex'
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
  , __facesizes :: Ptr CUInt
  , __nfaces :: CUInt
  , __alledges :: Ptr CRidge
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
  alledges <- (=<<) (mapM (cRidgeToRidge dim))
                          (peekArray nedges (__alledges cconvexhull))
  return ConvexHull { _allvertices = vertices
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _alledges = IM.fromAscList (zip [0 .. nedges-1] alledges)
                    }
