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
import qualified Data.Set as S

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
  , __neighridges :: Ptr CUInt
  , __nneighridges :: CUInt
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
      neighridges'      <- #{peek FullVertexT, neighridges}      ptr
      nneighridges'     <- #{peek FullVertexT, nneighridges}     ptr
      return CVertex' { __id' = id'
                      , __point' = point'
                      , __neighfacets = neighfacets'
                      , __nneighfacets = nneighfacets'
                      , __neighvertices = neighvertices'
                      , __nneighvertices = nneighsvertices'
                      , __neighridges = neighridges'
                      , __nneighridges = nneighridges'
                      }
    poke ptr (CVertex' r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          #{poke FullVertexT, id}               ptr r1
          #{poke FullVertexT, point}            ptr r2
          #{poke FullVertexT, neighfacets}      ptr r3
          #{poke FullVertexT, nneighfacets}     ptr r4
          #{poke FullVertexT, neighvertices}    ptr r5
          #{poke FullVertexT, nneighsvertices}  ptr r6
          #{poke FullVertexT, neighridges}       ptr r7
          #{poke FullVertexT, nneighridges}      ptr r8

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids             = map (fromIntegral . __id') cvertices
      nneighfacets    = map (fromIntegral . __nneighfacets) cvertices
      nneighsvertices = map (fromIntegral . __nneighvertices) cvertices
      nneighridges     = map (fromIntegral . __nneighridges) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point' cv)))
                 cvertices
  neighfacets <- mapM (\(i, cv) -> (<$!>) (map fromIntegral)
                                          (peekArray i (__neighfacets cv)))
                       (zip nneighfacets cvertices)
  neighvertices <- mapM (\(i, cv) ->
                          (<$!>) (map fromIntegral)
                                 (peekArray i (__neighvertices cv)))
                        (zip nneighsvertices cvertices)
  neighridges <- mapM (\(i, cv) ->
                       (<$!>) (map fromIntegral)
                              (peekArray i (__neighridges cv)))
                     (zip nneighridges cvertices)
  return $ IM.fromList (zip ids
                            (map (\(pt, fneighs, vneighs, eneighs) ->
                                  Vertex { _point = pt
                                         , _neighfaces =
                                              IS.fromAscList fneighs
                                         , _neighvertices =
                                              IS.fromAscList vneighs
                                         , _neighridges =
                                              IS.fromAscList eneighs
                                         })
                                  (zip4 points neighfacets neighvertices
                                        neighridges)))

data CRidge = CRidge {
    __rvertices :: Ptr CVertex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CUInt
  , __ridgeSize :: CUInt
}

instance Storable CRidge where
    sizeOf    __ = #{size RidgeT}
    alignment __ = #{alignment RidgeT}
    peek ptr = do
      rvertices <- #{peek RidgeT, vertices} ptr
      ridgeOf1' <- #{peek RidgeT, ridgeOf1} ptr
      ridgeOf2' <- #{peek RidgeT, ridgeOf2} ptr
      ridgeSize <- #{peek RidgeT, nvertices} ptr
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1 = ridgeOf1'
                    , __ridgeOf2 = ridgeOf2'
                    , __ridgeSize = ridgeSize }
    poke ptr (CRidge r1 r2 r3 r4)
      = do
          #{poke RidgeT, vertices} ptr r1
          #{poke RidgeT, ridgeOf1} ptr r2
          #{poke RidgeT, ridgeOf2} ptr r3
          #{poke RidgeT, nvertices} ptr r4

cRidgeToRidge :: Int -> CRidge -> IO Ridge
cRidgeToRidge dim cridge = do
  let f1 = fromIntegral $ __ridgeOf1 cridge
      f2 = fromIntegral $ __ridgeOf2 cridge
      n  = fromIntegral $ __ridgeSize cridge
  vertices <- peekArray n (__rvertices cridge)
  rvertices <- cVerticesToMap dim vertices
  return Ridge { _rvertices = rvertices
               , _ridgeOf = IS.fromAscList [f1,f2] }

data CFace = CFace {
    __fvertices :: Ptr CVertex
  , __ridges :: Ptr CRidge
  , __nridges' :: CUInt
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
      ridges'     <- #{peek FaceT, ridges} ptr
      nridges'    <- #{peek FaceT, nridges} ptr
      center'    <- #{peek FaceT, center} ptr
      normal'    <- #{peek FaceT, normal} ptr
      offset'    <- #{peek FaceT, offset} ptr
      area'      <- #{peek FaceT, area} ptr
      neighbors' <- #{peek FaceT, neighbors} ptr
      neighsize  <- #{peek FaceT, neighborsize} ptr
      family'    <- #{peek FaceT, family} ptr
      return CFace { __fvertices    = fvertices'
                   , __ridges        = ridges'
                   , __nridges'      = nridges'
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
          #{poke FaceT, ridges}        ptr r2
          #{poke FaceT, nridges}       ptr r3
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
      nridges    = fromIntegral (__nridges' cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  ridges  <- (=<<) (mapM (cRidgeToRidge dim))
                  (peekArray nridges (__ridges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  return Face { _fvertices = vertices
              , _ridges     = ridges
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
  , __allridges :: Ptr CRidge
  , __nridges :: CUInt
  , __alledges :: Ptr (Ptr CUInt)
  , __nalledges :: CUInt
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
      allridges'    <- #{peek ConvexHullT, ridges}     ptr
      nridges'      <- #{peek ConvexHullT, nridges}    ptr
      alledges'    <- #{peek ConvexHullT, edges}     ptr
      nedges'      <- #{peek ConvexHullT, nedges}    ptr
      return CConvexHull { __dim = dim'
                         , __allvertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
                         , __facesizes = facesizes'
                         , __nfaces = nfaces'
                         , __allridges = allridges'
                         , __nridges = nridges'
                         , __alledges = alledges'
                         , __nalledges = nedges'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6 r7 r8 r9 r10)
      = do
          #{poke ConvexHullT, dim}         ptr r1
          #{poke ConvexHullT, vertices}    ptr r2
          #{poke ConvexHullT, nvertices}   ptr r3
          #{poke ConvexHullT, faces}       ptr r4
          #{poke ConvexHullT, facesizes}   ptr r5
          #{poke ConvexHullT, nfaces}      ptr r6
          #{poke ConvexHullT, ridges}       ptr r7
          #{poke ConvexHullT, nridges}      ptr r8
          #{poke ConvexHullT, edges}       ptr r9
          #{poke ConvexHullT, nedges}      ptr r10

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
      nridges   = fromIntegral (__nridges cconvexhull)
      nedges    = fromIntegral (__nalledges cconvexhull)
  facesizes <- (<$!>) (map fromIntegral)
                      (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (=<<) (cVerticesToVertexMap dim)
                    (peekArray nvertices (__allvertices cconvexhull))
  faces <- (=<<) (imapM (\i cface -> cFaceToFace dim (facesizes !! i) cface))
                        (peekArray nfaces (__faces cconvexhull))
  --faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  allridges <- (=<<) (mapM (cRidgeToRidge dim))
                          (peekArray nridges (__allridges cconvexhull))
  alledges' <- (=<<) (mapM (peekArray 2)) (peekArray nedges (__alledges cconvexhull))
  let alledges = map (map fromIntegral) alledges'
  return ConvexHull { _allvertices = vertices
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _allridges = IM.fromAscList (zip [0 .. nridges-1] allridges)
                    , _alledges = S.fromList (map (\x -> (x!!0, x!!1)) alledges)
                    }
