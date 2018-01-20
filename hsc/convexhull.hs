{-# LINE 1 "convexhull.hsc" #-}
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



data CVertex = CVertex {
    __id :: CUInt
  , __point :: Ptr CDouble
}

instance Storable CVertex where
    sizeOf    __ = (16)
{-# LINE 25 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 26 "convexhull.hsc" #-}
    peek ptr = do
      id'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 28 "convexhull.hsc" #-}
      point'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8)    ptr
{-# LINE 29 "convexhull.hsc" #-}
      return CVertex { __id = id'
                     , __point = point' }
    poke ptr (CVertex r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 34 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)      ptr r2
{-# LINE 35 "convexhull.hsc" #-}

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
    sizeOf    __ = (64)
{-# LINE 61 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 62 "convexhull.hsc" #-}
    peek ptr = do
      id'              <- (\hsc_ptr -> peekByteOff hsc_ptr 0)              ptr
{-# LINE 64 "convexhull.hsc" #-}
      point'           <- (\hsc_ptr -> peekByteOff hsc_ptr 8)           ptr
{-# LINE 65 "convexhull.hsc" #-}
      neighfacets'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16)     ptr
{-# LINE 66 "convexhull.hsc" #-}
      nneighfacets'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24)    ptr
{-# LINE 67 "convexhull.hsc" #-}
      neighvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32)   ptr
{-# LINE 68 "convexhull.hsc" #-}
      nneighsvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 69 "convexhull.hsc" #-}
      neighedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 48)      ptr
{-# LINE 70 "convexhull.hsc" #-}
      nneighedges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 56)     ptr
{-# LINE 71 "convexhull.hsc" #-}
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
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)               ptr r1
{-# LINE 83 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)            ptr r2
{-# LINE 84 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr r3
{-# LINE 85 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)     ptr r4
{-# LINE 86 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)    ptr r5
{-# LINE 87 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)  ptr r6
{-# LINE 88 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 89 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)      ptr r8
{-# LINE 90 "convexhull.hsc" #-}

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
                                         , _neighfacets =
                                              IS.fromAscList fneighs
                                         , _neighvertices =
                                              IS.fromAscList vneighs
                                         , _neighedges =
                                              IS.fromAscList eneighs
                                         })
                                  (zip4 points neighfacets neighvertices
                                        neighedges)))

data CEdge = CEdge {
    __v1 :: CVertex
  , __v2 :: CVertex
}

instance Storable CEdge where
    sizeOf    __ = (32)
{-# LINE 130 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 131 "convexhull.hsc" #-}
    peek ptr = do
      v1'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)    ptr
{-# LINE 133 "convexhull.hsc" #-}
      v2'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16)    ptr
{-# LINE 134 "convexhull.hsc" #-}
      return CEdge { __v1 = v1'
                   , __v2 = v2' }
    poke ptr (CEdge r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)      ptr r1
{-# LINE 139 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr r2
{-# LINE 140 "convexhull.hsc" #-}

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
  , __family :: CInt
}

instance Storable CFace where
    sizeOf    __ = (64)
{-# LINE 161 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 162 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 164 "convexhull.hsc" #-}
      edges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 165 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 166 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 167 "convexhull.hsc" #-}
      offset'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 168 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 169 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 170 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 171 "convexhull.hsc" #-}
      family'    <- (\hsc_ptr -> peekByteOff hsc_ptr 60) ptr
{-# LINE 172 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize
                   , __family       = family' }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8 r9)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 184 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r2
{-# LINE 185 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 186 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 187 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)       ptr r5
{-# LINE 188 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)         ptr r6
{-# LINE 189 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)    ptr r7
{-# LINE 190 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r8
{-# LINE 191 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 60)       ptr r9
{-# LINE 192 "convexhull.hsc" #-}

cFaceToFace :: Int -> Int -> CFace -> IO Face
cFaceToFace dim nvertices cface = do
  let area      = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
      offset    = realToFrac (__offset cface)
      family    = fromIntegral (__family cface)
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
              , _centroid  = center
              , _normal    = normal
              , _offset    = offset
              , _area      = area
              , _neighbors = IS.fromAscList neighbors
              , _family    = if family==-1 then Nothing else Just family }

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
    sizeOf    __ = (64)
{-# LINE 229 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 230 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 232 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 233 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 234 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 235 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 236 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 237 "convexhull.hsc" #-}
      alledges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48)     ptr
{-# LINE 238 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 56)    ptr
{-# LINE 239 "convexhull.hsc" #-}
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
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 251 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 252 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 253 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 254 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 255 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 256 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 257 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)      ptr r8
{-# LINE 258 "convexhull.hsc" #-}

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
  alledges <- (=<<) (mapM (cEdgeToEdge dim))
                          (peekArray nedges (__alledges cconvexhull))
  return ConvexHull { _allvertices = vertices
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _alledges = IM.fromAscList (zip [0 .. nedges-1] alledges)
                    }
