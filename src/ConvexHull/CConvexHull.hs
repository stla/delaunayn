{-# LINE 1 "convexhull.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( peekConvexHull
  , c_convexhull )
  where
import           Control.Monad      ((<$!>), (=<<))
import           ConvexHull.Types
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.List
import           Data.List.Index    (imapM)
import           Foreign
import           Foreign.C.Types
import Data.Maybe

data CVertex = CVertex {
    __id    :: CUInt
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
    __id'            :: CUInt
  , __point'         :: Ptr CDouble
  , __neighfacets    :: Ptr CUInt
  , __nneighfacets   :: CUInt
  , __neighvertices  :: Ptr CUInt
  , __nneighvertices :: CUInt
  , __neighedges     :: Ptr CUInt
  , __nneighedges    :: CUInt
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
  , __ridgeOf1  :: CUInt
  , __ridgeOf2  :: CUInt
  , __ridgeSize :: CUInt
}

instance Storable CRidge where
    sizeOf    __ = (24)
{-# LINE 132 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 133 "convexhull.hsc" #-}
    peek ptr = do
      rvertices <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 135 "convexhull.hsc" #-}
      ridgeOf1' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 136 "convexhull.hsc" #-}
      ridgeOf2' <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 137 "convexhull.hsc" #-}
      ridgeSize <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 138 "convexhull.hsc" #-}
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1 = ridgeOf1'
                    , __ridgeOf2 = ridgeOf2'
                    , __ridgeSize = ridgeSize }
    poke ptr (CRidge r1 r2 r3 r4)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 145 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 146 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr r3
{-# LINE 147 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r4
{-# LINE 148 "convexhull.hsc" #-}

cRidgeToRidge :: Int -> Maybe Int -> CRidge -> IO Ridge
cRidgeToRidge dim rsize cridge = do
  let f1 = fromIntegral $ __ridgeOf1 cridge
      f2 = fromIntegral $ __ridgeOf2 cridge
      n  = if isNothing rsize then fromIntegral $ __ridgeSize cridge else fromJust rsize
  vertices <- peekArray n (__rvertices cridge)
  --print $ __id (vertices !! 0)
  rvertices <- cVerticesToMap dim vertices
--  let rvertices = IM.empty
  return Ridge { _rvertices = rvertices
               , _ridgeOf = IS.fromAscList [f1,f2] }

data CFace = CFace {
    __fvertices    :: Ptr CVertex
  , __edges        :: Ptr CRidge
  , __nedges'      :: CUInt
  , __center       :: Ptr CDouble
  , __normal       :: Ptr CDouble
  , __offset       :: CDouble
  , __area         :: CDouble
  , __neighbors    :: Ptr CUInt
  , __neighborsize :: CUInt
  , __family       :: CInt
}

instance Storable CFace where
    sizeOf    __ = (72)
{-# LINE 174 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 175 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 177 "convexhull.hsc" #-}
      edges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 178 "convexhull.hsc" #-}
      nedges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 179 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 180 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 181 "convexhull.hsc" #-}
      offset'    <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 182 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 183 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 184 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 185 "convexhull.hsc" #-}
      family'    <- (\hsc_ptr -> peekByteOff hsc_ptr 68) ptr
{-# LINE 186 "convexhull.hsc" #-}
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
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 199 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r2
{-# LINE 200 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 201 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 202 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)       ptr r5
{-# LINE 203 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)       ptr r6
{-# LINE 204 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)         ptr r7
{-# LINE 205 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)    ptr r8
{-# LINE 206 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr r9
{-# LINE 207 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 68)       ptr r10
{-# LINE 208 "convexhull.hsc" #-}

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
  edges  <- (=<<) (mapM (cRidgeToRidge dim Nothing)) -- (Just $ dim-1)))
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
    __dim         :: CUInt
  , __allvertices :: Ptr CVertex'
  , __nvertices   :: CUInt
  , __faces       :: Ptr CFace
  , __facesizes   :: Ptr CUInt
  , __nfaces      :: CUInt
  , __alledges    :: Ptr CRidge
  , __nedges      :: CUInt
}

instance Storable CConvexHull where
    sizeOf    __ = (64)
{-# LINE 246 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 247 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 249 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 250 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 251 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 252 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 253 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 254 "convexhull.hsc" #-}
      alledges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48)     ptr
{-# LINE 255 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 56)    ptr
{-# LINE 256 "convexhull.hsc" #-}
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
{-# LINE 268 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 269 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 270 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 271 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 272 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 273 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 274 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)      ptr r8
{-# LINE 275 "convexhull.hsc" #-}

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
  alledges <- (=<<) (mapM (cRidgeToRidge dim Nothing))
                          (peekArray nedges (__alledges cconvexhull))
  return ConvexHull { _allvertices = vertices
                    , _faces = IM.fromAscList (zip [0 .. nfaces-1] faces)
                    , _alledges = IM.fromAscList (zip [0 .. nedges-1] alledges)
                    }
