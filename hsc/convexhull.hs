{-# LINE 1 "convexhull.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module ConvexHull.CConvexHull
  ( peekConvexHull
  , c_convexhull )
  where
import           Control.Monad       ((<$!>), (=<<))
import           ConvexHull.Types
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
import           Data.List
import           Data.List.Index     (imapM)
import qualified Data.HashMap.Strict as H
import qualified Data.Set            as S
import           Data.Tuple.Extra    (both)
import           Foreign
import           Foreign.C.Types



data CVertex = CVertex {
    __id :: CUInt
  , __point :: Ptr CDouble
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

--data Vertex = Vertex {
--    _id :: Int
--  , _point :: [Double]
--} deriving Show

cVerticesToMap :: Int -> [CVertex] -> IO (IntMap [Double])
cVerticesToMap dim cvertices = do
  let ids = map (fromIntegral . __id) cvertices
  points <- mapM (\cv -> (<$!>) (map realToFrac) (peekArray dim (__point cv)))
                 cvertices
  return $ fromAscList (zip ids points)

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
    sizeOf    __ = (64)
{-# LINE 64 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 65 "convexhull.hsc" #-}
    peek ptr = do
      id'              <- (\hsc_ptr -> peekByteOff hsc_ptr 0)              ptr
{-# LINE 67 "convexhull.hsc" #-}
      point'           <- (\hsc_ptr -> peekByteOff hsc_ptr 8)           ptr
{-# LINE 68 "convexhull.hsc" #-}
      neighfacets'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16)     ptr
{-# LINE 69 "convexhull.hsc" #-}
      nneighfacets'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24)    ptr
{-# LINE 70 "convexhull.hsc" #-}
      neighvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32)   ptr
{-# LINE 71 "convexhull.hsc" #-}
      nneighsvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 72 "convexhull.hsc" #-}
      neighridges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 48)      ptr
{-# LINE 73 "convexhull.hsc" #-}
      nneighridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56)     ptr
{-# LINE 74 "convexhull.hsc" #-}
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
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)               ptr r1
{-# LINE 86 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)            ptr r2
{-# LINE 87 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr r3
{-# LINE 88 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)     ptr r4
{-# LINE 89 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)    ptr r5
{-# LINE 90 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)  ptr r6
{-# LINE 91 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 92 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)      ptr r8
{-# LINE 93 "convexhull.hsc" #-}

cVerticesToVertexMap :: Int -> [CVertex'] -> IO (IntMap Vertex)
cVerticesToVertexMap dim cvertices = do
  let ids             = map (fromIntegral . __id') cvertices
      nneighfacets    = map (fromIntegral . __nneighfacets) cvertices
      nneighsvertices = map (fromIntegral . __nneighvertices) cvertices
      nneighridges    = map (fromIntegral . __nneighridges) cvertices
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
    sizeOf    __ = (24)
{-# LINE 135 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 136 "convexhull.hsc" #-}
    peek ptr = do
      rvertices <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 138 "convexhull.hsc" #-}
      ridgeOf1' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 139 "convexhull.hsc" #-}
      ridgeOf2' <- (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 140 "convexhull.hsc" #-}
      ridgeSize <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 141 "convexhull.hsc" #-}
      return CRidge { __rvertices = rvertices
                    , __ridgeOf1 = ridgeOf1'
                    , __ridgeOf2 = ridgeOf2'
                    , __ridgeSize = ridgeSize }
    poke ptr (CRidge r1 r2 r3 r4)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 148 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 149 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr r3
{-# LINE 150 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r4
{-# LINE 151 "convexhull.hsc" #-}

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
  , __nvertices' :: CUInt
  , __ridges :: Ptr CRidge
  , __nridges' :: CUInt
  , __center :: Ptr CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __area :: CDouble
  , __neighbors :: Ptr CUInt
  , __neighborsize :: CUInt
  , __family :: CInt
  , __edges :: Ptr (Ptr CUInt)
  , __nedges :: CUInt
}

instance Storable CFace where
    sizeOf    __ = (96)
{-# LINE 180 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 181 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 183 "convexhull.hsc" #-}
      nvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 184 "convexhull.hsc" #-}
      ridges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 185 "convexhull.hsc" #-}
      nridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 186 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 187 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 188 "convexhull.hsc" #-}
      offset'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 189 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 190 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 191 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 192 "convexhull.hsc" #-}
      family'    <- (\hsc_ptr -> peekByteOff hsc_ptr 76) ptr
{-# LINE 193 "convexhull.hsc" #-}
      edges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 194 "convexhull.hsc" #-}
      nedges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 195 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __nvertices'    = nvertices'
                   , __ridges        = ridges'
                   , __nridges'      = nridges'
                   , __center       = center'
                   , __normal       = normal'
                   , __offset       = offset'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize
                   , __family       = family'
                   , __edges        = edges'
                   , __nedges       = nedges'
                 }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 212 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 213 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 214 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)      ptr r4
{-# LINE 215 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)       ptr r5
{-# LINE 216 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)       ptr r6
{-# LINE 217 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 218 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)         ptr r8
{-# LINE 219 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)    ptr r9
{-# LINE 220 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr r10
{-# LINE 221 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 76)       ptr r11
{-# LINE 222 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80)       ptr r12
{-# LINE 223 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88)      ptr r13
{-# LINE 224 "convexhull.hsc" #-}

cFaceToFace :: Int -> CFace -> IO Face
cFaceToFace dim cface = do
  let area      = realToFrac (__area cface)
      neighsize = fromIntegral (__neighborsize cface)
      offset    = realToFrac (__offset cface)
      family    = fromIntegral (__family cface)
      nridges   = fromIntegral (__nridges' cface)
      nvertices = fromIntegral (__nvertices' cface)
      nedges    = fromIntegral (__nedges cface)
  center    <- (<$!>) (map realToFrac) (peekArray dim (__center cface))
  normal    <- (<$!>) (map realToFrac) (peekArray dim (__normal cface))
  vertices  <- (=<<) (cVerticesToMap dim)
                     (peekArray nvertices (__fvertices cface))
  ridges  <- (=<<) (mapM (cRidgeToRidge dim))
                  (peekArray nridges (__ridges cface))
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray neighsize (__neighbors cface))
  edges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__edges cface)))
  let edges = H.fromList
              (zip (map (\(i,j) -> Pair i j) edges')
                   (map (both ((IM.!) vertices)) edges'))
  return Face { _fvertices = vertices
              , _ridges    = ridges
              , _centroid  = center
              , _normal    = normal
              , _offset    = offset
              , _area      = area
              , _neighbors = IS.fromAscList neighbors
              , _family    = if family == -1 then Nothing else Just family
              , _edges     = edges }

data CConvexHull = CConvexHull {
    __dim    :: CUInt
  , __allvertices :: Ptr CVertex'
  , __nvertices :: CUInt
  , __faces :: Ptr CFace
--  , __facesizes :: Ptr CUInt
  , __nfaces :: CUInt
  , __allridges :: Ptr CRidge
  , __nridges :: CUInt
  , __alledges :: Ptr (Ptr CUInt)
  , __nalledges :: CUInt
}

instance Storable CConvexHull where
    sizeOf    __ = (72)
{-# LINE 273 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 274 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 276 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 277 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 278 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 279 "convexhull.hsc" #-}
--      facesizes'   <- #{peek ConvexHullT, facesizes} ptr
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 32)    ptr
{-# LINE 281 "convexhull.hsc" #-}
      allridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 40)     ptr
{-# LINE 282 "convexhull.hsc" #-}
      nridges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 48)    ptr
{-# LINE 283 "convexhull.hsc" #-}
      alledges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 56)     ptr
{-# LINE 284 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 64)    ptr
{-# LINE 285 "convexhull.hsc" #-}
      return CConvexHull { __dim = dim'
                         , __allvertices = vertices'
                         , __nvertices = nvertices'
                         , __faces = faces'
--                       , __facesizes = facesizes'
                         , __nfaces = nfaces'
                         , __allridges = allridges'
                         , __nridges = nridges'
                         , __alledges = alledges'
                         , __nalledges = nedges'
                     }
    poke ptr (CConvexHull r1 r2 r3 r4 r5 r6 r7 r8 r9)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)         ptr r1
{-# LINE 299 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 300 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 301 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 302 "convexhull.hsc" #-}
--          #{poke ConvexHullT, facesizes}   ptr r5
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)      ptr r5
{-# LINE 304 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 305 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)     ptr r7
{-# LINE 306 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)       ptr r8
{-# LINE 307 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64)      ptr r9
{-# LINE 308 "convexhull.hsc" #-}

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
--  facesizes <- (<$!>) (map fromIntegral)
--                      (peekArray nfaces (__facesizes cconvexhull))
  vertices <- (=<<) (cVerticesToVertexMap dim)
                    (peekArray nvertices (__allvertices cconvexhull))
--  faces <- (=<<) (imapM (\i cface -> cFaceToFace dim (facesizes !! i) cface))
--                        (peekArray nfaces (__faces cconvexhull))
  faces <- (=<<) (mapM (cFaceToFace dim))
                       (peekArray nfaces (__faces cconvexhull))
  --faces <- mapM (\i -> (<$!>) (map fromIntegral) (peekArray (facesizes !! i) (faces' !! i))) [0 .. length faces'-1]
  allridges <- (=<<) (mapM (cRidgeToRidge dim))
                          (peekArray nridges (__allridges cconvexhull))
  alledges' <- (<$!>) (map (\x -> (fromIntegral (x!!0), fromIntegral (x!!1))))
                      ((=<<) (mapM (peekArray 2))
                             (peekArray nedges (__alledges cconvexhull)))
  let alledges = let points = IM.map _point vertices in
                  H.fromList
                  (zip (map (\(i,j) -> Pair i j) alledges')
                       (map (both ((IM.!) points)) alledges'))
  return ConvexHull { _allvertices = vertices
                    , _faces = fromAscList (zip [0 .. nfaces-1] faces)
                    , _allridges = fromAscList (zip [0 .. nridges-1] allridges)
                    , _alledges = alledges
                    }
