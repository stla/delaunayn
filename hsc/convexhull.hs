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
    sizeOf    __ = (32)
{-# LINE 60 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 61 "convexhull.hsc" #-}
    peek ptr = do
      id'           <- (\hsc_ptr -> peekByteOff hsc_ptr 0)         ptr
{-# LINE 63 "convexhull.hsc" #-}
      point'        <- (\hsc_ptr -> peekByteOff hsc_ptr 8)      ptr
{-# LINE 64 "convexhull.hsc" #-}
      neighfacets'  <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 65 "convexhull.hsc" #-}
      nneighfacets' <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 66 "convexhull.hsc" #-}
      return CVertex' { __id' = id'
                     , __point' = point'
                     , __neighfacets = neighfacets'
                     , __nneighfacets = nneighfacets' }
    poke ptr (CVertex' r1 r2 r3 r4)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)            ptr r1
{-# LINE 73 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)         ptr r2
{-# LINE 74 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 75 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)  ptr r4
{-# LINE 76 "convexhull.hsc" #-}

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
                                  zip points neighfacets))

data CEdge = CEdge {
    __v1 :: CVertex
  , __v2 :: CVertex
}

instance Storable CEdge where
    sizeOf    __ = (32)
{-# LINE 105 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 106 "convexhull.hsc" #-}
    peek ptr = do
      v1'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0)    ptr
{-# LINE 108 "convexhull.hsc" #-}
      v2'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16)    ptr
{-# LINE 109 "convexhull.hsc" #-}
      return CEdge { __v1 = v1'
                   , __v2 = v2' }
    poke ptr (CEdge r1 r2)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)      ptr r1
{-# LINE 114 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)      ptr r2
{-# LINE 115 "convexhull.hsc" #-}

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
    sizeOf    __ = (64)
{-# LINE 137 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 138 "convexhull.hsc" #-}
    peek ptr = do
      fvertices' <- (\hsc_ptr -> peekByteOff hsc_ptr 0)     ptr
{-# LINE 140 "convexhull.hsc" #-}
      edges'     <- (\hsc_ptr -> peekByteOff hsc_ptr 8)        ptr
{-# LINE 141 "convexhull.hsc" #-}
      center'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16)       ptr
{-# LINE 142 "convexhull.hsc" #-}
      normal'    <- (\hsc_ptr -> peekByteOff hsc_ptr 24)       ptr
{-# LINE 143 "convexhull.hsc" #-}
      offset'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32)       ptr
{-# LINE 144 "convexhull.hsc" #-}
      area'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)         ptr
{-# LINE 145 "convexhull.hsc" #-}
      neighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 48)    ptr
{-# LINE 146 "convexhull.hsc" #-}
      neighsize  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 147 "convexhull.hsc" #-}
      return CFace { __fvertices    = fvertices'
                   , __edges        = edges'
                   , __center       = center'
                   , __normal       = normal'
                   , __area         = area'
                   , __neighbors    = neighbors'
                   , __neighborsize = neighsize }
    poke ptr (CFace r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0)     ptr r1
{-# LINE 157 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)        ptr r2
{-# LINE 158 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)       ptr r3
{-# LINE 159 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 160 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)       ptr r5
{-# LINE 161 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)         ptr r6
{-# LINE 162 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)    ptr r7
{-# LINE 163 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r8
{-# LINE 164 "convexhull.hsc" #-}

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _edges :: [Edge]
  , _center :: [Double]
  , _normal :: [Double]
  , _offset :: [Double]
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
    sizeOf    __ = (64)
{-# LINE 209 "convexhull.hsc" #-}
    alignment __ = 8
{-# LINE 210 "convexhull.hsc" #-}
    peek ptr = do
      dim'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0)       ptr
{-# LINE 212 "convexhull.hsc" #-}
      vertices'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8)  ptr
{-# LINE 213 "convexhull.hsc" #-}
      nvertices'   <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 214 "convexhull.hsc" #-}
      faces'       <- (\hsc_ptr -> peekByteOff hsc_ptr 24)     ptr
{-# LINE 215 "convexhull.hsc" #-}
      facesizes'   <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 216 "convexhull.hsc" #-}
      nfaces'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40)    ptr
{-# LINE 217 "convexhull.hsc" #-}
      alledges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 48)     ptr
{-# LINE 218 "convexhull.hsc" #-}
      nedges'      <- (\hsc_ptr -> peekByteOff hsc_ptr 56)    ptr
{-# LINE 219 "convexhull.hsc" #-}
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
{-# LINE 231 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8)    ptr r2
{-# LINE 232 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16)   ptr r3
{-# LINE 233 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24)       ptr r4
{-# LINE 234 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32)   ptr r5
{-# LINE 235 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40)      ptr r6
{-# LINE 236 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48)       ptr r7
{-# LINE 237 "convexhull.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56)      ptr r8
{-# LINE 238 "convexhull.hsc" #-}

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
