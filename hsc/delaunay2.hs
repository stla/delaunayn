{-# LINE 1 "delaunay2.hsc" #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay2.CDelaunay
  ( cTesselationToTesselation
  , c_tesselation )
  where
import           Control.Monad       ((<$!>))
import           Delaunay2.Types
import           Data.IntMap.Strict  (IntMap, fromAscList)
-- import qualified Data.IntMap.Strict  as IM
import qualified Data.IntSet         as IS
-- import           Data.List
import qualified Data.HashMap.Strict as H
-- import           Data.Tuple.Extra    (both)
import           Foreign
import           Foreign.C.Types
-- import           Foreign.C.String



data CSite = CSite {
    __id :: CUInt
  , __neighsites :: Ptr CUInt
  , __nneighsites :: CUInt
  , __neighridgesids :: Ptr CUInt
  , __nneighridges :: CUInt
  , __neightiles :: Ptr CUInt
  , __nneightiles :: CUInt
}

instance Storable CSite where
    sizeOf    __ = (56)
{-# LINE 32 "delaunay2.hsc" #-}
    alignment __ = 8
{-# LINE 33 "delaunay2.hsc" #-}
    peek ptr = do
      id'              <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 35 "delaunay2.hsc" #-}
      neighsites'      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 36 "delaunay2.hsc" #-}
      nneighsites'     <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 37 "delaunay2.hsc" #-}
      neighridgesids'  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 38 "delaunay2.hsc" #-}
      nneighridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 39 "delaunay2.hsc" #-}
      neightiles'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 40 "delaunay2.hsc" #-}
      nneightiles'     <- (\hsc_ptr -> peekByteOff hsc_ptr 48) ptr
{-# LINE 41 "delaunay2.hsc" #-}
      return CSite { __id = id'
                   , __neighsites = neighsites'
                   , __nneighsites = nneighsites'
                   , __neighridgesids = neighridgesids'
                   , __nneighridges = nneighridges'
                   , __neightiles = neightiles'
                   , __nneightiles = nneightiles'
                  }
    poke ptr (CSite r1 r2 r3 r4 r5 r6 r7)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 52 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 53 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 54 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 55 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 56 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 57 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 48) ptr r7
{-# LINE 58 "delaunay2.hsc" #-}

cSiteToSite :: [[Double]] -> CSite -> IO (Int, Site)
cSiteToSite sites csite = do
  let id'          = fromIntegral $ __id csite
      nneighsites  = fromIntegral $ __nneighsites csite
      nneighridges = fromIntegral $ __nneighridges csite
      nneightiles  = fromIntegral $ __nneightiles csite
      point        = sites !! id'
  neighsites <- (<$!>) (map fromIntegral)
                       (peekArray nneighsites (__neighsites csite))
  neighridges <- (<$!>) (map fromIntegral)
                        (peekArray nneighridges (__neighridgesids csite))
  neightiles <- (<$!>) (map fromIntegral)
                       (peekArray nneightiles (__neightiles csite))
  return (id', Site {
                      _point          = point
                    , _neighsitesIds  = IS.fromAscList neighsites
                    , _neighridgesIds = IS.fromAscList neighridges
                    , _neightilesIds  = IS.fromAscList neightiles
                  })

data CSimplex = CSimplex {
    __sitesids :: Ptr CUInt
  , __center :: Ptr CDouble
  , __radius :: CDouble
  , __normal :: Ptr CDouble
  , __offset :: CDouble
  , __volume :: CDouble
}

instance Storable CSimplex where
    sizeOf    __ = (48)
{-# LINE 90 "delaunay2.hsc" #-}
    alignment __ = 8
{-# LINE 91 "delaunay2.hsc" #-}
    peek ptr = do
      sitesids'    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 93 "delaunay2.hsc" #-}
      center'      <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 94 "delaunay2.hsc" #-}
      radius'      <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 95 "delaunay2.hsc" #-}
      normal'      <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 96 "delaunay2.hsc" #-}
      offset'      <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 97 "delaunay2.hsc" #-}
      volume'      <- (\hsc_ptr -> peekByteOff hsc_ptr 40) ptr
{-# LINE 98 "delaunay2.hsc" #-}
      return CSimplex { __sitesids    = sitesids'
                      , __center      = center'
                      , __radius      = radius'
                      , __normal      = normal'
                      , __offset      = offset'
                      , __volume      = volume'
                    }
    poke ptr (CSimplex r1 r2 r3 r4 r5 r6)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 108 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 109 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 110 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 111 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 112 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 40) ptr r6
{-# LINE 113 "delaunay2.hsc" #-}

cSimplexToSimplex :: [[Double]] -> Int -> CSimplex -> IO Simplex
cSimplexToSimplex sites simplexdim csimplex = do
  let radius      = cdbl2dbl $ __radius csimplex
      offset      = cdbl2dbl $ __offset csimplex
      volume      = cdbl2dbl $ __volume csimplex
      dim         = length (head sites)
  sitesids <- (<$!>) (map fromIntegral)
                     (peekArray simplexdim (__sitesids csimplex))
  let points = IM.fromAscList
               (zip sitesids (map ((!!) sites) sitesids))
  center <- (<$!>) (map cdbl2dbl) (peekArray dim (__center csimplex))
  normal <- (<$!>) (map cdbl2dbl) (peekArray simplexdim (__normal csimplex))
  return Simplex { _points       = points
                 , _circumcenter = center
                 , _circumradius = radius
                 , _normal       = normal
                 , _offset       = offset
                 , _volume       = volume
                 }
  where
    cdbl2dbl :: CDouble -> Double
    cdbl2dbl x = if isNaN x then 0/0 else realToFrac x

data CSubTile = CSubTile {
    __id' :: CUInt
  , __subsimplex :: CSimplex
  , __ridgeOf1 :: CUInt
  , __ridgeOf2 :: CInt
}

instance Storable CSubTile where
    sizeOf    __ = (72)
{-# LINE 146 "delaunay2.hsc" #-}
    alignment __ = 8
{-# LINE 147 "delaunay2.hsc" #-}
    peek ptr = do
      id'       <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 149 "delaunay2.hsc" #-}
      simplex'  <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 150 "delaunay2.hsc" #-}
      ridgeOf1' <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 151 "delaunay2.hsc" #-}
      ridgeOf2' <- (\hsc_ptr -> peekByteOff hsc_ptr 60) ptr
{-# LINE 152 "delaunay2.hsc" #-}
      return CSubTile { __id'        = id'
                      , __subsimplex = simplex'
                      , __ridgeOf1   = ridgeOf1'
                      , __ridgeOf2   = ridgeOf2'
                    }
    poke ptr (CSubTile r1 r2 r3 r4)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 160 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 161 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r3
{-# LINE 162 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 60) ptr r4
{-# LINE 163 "delaunay2.hsc" #-}

cSubTiletoSubTile :: [[Double]] -> CSubTile -> IO (Int, SubTile)
cSubTiletoSubTile points csubtile = do
  let dim        = length (head points)
      ridgeOf1   = fromIntegral $ __ridgeOf1 csubtile
      ridgeOf2   = fromIntegral $ __ridgeOf2 csubtile
      ridgeOf    = if ridgeOf2 == -1 then [ridgeOf1] else [ridgeOf1, ridgeOf2]
      id'        = fromIntegral $ __id' csubtile
      subsimplex = __subsimplex csubtile
  simplex <- cSimplexToSimplex points dim subsimplex
  return (id', SubTile {
                        _subsimplex = simplex
                      , _ridgeOf    = IS.fromAscList ridgeOf
               })

data CTile = CTile {
    __id'' :: CUInt
  , __simplex :: CSimplex
  , __neighbors :: Ptr CUInt
  , __nneighbors :: CUInt
  , __ridgesids :: Ptr CUInt
  , __nridges :: CUInt
  , __family :: CInt
  , __orientation :: CInt
}

instance Storable CTile where
    sizeOf    __ = (96)
{-# LINE 191 "delaunay2.hsc" #-}
    alignment __ = 8
{-# LINE 192 "delaunay2.hsc" #-}
    peek ptr = do
      id'         <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 194 "delaunay2.hsc" #-}
      simplex'    <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 195 "delaunay2.hsc" #-}
      neighbors'  <- (\hsc_ptr -> peekByteOff hsc_ptr 56) ptr
{-# LINE 196 "delaunay2.hsc" #-}
      nneighbors' <- (\hsc_ptr -> peekByteOff hsc_ptr 64) ptr
{-# LINE 197 "delaunay2.hsc" #-}
      ridgesids'  <- (\hsc_ptr -> peekByteOff hsc_ptr 72) ptr
{-# LINE 198 "delaunay2.hsc" #-}
      nridges'    <- (\hsc_ptr -> peekByteOff hsc_ptr 80) ptr
{-# LINE 199 "delaunay2.hsc" #-}
      family'     <- (\hsc_ptr -> peekByteOff hsc_ptr 84) ptr
{-# LINE 200 "delaunay2.hsc" #-}
      orient      <- (\hsc_ptr -> peekByteOff hsc_ptr 88) ptr
{-# LINE 201 "delaunay2.hsc" #-}
      return CTile { __id''        = id'
                   , __simplex     = simplex'
                   , __neighbors   = neighbors'
                   , __nneighbors  = nneighbors'
                   , __ridgesids   = ridgesids'
                   , __nridges     = nridges'
                   , __family      = family'
                   , __orientation = orient
                  }
    poke ptr (CTile r1 r2 r3 r4 r5 r6 r7 r8)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 213 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 214 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 56) ptr r3
{-# LINE 215 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 64) ptr r4
{-# LINE 216 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 72) ptr r5
{-# LINE 217 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 80) ptr r6
{-# LINE 218 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 84) ptr r7
{-# LINE 219 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 88) ptr r8
{-# LINE 220 "delaunay2.hsc" #-}

cTileToTile :: [[Double]] -> CTile -> IO (Int, Tile)
cTileToTile points ctile = do
  let id'        = fromIntegral $ __id'' ctile
      csimplex   = __simplex ctile
      nneighbors = fromIntegral $ __nneighbors ctile
      nridges    = fromIntegral $ __nridges ctile
      family     = __family ctile
      orient     = __orientation ctile
      dim        = length (head points)
  simplex <- cSimplexToSimplex points (dim+1) csimplex
  neighbors <- (<$!>) (map fromIntegral)
                      (peekArray nneighbors (__neighbors ctile))
  ridgesids <- (<$!>) (map fromIntegral)
                      (peekArray nridges (__ridgesids ctile))
  return (id', Tile {
                 _simplex      = simplex
               , _neighborsIds = IS.fromAscList neighbors
               , _subtilesIds  = IS.fromAscList ridgesids
               , _family       = if family == -1
                                  then Nothing
                                  else Just (fromIntegral family)
               , _toporiented  = orient == 1
              })

data CTesselation = CTesselation {
    __sites :: Ptr CSite
  , __tiles :: Ptr CTile
  , __ntiles :: CUInt
  , __subtiles :: Ptr CSubTile
  , __nsubtiles :: CUInt
}

instance Storable CTesselation where
    sizeOf    __ = (40)
{-# LINE 255 "delaunay2.hsc" #-}
    alignment __ = 8
{-# LINE 256 "delaunay2.hsc" #-}
    peek ptr = do
      sites'     <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 258 "delaunay2.hsc" #-}
      tiles'     <- (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 259 "delaunay2.hsc" #-}
      ntiles'    <- (\hsc_ptr -> peekByteOff hsc_ptr 16) ptr
{-# LINE 260 "delaunay2.hsc" #-}
      subtiles'  <- (\hsc_ptr -> peekByteOff hsc_ptr 24) ptr
{-# LINE 261 "delaunay2.hsc" #-}
      nsubtiles' <- (\hsc_ptr -> peekByteOff hsc_ptr 32) ptr
{-# LINE 262 "delaunay2.hsc" #-}
      return CTesselation {
                     __sites     = sites'
                   , __tiles     = tiles'
                   , __ntiles    = ntiles'
                   , __subtiles  = subtiles'
                   , __nsubtiles = nsubtiles'
                  }
    poke ptr (CTesselation r1 r2 r3 r4 r5)
      = do
          (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr r1
{-# LINE 272 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr r2
{-# LINE 273 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr r3
{-# LINE 274 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 24) ptr r4
{-# LINE 275 "delaunay2.hsc" #-}
          (\hsc_ptr -> pokeByteOff hsc_ptr 32) ptr r5
{-# LINE 276 "delaunay2.hsc" #-}

foreign import ccall unsafe "tesselation" c_tesselation
  :: Ptr CDouble -- sites
  -> CUInt       -- dim
  -> CUInt       -- nsites
  -> CUInt       -- 0/1, include degenerate
  -> Ptr CUInt   -- exitcode
  -> IO (Ptr CTesselation)

cTesselationToTesselation :: [[Double]] -> CTesselation -> IO Tesselation
cTesselationToTesselation sites ctess = do
  let ntiles    = fromIntegral $ __ntiles ctess
      nsubtiles = fromIntegral $ __nsubtiles ctess
      nsites    = length sites
  sites'' <- peekArray nsites (__sites ctess)
  tiles'' <- peekArray ntiles (__tiles ctess)
  subtiles'' <- peekArray nsubtiles (__subtiles ctess)
  sites' <- mapM (cSiteToSite sites) sites''
  tiles' <- mapM (cTileToTile sites) tiles''
  subtiles' <- mapM (cSubTiletoSubTile sites) subtiles''
  return Tesselation
         {
            _sites    = IM.fromAscList sites'
          , _tiles    = IM.fromAscList tiles'
          , _subtiles = IM.fromAscList subtiles'
         }
