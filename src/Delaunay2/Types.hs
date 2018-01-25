module Delaunay2.Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.IntSet        (IntSet)

type Index = Int
type IndexMap = IntMap
type IndexSet = IntSet

data Site = Site {
    _point          :: [Double]
  , _neighsitesIds  :: IndexSet
  , _neighridgesIds :: IntSet
  , _neightilesIds  :: IntSet
} deriving Show

data Simplex = Simplex {
    _points       :: IndexMap [Double]
  , _circumcenter :: [Double]
  , _circumradius :: Double
  , _normal       :: [Double]
  , _offset       :: Double
  , _volume       :: Double
} deriving Show

data TileFacet = TileFacet { -- c'est une facette d'un tile
    _subsimplex :: Simplex
  , _facetOf    :: IntSet
} deriving Show

data Tile = Tile {
    _simplex      :: Simplex
  , _neighborsIds :: IntSet
  , _facetsIds    :: IntSet
  , _family       :: Maybe Int
  , _toporiented  :: Bool
} deriving Show

data Tesselation = Tesselation {
    _sites      :: IndexMap Site
  , _tiles      :: IntMap Tile
  , _tilefacets :: IntMap TileFacet
} deriving Show
