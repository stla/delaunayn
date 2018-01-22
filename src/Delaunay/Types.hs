module Delaunay.Types
  where
import           Data.IntMap.Strict    (IntMap)
import           Data.IntSet           (IntSet)
import           Data.Set              (Set)

type Index = Int
type IndexSet = IntSet
type IndexMap = IntMap

data Facet = Facet {
    _simplex   :: Simplex
  , _neighbors :: IntSet
} deriving Show

data Site = Site {
    _coordinates   :: [Double]
  , _neighSites :: IntSet
  , _neighRidges   :: Set IndexSet
  , _neighFacets   :: IntSet
} deriving Show

data Simplex = Simplex {
    _points :: IndexMap [Double]
  , _circumcenter :: [Double]
  , _normal :: [Double]
  , _volume :: Double
} deriving Show

data Ridge = Ridge {
    _subsimplex :: Simplex
  , _ridgeOf  :: IntSet
} deriving Show

data Delaunay = Delaunay {
    _sites :: IndexMap Site
  , _ridges   :: [Ridge]
  , _facets   :: IntMap Facet
} deriving Show
