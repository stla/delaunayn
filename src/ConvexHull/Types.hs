module ConvexHull.Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.IntSet        (IntSet)


type Index = Int -- not used here but used in ConvexHull.ConvexHull
type IndexMap = IntMap
type IndexSet = IntSet

data Vertex = Vertex {
    _point         :: [Double]
  , _neighfacets   :: IntSet
  , _neighvertices :: IndexSet
  , _neighedges    :: IndexSet
} deriving Show

type Edge = IntMap [Double]

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _edges     :: [Edge]
  , _centroid  :: [Double]
  , _normal    :: [Double]
  , _offset    :: Double
  , _area      :: Double
  , _neighbors :: IntSet
  , _family    :: Maybe Int
} deriving Show

data ConvexHull = ConvexHull {
    _allvertices :: IndexMap Vertex
  , _faces       :: IntMap Face
  , _alledges    :: IntMap Edge
} deriving Show
