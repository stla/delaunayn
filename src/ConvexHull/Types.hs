module ConvexHull.Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.IntSet        (IntSet)


type Index = Int -- not used here but used in ConvexHull.ConvexHull
type IndexMap = IntMap
type IndexSet = IntSet

data Vertex = Vertex {
    _point         :: [Double]
  , _neighfaces    :: IntSet
  , _neighvertices :: IndexSet
  , _neighedges    :: IndexSet
} deriving Show

data Ridge = Ridge {
    _rvertices :: IndexMap [Double]
  , _ridgeOf   :: IntSet
} deriving Show

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _edges     :: [Ridge]
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
  , _alledges    :: IntMap Ridge
} deriving Show
