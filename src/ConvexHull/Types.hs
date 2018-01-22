module ConvexHull.Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.IntSet        (IntSet)
import           Data.Set           (Set)


type Index = Int -- not used here but used in ConvexHull.ConvexHull
type IndexMap = IntMap
type IndexSet = IntSet

data Vertex = Vertex {
    _point         :: [Double]
  , _neighfaces    :: IntSet
  , _neighvertices :: IndexSet
  , _neighridges   :: IndexSet
} deriving Show

data Ridge = Ridge {
    _rvertices :: IndexMap [Double]
  , _ridgeOf   :: IntSet
} deriving Show

data Face = Face {
    _fvertices :: IndexMap [Double]
  , _ridges    :: [Ridge]
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
  , _allridges   :: IntMap Ridge
  , _alledges    :: Set (Index, Index)
} deriving Show
