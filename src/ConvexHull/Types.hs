module ConvexHull.Types
  where
import           Data.IntMap.Strict (IntMap)
import           Data.Map.Strict    (Map)
import           Data.IntSet        (IntSet)
import           Data.Set           (Set)
import Data.Hashable
import Data.HashMap.Strict (HashMap)

type Index = Int -- not used here but used in ConvexHull.ConvexHull
type IndexMap = IntMap
type IndexSet = IntSet

data IndexPair = Pair Index Index
  deriving (Show, Read)
instance Eq IndexPair where
    Pair i j == Pair i' j' = (i == i' && j == j') || (i == j' && j == i')

instance Hashable IndexPair where
  hashWithSalt _ (Pair i j) = 2*(i+j)*(i+j+1) + min i j

type EdgeMap = HashMap IndexPair ([Double],[Double])

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
  , _edges     :: EdgeMap
} deriving Show

data ConvexHull = ConvexHull {
    _allvertices :: IndexMap Vertex
  , _faces       :: IntMap Face
  , _allridges   :: IntMap Ridge
  , _alledges    :: EdgeMap
} deriving Show
