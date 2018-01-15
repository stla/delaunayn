module VoronoiShared
  where
import           Data.IntSet        (IntSet)
import           Data.List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import           Delaunay

type Index = Int
type Site = [Double]
type Point = [Double]
type Vector = [Double]
data Edge = Edge (Point, Point) | IEdge (Point, Vector)


factor2 :: (Double,Double,Double,Double) -> (Double,Double) -> (Double,Double) -> Double
factor2 box@(xmin, xmax, ymin, ymax) p@(p1,p2) (v1,v2)
  | v1==0 = if v2>0 then (ymax-p2)/v2 else (ymin-p2)/v2
  | v2==0 = if v1>0 then (xmax-p1)/v1 else (xmin-p1)/v1
  | otherwise = min (factor2 box p (v1,0)) (factor2 box p (0,v2))
  --  | v1>0 && v2>0 = min ((r-p1)/v1) ((t-p2)/v2)
  --  | v1>0 && v2<0 = min ((r-p1)/v1) ((b-p2)/v2)
  --  | v1<0 && v2>0 = min ((l-p1)/v1) ((t-p2)/v2)
  --  | v1<0 && v2<0 = min ((l-p1)/v1) ((b-p2)/v2)

approx :: RealFrac a => Int -> a -> a
approx n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

-----
ridgeAsTriplet :: Ridge -> (CentredPolytope, [Int], Double)
ridgeAsTriplet ridge = (_rsimplex ridge, _ridgeOf ridge, _distance ridge)

edgesFromRidge :: Delaunay -> Ridge -> Maybe Edge
edgesFromRidge tess ridge =
  if length facetindices == 1
    then Just $ IEdge (c1, _normal polytope)
    else if c1==c2 then Nothing else Just $ Edge (c1, c2)
  where
    (polytope, facetindices, _) = ridgeAsTriplet ridge
    facets = _facets tess
    c1 = facetCenter (facets !! (head facetindices))
    c2 = facetCenter (facets !! (last facetindices))


-- -- rien à voir avec neighbors
-- vertexNeighborsRidges :: Delaunay -> Map IntSet [(CentredPolytope, [Int], Double)]
-- vertexNeighborsRidges tess =
--   M.fromListWith (unionBy ((==) `on` _ridgesVertices)) ridges' -- ce unionby me semble inutile (fromList ferait pareil)
--   where
--     ridges' :: [(IntSet, [(CentredPolytope, [Int], Double)])]
--     ridges' =  map (\(x,y) -> (x,[y])) $ concat $ map
--                         (\facet -> let ridges = _ridges facet in
--                                      let vertices = map _ridgesVertices ridges in
--                                           zip vertices ridges)
--                         (_facets tess)
-- --
-- getVertexRidges' :: Delaunay -> Index -> [(CentredPolytope, [Int], Double)]
-- getVertexRidges' tess i =
--   foldr (unionBy equalRidges) [] $
--     M.elems $ M.restrictKeys (vertexNeighborsRidges tess)
--                              (S.fromList $ _vrneighbors tess !! i)
--
allRidges :: Delaunay -> Map IntSet Ridge
allRidges tess = M.fromList ridges'
  where
    ridges' :: [(IntSet, Ridge)]
    ridges' =  concat $ map
                        (\facet -> let ridges = _ridges facet in
                                   let vertices = map _ridgesVertices ridges in
                                    zip vertices ridges)
                        (_facets tess)
    _ridgesVertices = _vertices . _rsimplex

getVertexRidges :: Delaunay -> Index -> [Ridge]
getVertexRidges tess i =
    M.elems $ M.restrictKeys (allRidges tess)
                             (S.fromList $ _vrneighbors tess !! i)

uniqueWith :: (a -> a -> Bool) -> [a] -> [a]
uniqueWith f list = foldr (unionBy f) [] (map (\x -> [x]) list)
