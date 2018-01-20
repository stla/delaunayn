module Voronoi
  where
import qualified Data.IntMap.Strict as IM
-- import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IS
-- import           Data.List
-- import           Data.Map.Strict    (Map)
-- import qualified Data.Map.Strict    as M
import           Data.Maybe
--import qualified Data.Set           as S
import           Data.Tuple.Extra   ((&&&))
import           Delaunay


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
approx n x = fromInteger (round $ x * (10^n)) / (10.0^^n)

---

ridgeAsPair :: Ridge -> (Simplex, [Int])
ridgeAsPair = _subsimplex &&& (IS.toList . _ridgeOf)

edgesFromRidge :: Delaunay -> Ridge -> Maybe Edge
edgesFromRidge tess ridge
  | length facetindices == 1
    = Just $ IEdge (c1, _normal simplex)
  | c1 == c2 = Nothing
  | otherwise = Just $ Edge (c1, c2)
  where
    (simplex, facetindices) = ridgeAsPair ridge
    facets = _facets tess
    c1 = _circumcenter $ _simplex (facets IM.! head facetindices)
    c2 = _circumcenter $ _simplex (facets IM.! last facetindices)


voronoiCell :: ([Ridge] -> [Ridge]) -> (Edge -> a) -> Delaunay -> Index -> [a]
voronoiCell ridgesQuotienter edgeTransformer tess i =
  let ridges = ridgesQuotienter $ vertexNeighborRidges tess i in
  map (edgeTransformer . fromJust) $
      filter isJust $ map (edgesFromRidge tess) ridges

voronoi :: (Delaunay -> Index -> a) -> Delaunay -> [([Double], a)]
voronoi cellGetter tess =
  let sites = IM.elems $ IM.map _coordinates (_vertices tess) in
    zip sites (map (cellGetter tess) [0 .. length sites -1])

voronoi' :: Delaunay -> [([Double], [Edge])]
voronoi' = voronoi (voronoiCell id id)

-- getVertexRidges' :: Delaunay -> Index -> [(CentredPolytope, [Int], Double)]
-- getVertexRidges' tess i =
--   foldr (unionBy equalRidges) [] $
--     M.elems $ M.restrictKeys (vertexNeighborsRidges tess)
--                              (S.fromList $ _vrneighbors tess !! i)
--

-- getVertexRidges :: Delaunay -> Index -> [Ridge]
-- getVertexRidges tess i =
--     M.elems $ M.restrictKeys (ridgesMap tess)
--                              (_neighRidges $ _vertices tess IM.! i)

-- uniqueWith :: (a -> a -> Bool) -> [a] -> [a] -- nubBy dans Data.List !
-- uniqueWith f = foldr (unionBy f . (: [])) []
--
-- getVertexRidges' :: Delaunay -> Index -> Map IntSet Ridge
-- getVertexRidges' tess i =
--     M.restrictKeys (ridgesMap tess)
--                    (_neighRidges $ _vertices tess IM.! i)
--
-- uniqueWith' :: (a -> a -> Bool) -> Map IntSet a -> [a]
-- uniqueWith' f list = M.foldr (unionBy f) [] (M.map (: []) list)
