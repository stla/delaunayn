module Voronoi3D
  (Edge3
 , Cell3
 , Voronoi3
 , prettyShowVoronoi3
 , voronoi3
 , clipVoronoi3
 , testv3
 , voronoi3ForRgl)
  where
import           Data.IntSet      (IntSet)
import qualified Data.IntSet      as S
import           Data.List
import           Data.Map.Strict  (Map, difference, elems, empty, fromList,
                                   mapMaybeWithKey)
import qualified Data.Map.Strict  as M
import           Data.Maybe
import           Data.Tuple.Extra ((&&&), fst3, snd3, thd3)
import           Delaunay
import           Text.Show.Pretty (ppShow)
import           VoronoiShared
import Data.Function (on)

type Point3 = (Double, Double, Double)
type Vector3 = (Double, Double, Double)
data Edge3 = Edge3 (Point3, Point3) | IEdge3 (Point3, Vector3)
             | TIEdge3 (Point3, Point3)
              deriving Show
-- type Edge3' = (Edge3, Index)
type Cell3 = (Site, [Edge3])
type Voronoi3 = [Cell3]
type Ridge3 = IntSet -- (Index, Index, Index)
type Box3 = (Double, Double, Double, Double, Double, Double)

prettyShowVoronoi3 :: Voronoi3 -> Maybe Int -> IO ()
prettyShowVoronoi3 v m = do
  let string = intercalate "\n---\n" (map (prettyShowCell3 m) v)
  putStrLn string
  where
    roundPairPoint3 :: (Point3, Point3) -> Int -> (Point3, Point3)
    roundPairPoint3 ((x1,x2,x3), (y1,y2,y3)) n =
      (asTriplet $ map (approx n) [x1,x2,x3],
       asTriplet $ map (approx n) [y1,y2,y3])
    prettyShowEdge3 :: Maybe Int -> Edge3 -> String
    prettyShowEdge3 n edge = case edge of
      Edge3 x   -> " Edge " ++ string x
      IEdge3 x  -> " IEdge " ++ string x
      TIEdge3 x -> " TIEdge " ++ string x
      where
        string x = ppShow $ maybe x (roundPairPoint3 x) n
    prettyShowEdges3 :: Maybe Int -> [Edge3] -> String
    prettyShowEdges3 n edges = intercalate "\n" (map (prettyShowEdge3 n) edges)
    prettyShowCell3 :: Maybe Int -> Cell3 -> String
    prettyShowCell3 n (site, edges) =
      "Site " ++ ppShow site ++ " :\n" ++ (prettyShowEdges3 n edges)


asTriplet :: [a] -> (a, a, a)
asTriplet [x,y,z] = (x,y,z)
asTriplet _       = (undefined, undefined, undefined)

getNeighborsCenters :: Delaunay -> Int -> Map Ridge3 (Maybe Point3)
getNeighborsCenters Delaunay {_sites=_, _facets=facets} l =
  fromList $ map (\(i,j,k) -> (S.fromList [i,j,k],
                               maybeListElementWith
                               neighbors
                               (findIndex (f [i,j,k]) neighborsAsIndices)
                               (asTriplet . _center . _simplex . (!!) facets)))
                 [(i1,i2,i3), (i4,i1,i2), (i3,i4,i1), (i2,i3,i4)]
  where
    facet = facets!!l
    -- getFacetVertices :: Facet -> ([Int], [[Double]])
    -- getFacetVertices = _vertices
    -- getFacetCenter :: Facet -> [Double]
    -- getFacetCenter = _center
    [i1, i2, i3, i4] = (S.toList . fst . _vertices . _simplex) facet
    neighbors = _neighbours facet
    neighborsAsIndices = map (fst . _vertices . _simplex . (!!) facets) neighbors
    f x indices = all ((flip S.member) indices) x -- mieux: S.subsetOf (S.fromList x) indices
    maybeListElementWith list n g = maybe Nothing (Just . g . (!!) list) n



allEdges :: Delaunay -> Map Ridge3 Edge3
allEdges tess =
  foldr (update tess) empty [0 .. length (_facets tess) - 1]

getCells :: [Site] -> Map Ridge3 Edge3 -> [Cell3]
getCells sites edgemap =
  map (\i ->
          (sites!!i,
           elems (M.filterWithKey (\i1i2i3 _ -> S.member i i1i2i3) edgemap)))
      [0 .. length sites - 1]

voronoi3 :: Delaunay -> Voronoi3
voronoi3 tess = getCells (_sites tess) (allEdges tess)

update :: Delaunay -> Int -> Map Ridge3 Edge3 -> Map Ridge3 Edge3
update tess i edges =
  M.union edges edgemap
  where
    facet = _facets tess !! i
    top = _top facet
    center@(cx,cy,cz) = asTriplet $ _center (_simplex facet)
    -- ridgeVertices :: CentredPolytope -> (IntSet, [[Double]])
    -- ridgeVertices = _vertices
    -- asSet :: Ridge3 -> IntSet
    -- asSet (i1,i2,i3) = S.fromList [i1,i2,i3]
    ridgesnormals = map
                    ((fst . _vertices) &&& (asTriplet . _normal))
                    (map fst3 (_ridges facet))
    distances = map
                    ((fst . _vertices . fst3) &&& (thd3))
                    (_ridges facet)
    ridgesnormals' = map ((fst . _vertices . fst3) &&& (minus . _center . fst3))
                         (_ridges facet)
    f i1i2i3 center' =
      if isNothing center'
        then if fromJust (lookup i1i2i3 distances) < 0 -- == Just 0
               -- bizarre ce mix de ridgenormals et ridgenormals'
               then Just (IEdge3 (center, negateT $ fromJust $ lookup i1i2i3 ridgesnormals))
               else Just (IEdge3 (center, fromJust $ lookup i1i2i3 ridgesnormals')) -- if top then asTriplet vec else asTriplet $ map negate vec))
        else let c = fromJust center' in
             if c == center
              then Nothing
              else Just (Edge3 (center, c))
    edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)
    minus [x,y,z] = (cx-x,cy-y,cz-z)
    negateT (x,y,z) = (-x,-y,-z)

-- --
getVertexNeighbors :: Delaunay -> Index -> [Facet]
getVertexNeighbors tess i =
  filter (\facet -> S.member i ((fst . _vertices . _simplex) facet)) (_facets tess)

getVertexRidges :: Delaunay -> Index -> [(CentredPolytope, Double)]
getVertexRidges tess i =
  let facets = getVertexNeighbors tess i in
  concat $ map (\facet -> filter (\(polytope, _) -> S.member i ((fst . _vertices) polytope)) (map (fst3 &&& thd3) $ _ridges facet)) facets

-- edgesFromTwoFacets :: Facet -> Facet -> Maybe [Edge3]
-- edgesFromTwoFacets facet1 facet2 =
--   if isNeighbor
--     then xxx
--     else Nothing
--   where
--     isNeighbor = (S.toList . fst . _vertices . _simplex) facet1 `elem` (_neighbours facet2)
--
-- update' :: Delaunay -> Index -> [Edge3] -> [Edge3]
-- update' tess i edges =
--   edges ++ newedges
--   where



truncEdge3 :: Box3 -> Edge3 -> Edge3
truncEdge3 (xmin, xmax, ymin, ymax, zmin, zmax) edge =
  if isIEdge edge
    then TIEdge3 ((p1,p2,p3), (p1+(factor v1 v2 v3)*v1,
                  p2+(factor v1 v2 v3)*v2, p3+(factor v1 v2 v3)*v3))
    else edge
  where
    isIEdge (IEdge3 _) = True
    isIEdge _          = False
    IEdge3 ((p1,p2,p3), (v1,v2,v3)) = edge
    -- factor u1 u2 u3 | u1==0 && u2==0 = if u3>0 then (zmax-p3)/u3 else (zmin-p3)/u3
    --                 | u1==0 && u3==0 = if u2>0 then (ymax-p2)/u2 else (ymin-p2)/u2
    --                 | u2==0 && u3==0 = if u1>0 then (xmax-p1)/u1 else (xmin-p1)/u1
    --                 | otherwise = min (min (factor u1 0 0) (factor 0 u2 0))
    --                                   (factor 0 0 u3)
    -- IEdge3 ((p1,p2,p3), (v1,v2,v3)) = edge
    factor u1 u2 u3 | u3==0 = factor2 (xmin,xmax,ymin,ymax) (p1,p2) (u1,u2)
                    | u2==0 = factor2 (zmin,zmax,xmin,xmax) (p3,p1) (u3,u1)
                    | u1==0 = factor2 (ymin,ymax,zmin,zmax) (p2,p3) (u2,u3)
                    | otherwise = min (min (factor u1 u2 0) (factor 0 u2 u3))
                                      (factor u1 0 u3)

clipVoronoi3 :: Box3 -> Voronoi3 -> Voronoi3
clipVoronoi3 box v = map (\(site,edges) -> (site, map (truncEdge3 box) edges)) v

testv3 :: IO Voronoi3
testv3 = do
  tess <- test2
  return $ voronoi3 tess


voronoi3ForRgl :: Voronoi3 -> String
voronoi3ForRgl v = intercalate "\nopend3d()\n" $ map cellForRgl v
  where
    cellForRgl :: Cell3 -> String
    cellForRgl (_, edges) = unlines $ map f edges
      where
        f :: Edge3 -> String
        f edge = case edge of
          Edge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ show y ++ "))"
          TIEdge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ show y ++ "), col=c(\"red\",\"red\"))"
