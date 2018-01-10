module Voronoi3D
  (Edge3
 , Cell3
 , Voronoi3
 , prettyShowVoronoi3
 , voronoi3
 , clipVoronoi3
 , testv3)
  where
import           Delaunay
import           Data.List
import           Data.Map.Strict (Map, mapMaybeWithKey, difference, fromList,
                                  elems, empty)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Show.Pretty (ppShow)
import           VoronoiShared

type Point3 = (Double, Double, Double)
type Vector3 = (Double, Double, Double)
data Edge3 = Edge3 (Point3, Point3) | IEdge3 (Point3, Vector3)
             | TIEdge3 (Point3, Point3)
              deriving Show -- pourrait faire une instance show pour virer le 3
type Edge3' = (Edge3, Index)
type Cell3 = (Site, [Edge3])
type Voronoi3 = [Cell3]
type Ridge3 = (Index, Index, Index)
type Box3 = (Double, Double, Double, Double, Double, Double) -- xmin xmax ymin ymax zmin zmax

prettyShowVoronoi3 :: Voronoi3 -> Maybe Int -> IO ()
prettyShowVoronoi3 v m = do
  let string = intercalate "\n---\n" (map (prettyShowCell3 m) v)
  putStrLn string
  where
    roundPairPoint3 :: (Point3, Point3) -> Int -> (Point3, Point3)
    roundPairPoint3 ((x1,x2,x3), (y1,y2,y3)) n =
      (asTriplet $ map (approx n) [x1,x2,x3], asTriplet $ map (approx n) [y1,y2,y3])
    prettyShowEdge3 :: Maybe Int -> Edge3 -> String
    prettyShowEdge3 n edge = case edge of
      Edge3 x -> " Edge " ++ string x
      IEdge3 x -> " IEdge " ++ string x
      TIEdge3 x -> " TIEdge " ++ string x
      where
        string x = ppShow $ maybe x (roundPairPoint3 x) n
    prettyShowEdges3 :: Maybe Int -> [Edge3] -> String
    prettyShowEdges3 n edges = intercalate "\n" (map (prettyShowEdge3 n) edges)
    prettyShowCell3 :: Maybe Int -> Cell3 -> String
    prettyShowCell3 n (site, edges) =
      "Site " ++ ppShow site ++ " :\n" ++ (prettyShowEdges3 n edges)


asTriplet :: [Double] -> (Double, Double, Double)
asTriplet [a,b,c] = (a,b,c)
asTriplet _ = (undefined, undefined, undefined)

getNeighborsCenters :: Delaunay -> Int -> Map Ridge3 (Maybe Point3)
getNeighborsCenters Delaunay {_sites=_, _facets=facets} l =
  fromList $ map (\(i,j,k) -> ((i,j,k),
                               maybeListElementWith
                               neighbors
                               (findIndex (f [i,j,k]) neighborsAsIndices)
                               (asTriplet . _center . (!!) facets)))
                 [(i1,i2,i3), (i1,i2,i4), (i1,i3,i4), (i2,i3,i4)]
  where
    facet = facets!!l
    [i1, i2, i3, i4] = _vertices facet
    neighbors = _neighbours facet
    neighborsAsIndices = map (_vertices . (!!) facets) neighbors
    f x indices = all (`elem` indices) x
    maybeListElementWith list n g = maybe Nothing (Just . g . (!!) list) n

allEdges :: Delaunay -> Map Ridge3 (Edge3', Edge3', Edge3')
allEdges tess@(Delaunay {_sites=_, _facets=facets}) =
  foldr (update tess) empty [0 .. length facets - 1]

getCells :: [Site] -> Map Ridge3 (Edge3', Edge3', Edge3') -> [Cell3]
getCells sites edgemap =
  map (\i ->
          (sites!!i,
           elems (M.map fst (M.filter ((==i).snd) (M.map (\(a,_,_) -> a) edgemap)))
           ++
           elems (M.map fst (M.filter ((==i).snd) (M.map (\(_,b,_) -> b) edgemap)))
           ++
           elems (M.map fst (M.filter ((==i).snd) (M.map (\(_,_,c) -> c) edgemap)))))
      [0 .. length sites - 1]

voronoi3 :: Delaunay -> Voronoi3
voronoi3 tess = getCells (_sites tess) (allEdges tess)

update :: Delaunay -> Int -> Map Ridge3 (Edge3',Edge3',Edge3') -> Map Ridge3 (Edge3',Edge3',Edge3')
update tess@(Delaunay {_sites=sites, _facets=facets}) i edges =
  M.union edges edgemap
  where
    center@(cx,cy,cz) = asTriplet $ _center (facets!!i)
    f (i1,i2,i3) center' =
      if isNothing center'
        then Just ((IEdge3 (center, vec), i1), (IEdge3 (center, vec), i2), (IEdge3 (center, vec), i3))
        else let c = fromJust center' in
             if c == center
              then Nothing
              else Just ((Edge3 (center, c), i1), (Edge3 (center, c), i2), (Edge3 (center, c), i3))
        where
          vec = (mx-cx, my-cy, mz-cz)
          [x1,y1,z1] = sites!!i1
          [x2,y2,z2] = sites!!i2
          [x3,y3,z3] = sites!!i3
          mx = (x1+x2+x3)/3
          my = (y1+y2+y3)/3
          mz = (z1+z2+z3)/3
    edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)

truncEdge3 :: Box3 -> Edge3 -> Edge3
truncEdge3 (xmin, xmax, ymin, ymax, zmin, zmax) edge =
  if isIEdge edge
    then TIEdge3 ((p1,p2,p3), (p1+(factor v1 v2 v3)*v1, p2+(factor v1 v2 v3)*v2, p3+(factor v1 v2 v3)*v3))
    else edge
  where
    isIEdge (IEdge3 _) = True
    isIEdge _          = False
    IEdge3 ((p1,p2,p3), (v1,v2,v3)) = edge
    factor u1 u2 u3 | u3==0 = factor2 (xmin,xmax,ymin,ymax) (p1,p2) (u1,u2)
                    | u2==0 = factor2 (xmin,xmax,zmin,zmax) (p1,p3) (u1,u3)
                    | u1==0 = factor2 (ymin,ymax,zmin,zmax) (p2,p3) (u2,u3)
                    | otherwise = min (min (factor u1 u2 0) (factor 0 u2 u3)) (factor u1 0 u3)

clipVoronoi3 :: Box3 -> Voronoi3 -> Voronoi3
clipVoronoi3 box v = map (\(site,edges) -> (site, map (truncEdge3 box) edges)) v

testv3 :: IO Voronoi3
testv3 = do
  tess <- test2
  return $ voronoi3 tess
