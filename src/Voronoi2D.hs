module Voronoi2D
  (Edge2
 , Cell2
 , Voronoi2
 , prettyShowVoronoi2
 , voronoi2
 , clipVoronoi2
 , voronoi2ForR
 , testv2)
  where
import           Delaunay
import           Data.List
import           Data.Map.Strict (Map, mapMaybeWithKey, difference, fromList,
                                  elems, empty)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Show.Pretty (ppShow)
import           VoronoiShared

type Point2 = (Double, Double)
type Vector2 = (Double, Double)
data Edge2 = Edge2 (Point2, Point2) | IEdge2 (Point2, Vector2)
             | TIEdge2 (Point2, Point2)
              deriving Show -- pourrait faire une instance show pour virer le 2
--type Edge2' = (Edge2, Index)
type Cell2 = (Site, [Edge2])
type Voronoi2 = [Cell2]
type Ridge2 = (Index, Index)
type Box2 = (Double, Double, Double, Double)

prettyShowVoronoi2 :: Voronoi2 -> Maybe Int -> IO ()
prettyShowVoronoi2 v m = do
  let string = intercalate "\n---\n" (map (prettyShowCell2 m) v)
  putStrLn string
  where
    roundPairPoint2 :: (Point2, Point2) -> Int -> (Point2, Point2)
    roundPairPoint2 ((x1,x2), (y1,y2)) n =
      (asPair $ map (approx n) [x1,x2], asPair $ map (approx n) [y1,y2])
    prettyShowEdge2 :: Maybe Int -> Edge2 -> String
    prettyShowEdge2 n edge = case edge of
      Edge2 x   -> " Edge " ++ string x
      IEdge2 x  -> " IEdge " ++ string x
      TIEdge2 x -> " TIEdge " ++ string x
      where
        string x = ppShow $ maybe x (roundPairPoint2 x) n
    prettyShowEdges2 :: Maybe Int -> [Edge2] -> String
    prettyShowEdges2 n edges = intercalate "\n" (map (prettyShowEdge2 n) edges)
    prettyShowCell2 :: Maybe Int -> Cell2 -> String
    prettyShowCell2 n (site, edges) =
      "Site " ++ ppShow site ++ " :\n" ++ (prettyShowEdges2 n edges)

asPair :: [Double] -> (Double, Double)
asPair [a,b] = (a,b)
asPair _ = (undefined, undefined)

getNeighborsCenters :: Delaunay -> Int -> Map Ridge2 (Maybe Point2)
getNeighborsCenters Delaunay {_sites=_, _facets=facets} k =
  fromList $ map (\(i,j) -> ((i,j),
                             maybeListElementWith
                             neighbors
                             (findIndex (f [i,j]) neighborsAsIndices)
                             (asPair . _center . (!!) facets)))
                   [(i1,i2), (i1,i3), (i2,i3)]
  where
    facet = facets!!k
    [i1, i2, i3] = _vertices facet
    neighbors = _neighbours facet
    neighborsAsIndices = map (_vertices . (!!) facets) neighbors
    f x indices = all (`elem` indices) x
    --tripletAsList (a,b,c) = [a,b,c]
    maybeListElementWith list l g = maybe Nothing (Just . g . (!!) list) l

allEdges :: Delaunay -> Map Ridge2 Edge2
allEdges tess@(Delaunay {_sites=_, _facets=facets}) =
  foldr (update tess) empty [0 .. length facets - 1]

-- getCells :: [Site] -> Map Ridge2 (Edge2', Edge2') -> [Cell2]
-- getCells sites edgemap =
--   map (\i ->
--           (sites!!i,
--            elems (M.map fst (M.filter ((==i).snd) (M.map fst edgemap)))
--            ++
--            elems (M.map fst (M.filter ((==i).snd) (M.map snd edgemap)))))
--       [0 .. length sites - 1]
getCells :: [Site] -> Map Ridge2 Edge2 -> [Cell2]
getCells sites edgemap =
  map (\i ->
          (sites!!i,
           elems (M.filterWithKey (\(i1,i2) _ -> i `elem` [i1,i2]) edgemap)))
      [0 .. length sites - 1]

voronoi2 :: Delaunay -> Voronoi2
voronoi2 tess = getCells (_sites tess) (allEdges tess)

-- update :: Delaunay -> Int -> Map Ridge2 (Edge2',Edge2') -> Map Ridge2 (Edge2',Edge2')
-- update tess@(Delaunay {_sites=sites, _facets=facets}) i edges =
--   M.union edges edgemap
--   where
--     center@(cx,cy) = asPair $ _center (facets!!i)--circumcenter (getTriangle sites (_triangle (facets!!i)))
--     f (i1,i2) center' =
--       if isNothing center'
--         then Just ((IEdge2 (center, vec), i1), (IEdge2 (center, vec), i2))
--         else let c = fromJust center'
--              in
--              if c == center
--               then Nothing
--               else Just ((Edge2 (center, c), i1), (Edge2 (center, c), i2))
--         where
--           vec = (mx-cx, my-cy)
--           [x1,y1] = sites!!i1
--           [x2,y2] = sites!!i2
--           mx = (x1+x2)/2
--           my = (y1+y2)/2
--     edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)
update :: Delaunay -> Int -> Map Ridge2 Edge2 -> Map Ridge2 Edge2
update tess@(Delaunay {_sites=sites, _facets=facets}) i edges =
  M.union edges edgemap
  where
    center@(cx,cy) = asPair $ _center (facets!!i)--circumcenter (getTriangle sites (_triangle (facets!!i)))
    f (i1,i2) center' =
      if isNothing center'
        then if vec==(0,0)
               then Just (IEdge2 (center, (y1-y2,x2-x1))) -- pb orientation
               else Just (IEdge2 (center, vec))
        else let c = fromJust center' in
             if c == center
              then Nothing
              else Just (Edge2 (center, c))
        where
          vec = (mx-cx, my-cy)
          [x1,y1] = sites!!i1
          [x2,y2] = sites!!i2
          mx = (x1+x2)/2
          my = (y1+y2)/2
    edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)

truncEdge2 :: Box2 -> Edge2 -> Edge2
truncEdge2 box edge =
  if isIEdge edge
    then TIEdge2 (p, (p1 + factor * v1, p2 + factor * v2))
    else edge
  where
    isIEdge (IEdge2 _) = True
    isIEdge _          = False
    IEdge2 (p@(p1,p2), v@(v1,v2)) = edge
    factor = factor2 box p v

clipVoronoi2 :: Box2 -> Voronoi2 -> Voronoi2
clipVoronoi2 box v = map (\(site,edges) -> (site, map (truncEdge2 box) edges)) v

testv2 :: IO Voronoi2
testv2 = do
  tess <- test4
  return $ voronoi2 tess

voronoi2ForR :: Voronoi2 -> String
voronoi2ForR v = unlines $ map cellForRgl v
  where
    cellForRgl :: Cell2 -> String
    cellForRgl (_, edges) = unlines $ map f edges
      where
        f :: Edge2 -> String
        f edge = case edge of
          Edge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++ ")"
          TIEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++ ", col=\"red\")"
