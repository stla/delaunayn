module Voronoi2D
  (Edge2
 , Cell2
 , Voronoi2
 , prettyShowVoronoi2
 , voronoiCell2
 , voronoi2
 , clipVoronoi2
 , voronoi2ForR
 , testv2)
  where
import           Data.List
import           Data.Maybe
import           Data.Tuple.Extra   (both)
import           Delaunay
import           Text.Show.Pretty   (ppShow)
import           VoronoiShared
import qualified Data.IntMap.Strict as IM

type Point2 = (Double, Double)
type Vector2 = (Double, Double)
data Edge2 = Edge2 (Point2, Point2) | IEdge2 (Point2, Vector2)
             | TIEdge2 (Point2, Point2)
              deriving Show
type Cell2 = (Site, [Edge2])
type Voronoi2 = [Cell2]
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

edgeToEdge2 :: Edge -> Edge2
edgeToEdge2 (Edge (x, y))  = Edge2 (both asPair (x, y))
edgeToEdge2 (IEdge (x, v)) = IEdge2 (both asPair (x, v))

voronoiCell2 :: Delaunay -> Index -> [Edge2]
voronoiCell2 tess i =
  let ridges = getVertexRidges tess i in
  map (edgeToEdge2 . fromJust) $
      filter isJust $ map (edgesFromRidge tess) ridges

voronoi2 :: Delaunay -> Voronoi2
voronoi2 tess = let sites = IM.elems $ IM.map _coordinates (_vertices tess) in
                    zip sites (map (voronoiCell2 tess) [0 .. length sites -1])

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
  putStrLn $ ppShow tess
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
          IEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x0+x1,y0+y1]) ++ ", col=\"red\")"
          TIEdge2 ((x0,y0),(x1,y1)) ->
            "segments(" ++ intercalate "," (map show [x0,y0,x1,y1]) ++ ", col=\"red\")"
