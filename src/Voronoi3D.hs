module Voronoi3D
  (Edge3
 , Cell3
 , Voronoi3
 , prettyShowVoronoi3
 , clipVoronoi3
 , testv3
 , voronoi3ForRgl
 , voronoiCell3
 , voronoi3)
  where
import           Data.List
import           Data.Maybe
import           Data.Tuple.Extra   (both)
import           Delaunay
import           Text.Show.Pretty   (ppShow)
import           VoronoiShared

type Point3 = (Double, Double, Double)
type Vector3 = (Double, Double, Double)
data Edge3 = Edge3 (Point3, Point3) | IEdge3 (Point3, Vector3)
             | TIEdge3 (Point3, Point3)
              deriving Show
type Cell3 = (Site, [Edge3])
type Voronoi3 = [Cell3]
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

edgeToEdge3 :: Edge -> Edge3
edgeToEdge3 (Edge (x, y))  = Edge3 (both asTriplet (x, y))
edgeToEdge3 (IEdge (x, v)) = IEdge3 (both asTriplet (x, v))

equalRidges :: Ridge -> Ridge -> Bool
equalRidges ridge1 ridge2 = -- same ridges or same centers and parallel normals
  _vertices p1 == _vertices p2 -- inutile je pense, c'est déjà unique
  ||
  (length f1 == 1 && length f2 == 1 &&
   _center p1 == _center p2 &&
   crossProduct (asTriplet $ _normal p1) (asTriplet $ _normal p2) == (0,0,0))
  where
    (p1, f1, _) = ridgeAsTriplet ridge1
    (p2, f2, _) = ridgeAsTriplet ridge2

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (x1,y1,z1) (x2,y2,z2) = (v3x, v3y, v3z)
  where
    v3x = y1 * z2   -   y2 * z1
    v3y = z1 * x2   -   z2 * x1
    v3z = x1 * y2   -   x2 * y1

voronoiCell3 :: Delaunay -> Index -> [Edge3]
voronoiCell3 tess i =
  let ridges = uniqueWith equalRidges $ getVertexRidges tess i in
  map (edgeToEdge3 . fromJust) $
      filter isJust $ map (edgesFromRidge tess) ridges --, map (_vertices . fst3) ridges)

voronoi3 :: Delaunay -> Voronoi3
voronoi3 tess = let sites = _sites tess in
                    zip sites (map (voronoiCell3 tess) [0 .. length sites -1])


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
          IEdge3 (x,y) ->
            "segments3d(rbind(c" ++ show x ++ ", c" ++ (show $ sumTriplet x y) ++ "), col=c(\"red\",\"red\"))"
        sumTriplet (a,b,c) (a',b',c') = (a+a',b+b',c+c')
