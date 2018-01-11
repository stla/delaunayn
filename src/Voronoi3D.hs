module Voronoi3D
  (Edge3
 , Cell3
 , Voronoi3
 , prettyShowVoronoi3
 , voronoi3
 , clipVoronoi3
 , testv3
 , voronoi3ForRgl
 , circumcenter)
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
              deriving Show
-- type Edge3' = (Edge3, Index)
type Cell3 = (Site, [Edge3])
type Voronoi3 = [Cell3]
type Ridge3 = (Index, Index, Index)
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
                 [(i1,i2,i3), (i4,i1,i2), (i3,i4,i1), (i2,i3,i4)]
  where
    facet = facets!!l
    [i1, i2, i3, i4] = _vertices facet
    neighbors = _neighbours facet
    neighborsAsIndices = map (_vertices . (!!) facets) neighbors
    f x indices = all (`elem` indices) x
    maybeListElementWith list n g = maybe Nothing (Just . g . (!!) list) n

-- allEdges :: Delaunay -> Map Ridge3 (Edge3', Edge3', Edge3')
-- allEdges tess@(Delaunay {_sites=_, _facets=facets}) =
--   foldr (update tess) empty [0 .. length facets - 1]
--
-- getCells :: [Site] -> Map Ridge3 (Edge3', Edge3', Edge3') -> [Cell3]
-- getCells sites edgemap =
--   map (\i ->
--           (sites!!i,
--            elems (M.map fst (M.filter ((==i).snd) (M.map (\(a,_,_) -> a) edgemap)))
--            ++
--            elems (M.map fst (M.filter ((==i).snd) (M.map (\(_,b,_) -> b) edgemap)))
--            ++
--            elems (M.map fst (M.filter ((==i).snd) (M.map (\(_,_,c) -> c) edgemap)))))
--       [0 .. length sites - 1]
allEdges :: Delaunay -> Map Ridge3 Edge3
allEdges tess@(Delaunay {_sites=_, _facets=facets}) =
  foldr (update tess) empty [0 .. length facets - 1]

getCells :: [Site] -> Map Ridge3 Edge3 -> [Cell3]
getCells sites edgemap =
  map (\i ->
          (sites!!i,
           elems (M.filterWithKey (\(i1,i2,i3) _ -> i `elem` [i1,i2,i3]) edgemap)))
          --  ++
          --  elems (M.filterWithKey (\(i1,i2,i3) _ -> i2==i) edgemap)
          --  ++
          --  elems (M.filterWithKey (\(i1,i2,i3) _ -> i3==i) edgemap)))
      [0 .. length sites - 1]

voronoi3 :: Delaunay -> Voronoi3
voronoi3 tess = getCells (_sites tess) (allEdges tess)

-- TU DUPLIQUES POUR RIEN !!!!!!!!!!
-- update :: Delaunay -> Int -> Map Ridge3 (Edge3',Edge3',Edge3') -> Map Ridge3 (Edge3',Edge3',Edge3')
-- update tess@(Delaunay {_sites=sites, _facets=facets}) i edges =
--   M.union edges edgemap
--   where
--     center@(cx,cy,cz) = asTriplet $ _center (facets!!i)
--     f (i1,i2,i3) center' =
--       if isNothing center'
--         then Just ((IEdge3 (center, vec), i1), (IEdge3 (center, vec), i2), (IEdge3 (center, vec), i3))
--         else let c = fromJust center' in
--              if c == center
--               then Nothing
--               else Just ((Edge3 (center, c), i1), (Edge3 (center, c), i2), (Edge3 (center, c), i3))
--         where
--           vec = (mx-cx, my-cy, mz-cz)
--           [x1,y1,z1] = sites!!i1
--           [x2,y2,z2] = sites!!i2
--           [x3,y3,z3] = sites!!i3
--           mx = (x1+x2+x3)/3
--           my = (y1+y2+y3)/3
--           mz = (z1+z2+z3)/3
--     edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)
update :: Delaunay -> Int -> Map Ridge3 Edge3 -> Map Ridge3 Edge3
update tess@(Delaunay {_sites=sites, _facets=facets}) i edges =
  M.union edges edgemap
  where
    facet = facets!!i
    top = _top facet
    center@(cx,cy,cz) = asTriplet $ _center facet
    f (i1,i2,i3) center' =
      if isNothing center'
        then Just (IEdge3 (center,  if top then asTriplet vec else asTriplet $ map negate vec))
        else let c = fromJust center' in
             if c == center
              then Nothing
              else Just (Edge3 (center, c))
        where
          -- (x,y,z) = circumcenter (asTriplet $ sites!!i1) (asTriplet $ sites!!i2) (asTriplet $ sites!!i3)
          -- vec = (x-cx,y-cy,z-cz)
--          vec = (mx-cx, my-cy, mz-cz)
          vec = [u!!1*v!!2-u!!2*v!!1, u!!2*v!!0-u!!0*v!!2, u!!0*v!!1-u!!1*v!!0]
          [x1,y1,z1] = sites!!i1
          [x2,y2,z2] = sites!!i2
          [x3,y3,z3] = sites!!i3
          u = [x2-x1,y2-y1,z2-z1]
          v = [x3-x1,y3-y1,z3-z1]
          -- -- mx = (x1+x2+x3)/3 -- c'est pas ça
          -- -- my = (y1+y2+y3)/3
          -- -- mz = (z1+z2+z3)/3
    edgemap = mapMaybeWithKey f (difference (getNeighborsCenters tess i) edges)
-- O = center
-- OM.AB = 0: (mx-cx)*u!!0 + (my-cy)*u!!1 + (mz-cz)*u!!2 = 0
-- OM.AC = 0: (mx-cx)*v!!0 + (my-cy)*v!!1 + (mz-cz)*v!!2 = 0
-- I: milieu de AB
-- IM.AB = 0: (mx-ix)*u0+(my-iy)*u1+(mz-iz)*u2=0

--circumcenter :: Point3 -> Point3 -> Point3 -> Point3
circumcenter (u1,v1,w1) (u2,v2,w2) (u3,v3,w3) = -- A B C
  --(u1+o1,v1+o2,w1+o3)
  ((x1'*o1+y1''*o2 , x2'*(o1)+y2''*(o2), x3'*o1+y3''*o2),
  (o1,o2,o3), (x1',x2',x3'), (y1'',y2'',y3''), (znew1,znew2,znew3), mu, scal, x1'*y1''+x2'*y2''+x3'*y3'', x3)
--  (dx/2/a, dy/2/a, x1')
--  (dx/2/a*x1'+dy/2/a*y1''+u1, dx/2/a*x2'+dy/2/a*y2''+v1, mu)
   --(u1+dx/2/a*x1'+dy/2/a*y1''-u1-0.5, v1+dx/2/a*x2'+dy/2/a*y2''-v1-0.5, w1+dx/2/a*x3'+dy/2/a*y3''-w1-0.5)
  -- AC' = mu AB + nu AC
  -- orthonormalise AB : mu = 1/sqrt(x1²+x2²+x3²)
  -- AB.AC' = 0 : (cx'-ax)*x1 + (cy'-ay)*x2 + (cz'-az)*x3 = 0
  --              (mu*x1+nu*y1)*x1 + (mu*x2+nu*y2)*x2 + (mu*x3+nu*y3)*x3 = 0
  -- prend mu=1 => nu*(x1y1+x2y2+x3y3) = -x1²-x2²-x3²
  -- non, on orthonormalise
  -- nu*(x1y1+x2y2+x3y3) = -mu*(x1²+x2²+x3²)
  -- mu = 1/sqrt(x1²+x2²+x3²)
  where
    (o1,o2,o3) = (u1+dx/2/a, v1+dy/2/a, w1+1/2/a)
    c = x1*y1+x2*y2+x3*y3 -- c'est AB.AC ; si ça fait 0 on a déjà l'orthogonalité
    nu = if c==0 then 1 / sqrt (y1*y1+y2*y2+y3*y3) else mmm/c --sqrt (x1*x1+x2*x2+x3*x3) / c
    znew1 = x2'*y3''-x3'*y2''
    znew2 = x3'*y1''-x1'*y3''
    znew3 = x1'*y2''-x2'*y1''
--    u!!1*v!!2-u!!2*v!!1, u!!2*v!!0-u!!0*v!!2, u!!0*v!!1-u!!1*v!!0
    mu = 1 -- / sqrt (x1*x1+x2*x2+x3*x3)
    mmm = sqrt (x1*x1+x2*x2+x3*x3)
    x1 = u2-u1
    y1 = u3-u1
    x2 = v2-v1
    y2 = v3-v1
    x3 = w2-w1
    y3 = w3-w1
    x1' = mu*x1
    x2' = mu*x2
    x3' = mu*x3
    scal = (x1*y1+x2*y2+x3*y3)*mu*mu
    -- y1' = y1 - scal*x1
    -- y2' = y2 - scal*x2
    -- y3' = y3 - scal*x3
    y1' = x1'+nu*y1
    y2' = x2'+nu*y2
    y3' = x3'+nu*y3
    n = 1/sqrt (y1'*y1'+y2'*y2'+y3'*y3') * mmm
    y1'' = y1'*n
    y2'' = y2'*n
    y3'' = y3'*n
    p = x1'*x1'+y1''*y1''
    q = x2'*x2'+y2''*y2''
    r = x3'*x3'+y3''*y3''
    a = x2'*y3''-x3'*y2'' - (x1'*y3''-x3'*y1'') + x1'*y2''-x2'*y1''
    dx = q*y3''-r*y2'' - (p*y3''-r*y1'') + p*y2''-q*y1''
    dy = x2'*r-x3'*q - (x1'*r-x3'*p) + x1'*q-x2'*p
    -- p = x1'*x1'+y1''*y1''
    -- q = x2'*x2'+y2''*y2''
    -- r = x3'*x3'+y3''*y3''
    -- a = x2'*y3''-x3'*y2'' - (x1'*y3''-x3'*y1'') + x1'*y2''-x2'*y1''
    -- dx = q*y3''-r*y2'' - (p*y3''-r*y1'') + p*y2''-q*y1''
    -- dy = x2'*r-x3'*q - (x1'*r-x3'*p) + x1'*q-x2'*p

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
--
det3x3 :: [Double] -> [Double] -> [Double] -> Double
det3x3 [x1,x2,x3] [y1,y2,y3] [z1,z2,z3] =
  x1*y2*z3 + x2*y3*z1 + x3*y1*z2 - (x3*y2*z1) - (x2*y1*z3) - (x1*y3*z2)

circumcenter3d :: [Double] -> [Double] -> [Double] -> [Double] -> (Double, Double, Double)
circumcenter3d [x1,y1,z1] [x2,y2,z2] [x3,y3,z3] [x4,y4,z4] =
  (dx/2/a, dy/2/a, dz/2/a)
  where
    det4x4 [u1,u2,u3,u4] [v1,v2,v3,v4] [w1,w2,w3,w4] =
      - det3x3 [u2,u3,u4] [v2,v3,v4] [w2,w3,w4] +
      det3x3 [u1,u3,u4] [v1,v3,v4] [w1,w3,w4] -
      det3x3 [u1,u2,u4] [v1,v2,v4] [w1,w2,w4] +
      det3x3 [u1,u2,u3] [v1,v2,v3] [w1,w2,w3]
    x = [x1,x2,x3,x4]
    y = [y1,y2,y3,y4]
    z = [z1,z2,z3,z4]
    a = det4x4 x y z
    ssq = zipWith3 (\u v w -> u*u+v*v+w*w) x y z
    dx = det4x4 ssq y z
    dy = det4x4 ssq x z
    dz = det4x4 ssq x y

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
