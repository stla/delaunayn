module Main
  where
import           Data.Graph
import           Data.List
import           Data.List.Index  (imap, ifilter)
import           Data.List.Unique
import qualified Data.IntMap.Strict    as IM
import qualified Data.Map.Strict  as M
import           Data.Tuple.Extra (thd3, (&&&))
import           Delaunay
import           Linear
import           System.IO
import           Text.Show.Pretty
import           Vis
import           Voronoi2D
import           Voronoi3D


connectedEdges :: Edge3 -> Edge3 -> Bool
connectedEdges (Edge3 (x1,x2)) (Edge3 (y1,y2)) = length (intersect [x1,x2] [y1,y2]) == 1
connectedEdges _ _ = False

edgeVertices :: Edge3 -> [[Double]]
edgeVertices (Edge3 ((x1,x2,x3),(y1,y2,y3))) = [[x1,x2,x3],[y1,y2,y3]]

delaunay3vis :: Delaunay -> VisObject Double
delaunay3vis tess = VisObjects $ concat $ map visRidge (M.elems $ allRidges tess)
  where
    visRidge ridge =
      [ Triangle (pts!!0) (pts!!1) (pts!!2) (makeColor 0 0 1 0.5)
      , Line Nothing ((pts!!0) : pts) black ]
      where
        pts = map (\p -> V3 (p!!0) (p!!1) (p!!2)) (_points $ _rsimplex ridge)


main :: IO ()
main = do
  -- r <- testv2
  -- prettyShowVoronoi2 r (Just 3)
  -- let vv = clipVoronoi2 (-1,3,-1,3) r
  -- writeFile "rgl.txt" (voronoi2ForR vv)
  -- prettyShowVoronoi2 vv (Just 3)

--   tess <- delaunay cuboctahedron
--   let c = voronoiCell3 tess 12
--       l = imap (\i edge -> (edge, i, findIndices (connectedEdges edge) c)) c
--       (gr, _, _) = graphFromEdges l
--   putStrLn $ ppShow $ -- flattenSCCs $
--               stronglyConnComp l
-- --  putStrLn $ ppShow $ scc gr
--   let verts = foldr union [] $ map edgeVertices c
--   putStrLn $ show verts

  tess <- test3
  putStrLn $ ppShow tess
--  writeFile "rgl.txt" (delaunay3rgl tess)
--  display (defaultOpts {optWindowName = "display test"}) (delaunay3vis tess)
  --putStrLn $ ppShow $ IM.map _owner (_facets tess)
  -- let dd = Delaunay {
  --     _facets = IM.filterWithKey (\i f -> _owner f == Just i || _owner f == Nothing) (_facets tess)
  --   , _sites = _sites tess
  --   , _vrneighbors = _vrneighbors tess
  --   , _vfneighbors = _vfneighbors tess
  -- }
--   let dd = tess
--   -- let c = cell tess 0
--   -- putStrLn $ ppShow c
--   -- let c1 = cell tess 1
--   -- putStrLn $ ppShow c1
--   -- let c2 = cell tess 2
--   -- putStrLn $ ppShow c2
--   -- let c3 = cell tess 3
--   -- putStrLn $ ppShow c3
-- --  putStrLn $ ppShow $ map (cell' tess) [0 .. (length $ _sites tess) - 1]
-- --  let distances = map (map thd3 . _ridges) (_facets tess)
--   -- putStrLn $ ppShow $ map _ridges (_facets tess)
--   -- putStrLn $ ppShow $ (_facets tess !! 6, _facets tess !! 7)
-- --   mapM (putStrLn . show) distances
-- --   mapM (putStrLn . show . (map ((<0) &&& (==0)))) distances
--   let r = voronoi3 dd
-- -- -- --  r <- testv2
-- -- -- -- --  r <- testv3
-- --   prettyShowVoronoi3 r (Just 3)
--   putStrLn $ ppShow r
-- -- --   let vv = clipVoronoi3 (-2,2,-2,2,-2,2) r
--   writeFile "rgl.txt" (voronoi3ForRgl r)
--  writeFile "rgl.txt" (voronoi2ForR r)
--   prettyShowVoronoi3 vv (Just 3)
