module Main
  where
import           Voronoi3D
import Voronoi2D
import           Data.Tuple.Extra (thd3, (&&&))
import           Delaunay
import           System.IO
import           Text.Show.Pretty


main :: IO ()
main = do
  -- r <- testv2
  -- prettyShowVoronoi2 r (Just 3)
  -- let vv = clipVoronoi2 (-1,3,-1,3) r
  -- writeFile "rgl.txt" (voronoi2ForR vv)
  -- prettyShowVoronoi2 vv (Just 3)
  tess <- test2
  putStrLn $ ppShow tess
  -- let c = cell tess 0
  -- putStrLn $ ppShow c
  -- let c1 = cell tess 1
  -- putStrLn $ ppShow c1
  -- let c2 = cell tess 2
  -- putStrLn $ ppShow c2
  -- let c3 = cell tess 3
  -- putStrLn $ ppShow c3
--  putStrLn $ ppShow $ map (cell' tess) [0 .. (length $ _sites tess) - 1]
--  let distances = map (map thd3 . _ridges) (_facets tess)
  -- putStrLn $ ppShow $ map _ridges (_facets tess)
  -- putStrLn $ ppShow $ (_facets tess !! 6, _facets tess !! 7)
--   mapM (putStrLn . show) distances
--   mapM (putStrLn . show . (map ((<0) &&& (==0)))) distances
  let r = voronoi3 tess
--  r <- testv2
-- --  r <- testv3
  prettyShowVoronoi3 r (Just 3)
--  putStrLn $ ppShow r
--   let vv = clipVoronoi3 (-2,2,-2,2,-2,2) r
  writeFile "rgl.txt" (voronoi3ForRgl r)
--  writeFile "rgl.txt" (voronoi2ForR r)
--   prettyShowVoronoi3 vv (Just 3)
