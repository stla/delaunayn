module Main
  where
import Voronoi3D
--import Voronoi2D
import System.IO
import Delaunay (test2)
import Text.Show.Pretty

main :: IO ()
main = do
  -- r <- testv2
  -- prettyShowVoronoi2 r (Just 3)
  -- let vv = clipVoronoi2 (-1,3,-1,3) r
  -- writeFile "rgl.txt" (voronoi2ForR vv)
  -- prettyShowVoronoi2 vv (Just 3)
  tess <- test2
  putStrLn $ ppShow tess
  let r = voronoi3 tess
--  r <- testv3
  prettyShowVoronoi3 r (Just 3)
  let vv = clipVoronoi3 (-2,2,-2,2,-2,2) r
  writeFile "rgl.txt" (voronoi3ForRgl vv)
  prettyShowVoronoi3 vv (Just 3)
