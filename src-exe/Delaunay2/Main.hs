module Main
  where
import Delaunay2
import ConvexHull.Examples
import Text.Show.Pretty
import qualified Data.IntMap.Strict    as IM

main :: IO ()
main = do

  -- tess <- delaunay2 centricCube False
  -- pPrint tess

  x <- randomInSphere 1000
  tess <- delaunay2 x False
  print $ IM.size (_tiles tess)
