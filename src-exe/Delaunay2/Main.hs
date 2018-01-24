module Main
  where
import Delaunay2
import ConvexHull.Examples
import Text.Show.Pretty

main :: IO ()
main = do

  tess <- delaunay2 [[0,0],[0,1],[1,0],[1,1],[0.5,0.5]] False
  pPrint tess
