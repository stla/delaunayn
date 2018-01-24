module Main
  where
import           ConvexHull.Examples
import qualified Data.IntMap.Strict  as IM
import           Delaunay2
import           Text.Show.Pretty

main :: IO ()
main = do

  tess <- delaunay2 centricCube False
  pPrint tess

  -- x <- randomInSphere 1000
  -- tess <- delaunay2 x False
  -- print $ IM.size (_tiles tess)
