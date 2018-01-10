module Main
  where
import Voronoi3D

main :: IO ()
main = do
  r <- testv3
  prettyShowVoronoi3 r (Just 3)
