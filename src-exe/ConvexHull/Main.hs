module Main
  where
import ConvexHull
import Text.Show.Pretty

main :: IO ()
main = do
  chull <- test
  pPrint chull
