module Main
  where
import ConvexHull
import Text.Show.Pretty
import Data.List

main :: IO ()
main = do
  -- chull <- test
  -- pPrint chull

  let squareLattice = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]
  chull <- convexHull squareLattice False
  putStrLn "\n--- SQUARE LATTICE ---"
  pPrint chull

  let cube = [[-1,-1,-1]
             ,[-1,-1, 1]
             ,[-1, 1,-1]
             ,[-1, 1, 1]
             ,[ 1,-1,-1]
             ,[ 1,-1, 1]
             ,[ 1, 1,-1]
             ,[ 1, 1, 1]]
  chull2 <- convexHull cube False
  putStrLn "\n--- CUBE ---"
  pPrint chull2

  -- let hcube = [[-1,-1,-1,-1]
  --             ,[-1,-1, 1,-1]
  --             ,[-1, 1,-1,-1]
  --             ,[-1, 1, 1,-1]
  --             ,[ 1,-1,-1,-1]
  --             ,[ 1,-1, 1,-1]
  --             ,[ 1, 1,-1,-1]
  --             ,[ 1, 1, 1,-1]
  --             ,[-1,-1,-1, 1]
  --             ,[-1,-1, 1, 1]
  --             ,[-1, 1,-1, 1]
  --             ,[-1, 1, 1, 1]
  --             ,[ 1,-1,-1, 1]
  --             ,[ 1,-1, 1, 1]
  --             ,[ 1, 1,-1, 1]
  --             ,[ 1, 1, 1, 1]]
  -- chull <- convexHull hcube False
  -- pPrint (_allridges chull)
  -- pPrint (_allvertices chull)
  -- pPrint (_faces chull)

  -- let cube5 = [[-1,-1,-1,-1,-1]
  --             ,[-1,-1, 1,-1,-1]
  --             ,[-1, 1,-1,-1,-1]
  --             ,[-1, 1, 1,-1,-1]
  --             ,[ 1,-1,-1,-1,-1]
  --             ,[ 1,-1, 1,-1,-1]
  --             ,[ 1, 1,-1,-1,-1]
  --             ,[ 1, 1, 1,-1,-1]
  --             ,[-1,-1,-1, 1,-1]
  --             ,[-1,-1, 1, 1,-1]
  --             ,[-1, 1,-1, 1,-1]
  --             ,[-1, 1, 1, 1,-1]
  --             ,[ 1,-1,-1, 1,-1]
  --             ,[ 1,-1, 1, 1,-1]
  --             ,[ 1, 1,-1, 1,-1]
  --             ,[ 1, 1, 1, 1,-1]
  --             ,[-1,-1,-1,-1, 1]
  --             ,[-1,-1, 1,-1, 1]
  --             ,[-1, 1,-1,-1, 1]
  --             ,[-1, 1, 1,-1, 1]
  --             ,[ 1,-1,-1,-1, 1]
  --             ,[ 1,-1, 1,-1, 1]
  --             ,[ 1, 1,-1,-1, 1]
  --             ,[ 1, 1, 1,-1, 1]
  --             ,[-1,-1,-1, 1, 1]
  --             ,[-1,-1, 1, 1, 1]
  --             ,[-1, 1,-1, 1, 1]
  --             ,[-1, 1, 1, 1, 1]
  --             ,[ 1,-1,-1, 1, 1]
  --             ,[ 1,-1, 1, 1, 1]
  --             ,[ 1, 1,-1, 1, 1]
  --             ,[ 1, 1, 1, 1, 1]]
  -- chull <- convexHull cube5 False
  -- pPrint chull
  -- pPrint $ xxx chull
  -- pPrint $ length $ nub $ xxx chull

  -- let square = [[0,0],[0,1],[1,0],[1,1]]
  -- chull <- convexHull square False
  -- pPrint chull
