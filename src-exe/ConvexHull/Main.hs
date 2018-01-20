module Main
  where
import ConvexHull
import Text.Show.Pretty

main :: IO ()
main = do
  -- chull <- test
  -- pPrint chull
  -- let cube = [[-1,-1,-1]
  --            ,[-1,-1, 1]
  --            ,[-1, 1,-1]
  --            ,[-1, 1, 1]
  --            ,[ 1,-1,-1]
  --            ,[ 1,-1, 1]
  --            ,[ 1, 1,-1]
  --            ,[ 1, 1, 1]]
  -- chull <- convexHull cube True
  -- pPrint chull

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
  -- chull <- convexHull hcube True
  -- putStrLn "done"

  let square = [[0,0],[0,1],[1,0],[1,1]]
  chull <- convexHull square True
  pPrint chull
