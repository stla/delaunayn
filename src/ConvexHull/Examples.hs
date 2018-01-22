module ConvexHull.Examples
  where
import           Data.List.Split (chunksOf)
import           System.Random

rgg :: [[Double]]
rgg =[[-5,-5, 16], [-5, 8, 3 ], [ 4,-1, 3 ], [ 4,-5, 7], [ 4,-1,-10],
      [ 4,-5,-10], [-5, 8,-10], [-5,-5,-10]]

centricCube :: [[Double]]
centricCube =  [[-1,-1,-1],[-1,-1, 1],[-1, 1,-1],[-1, 1, 1],[ 1,-1,-1],
                [ 1,-1, 1],[ 1, 1,-1],[ 1, 1, 1],[ 0, 0, 0]]

squareLattice :: [[Double]]
squareLattice = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]]

centricSquare :: [[Double]]
centricSquare = [[0,0],[0,2],[2,0],[2,2],[1,1]]

centriCuboctahedron :: [[Double]]
centriCuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                      [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                      [[0,i,j] | i <- [-1,1], j <- [-1,1]] ++
                      [[0,0,0]]

rhombicDodecahedron :: [[Double]]
rhombicDodecahedron = [[-1.0, 0.0, 0.0], [-0.5,-0.5,-0.5], [-0.5,-0.5, 0.5],
                       [ 0.0,-1.0, 0.0], [-0.5, 0.5,-0.5], [-0.5, 0.5, 0.5],
                       [ 0.0, 1.0, 0.0], [ 1.0, 0.0, 0.0], [ 0.5,-0.5,-0.5],
                       [ 0.5,-0.5, 0.5], [ 0.5, 0.5,-0.5], [ 0.5, 0.5, 0.5],
                       [ 0.0, 0.0,-1.0], [ 0.0, 0.0, 1.0]]

faceCenteredCubic :: [[Double]]
faceCenteredCubic = [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1]
                    ,[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1]
                    ,[1,0,0],[-1,0,0]
                    ,[0,1,0],[0,-1,0]
                    ,[0,0,1],[0,0,-1]]

cube3 :: [[Double]]
cube3 = [[i,j,k] | i <- [-1,1], j <- [-1,1], k <- [-1,1]]

cube4 :: [[Double]]
cube4 = [[i,j,k,l] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1]]

cube5 :: [[Double]]
cube5 = [[i,j,k,l,m] | i <- [-1,1], j <- [-1,1], k <- [-1,1], l <- [-1,1],
                       m <- [-1,1]]

randomInSphere :: Int -> IO [[Double]]
randomInSphere n = do
  g <- newStdGen
  let theta = map (*(2*pi)) (take n (randoms g :: [Double]))
  let phi   = map (*(2*pi)) (take n (randoms g :: [Double]))
  let rho   = take n (randoms g :: [Double])
  return $ zipWith3 (\r a b -> [r * sin a * cos b,
                                r * sin a * sin b,
                                r * cos a         ])
                     rho phi theta

randomInCube :: Int -> IO [[Double]]
randomInCube n = do
  g <- newStdGen
  return $ chunksOf 3 (take (3*n) (randoms g :: [Double]))
