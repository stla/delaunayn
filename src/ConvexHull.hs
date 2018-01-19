module ConvexHull
  ( module X
  , test
  ) where
import ConvexHull.CConvexHull as X (ConvexHull(..))
import ConvexHull.ConvexHull  as X

test :: IO ConvexHull
test =
  convexHull [[-5,-5, 16], [-5, 8, 3 ], [ 4,-1, 3 ], [ 4,-5, 7], [ 4,-1,-10],
              [ 4,-5,-10], [-5, 8,-10], [-5,-5,-10]] True
-- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]

test2 :: IO ConvexHull
test2 = convexHull [[-1,-1,-1],[-1,-1, 1],[-1, 1,-1],[-1, 1, 1],[ 1,-1,-1],
                    [ 1,-1, 1],[ 1, 1,-1],[ 1, 1, 1],[ 0, 0, 0]] False

test3 :: IO ConvexHull
test3 = convexHull [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2],[2,0],[2,1],[2,2]] False

test4 :: IO ConvexHull
test4 = convexHull [[0,0],[0,2],[2,0],[2,2],[1,1]] False

cuboctahedron :: [[Double]]
cuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
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
