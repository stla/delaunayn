module ConvexHull.ConvexHull
  where
import           ConvexHull.CConvexHull
import           Control.Monad         (when)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)


convexHull :: [[Double]] -> Bool -> IO ConvexHull
convexHull points triangulate = do
  let n = length points
      dim = length (head points) -- TODO check même longueur
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  pointsPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray pointsPtr (concatMap (map realToFrac) points)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_convexhull pointsPtr
               (fromIntegral dim) (fromIntegral n)
               (fromIntegral $ fromEnum triangulate) exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free pointsPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- peekConvexHull resultPtr
      free resultPtr
      return result


test :: IO ConvexHull
test =
  convexHull [[-5,-5, 16], [-5, 8, 3 ], [ 4,-1, 3 ], [ 4,-5, 7], [ 4,-1,-10],
            [ 4,-5,-10], [-5, 8,-10], [-5,-5,-10]] False
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
