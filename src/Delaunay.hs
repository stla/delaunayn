{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay
  where
import           Data.List.Split       (chunksOf)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr)
import           Foreign.Storable      (peek, poke, sizeOf)
import           Result
import           System.Directory      (getTemporaryDirectory)

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -> CUInt -> CUInt -> Ptr CUInt -> Ptr CUInt -> CString
  -> IO (Ptr Result)

delaunay :: [[CDouble]] -> IO ([[CUInt]], [[CUInt]], [CDouble])
delaunay vertices = do
  let n = length vertices
      dim = length (head vertices)
  verticesPtr <- mallocBytes (n * dim * (sizeOf (undefined :: CDouble)))
  pokeArray verticesPtr (concat vertices)
  nfPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  tmpDir <- getTemporaryDirectory
  tmpFile <- newCString (tmpDir ++ "/tmp.txt")
  resultPtr <- c_delaunay
               verticesPtr (fromIntegral dim) (fromIntegral n) nfPtr
               exitcodePtr tmpFile
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free verticesPtr
  case exitcode /= 0 of
    True -> do
      free nfPtr
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    False -> do
      nf <- peek nfPtr
      free nfPtr
      result <- peek resultPtr
      indices <- peekArray ((fromIntegral nf) * (dim+1)) (_indices result)
      areas <- peekArray (fromIntegral nf) (_areas result)
      neighbors <- peekArray ((fromIntegral nf) * (dim+1)) (_neighbors result)
      let neighbors' = map ((map (\i -> i-1)).(filter (/=0))) $
                           chunksOf (dim+1) neighbors
      free resultPtr
      return (chunksOf (dim+1) indices, neighbors', areas)

test :: IO ([[CUInt]], [[CUInt]], [CDouble])
test = do
  delaunay [[-5,-5,16], [-5,8,3], [4,-1,3], [4,-5,7], [4,-1,-10], [4,-5,-10], [-5,8,-10], [-5,-5,-10]]
-- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]
