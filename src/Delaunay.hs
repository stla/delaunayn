{-# LANGUAGE ForeignFunctionInterface #-}
module Delaunay
  where
import           Control.Monad         (when, (<$!>))
import           Data.List.Split       (chunksOf)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (peekArray, pokeArray)
import           Foreign.Ptr           (Ptr)
import           Foreign.Storable      (peek, sizeOf)
import           Result
import           System.IO             (readFile)
import           TemporaryFile

foreign import ccall unsafe "delaunay" c_delaunay
  :: Ptr CDouble -> CUInt -> CUInt -> Ptr CUInt -> Ptr CUInt -> CString
  -> IO (Ptr Result)

delaunay :: [[Double]] -> IO ([[Int]], [[Int]], [Double])
delaunay vertices = do
  let n = length vertices
      dim = length (head vertices)
  when (n <= dim+1) $
    error "insufficient number of points"
  verticesPtr <- mallocBytes (n * dim * (sizeOf (undefined :: CDouble)))
  pokeArray verticesPtr (concat (map (map realToFrac) vertices))
  nfPtr <- mallocBytes (sizeOf (undefined :: CUInt))
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  tmpFile <- getTemporaryFile "tmp.txt"
  tmpFile' <- newCString tmpFile
  resultPtr <- c_delaunay
               verticesPtr (fromIntegral dim) (fromIntegral n) nfPtr
               exitcodePtr tmpFile'
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free verticesPtr
  if exitcode /= 0
    then do
      free nfPtr
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      nf <- (<$!>) fromIntegral (peek nfPtr)
      free nfPtr
      result <- peek resultPtr
      indices <- (<$!>) (map fromIntegral)
                        (peekArray (nf * (dim+1)) (_indices result))
      areas <- (<$!>) (map realToFrac) (peekArray nf (_areas result))
      neighbors <- (<$!>) (map fromIntegral)
                          (peekArray (nf * (dim+1)) (_neighbors result))
      let neighbors' = map ((map (subtract 1)).(filter (/=0))) $
                           chunksOf (dim+1) neighbors
      free resultPtr
      (>>=) (readFile tmpFile) putStrLn -- print summary
      return (chunksOf (dim+1) indices, neighbors', areas)

test :: IO ([[Int]], [[Int]], [Double])
test =
  delaunay [[-5,-5,16], [-5,8,3], [4,-1,3], [4,-5,7], [4,-1,-10], [4,-5,-10], [-5,8,-10], [-5,-5,-10]]
-- [[0,0,0], [1,0,0], [1,1,0], [1,1,1], [0,2,0]]

test2 :: IO ([[Int]], [[Int]], [Double])
test2 = delaunay [[-1,-1,-1],[-1,-1,1],[-1,1,-1],[-1,1,1],[1,-1,-1],[1,-1,1],[1,1,-1],[1,1,1],[0,0,0]]
