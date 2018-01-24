module Delaunay2.Delaunay
  where
import           Control.Monad         (when, unless)
import           Delaunay2.CDelaunay
import           Delaunay2.Types
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (free, mallocBytes)
import           Foreign.Marshal.Array (pokeArray)
import           Foreign.Storable      (peek, sizeOf)

delaunay2 :: [[Double]] -> Bool -> IO Tesselation
delaunay2 sites deg = do
  let n     = length sites
      dim   = length (head sites)
      check = all (== dim) (map length (tail sites))
  unless check $
    error "the points must have the same dimension"
  when (dim < 2) $
    error "dimension must be at least 2"
  when (n <= dim+1) $
    error "insufficient number of points"
  sitesPtr <- mallocBytes (n * dim * sizeOf (undefined :: CDouble))
  pokeArray sitesPtr (concatMap (map realToFrac) sites)
  exitcodePtr <- mallocBytes (sizeOf (undefined :: CUInt))
  resultPtr <- c_tesselation sitesPtr
               (fromIntegral dim) (fromIntegral n) (fromIntegral $ fromEnum deg)
               exitcodePtr
  exitcode <- peek exitcodePtr
  free exitcodePtr
  free sitesPtr
  if exitcode /= 0
    then do
      free resultPtr
      error $ "qhull returned an error (code " ++ show exitcode ++ ")"
    else do
      result <- peek resultPtr
      out <- cTesselationToTesselation sites result
      free resultPtr
      return out
