module Main
  where
import           Data.Map.Strict  (elems)
import           Delaunay
import           Linear
import           System.IO
import           Text.Show.Pretty
import           Vis

delaunay3vis :: Delaunay -> VisObject Double
delaunay3vis tess = VisObjects $ concat $ map visRidge (elems $ allRidges tess)
  where
    visRidge ridge =
      [ Triangle (pts!!0) (pts!!1) (pts!!2) (makeColor 0 0 1 0.5)
      , Line Nothing ((pts!!0) : pts) black ]
      where
        pts = map (\p -> V3 (p!!0) (p!!1) (p!!2)) (_points $ _rsimplex ridge)

main :: IO ()
main = do
  tess <- delaunay rhombicDodecahedron
  putStrLn $ ppShow tess
  writeFile "rhombicDodecahedron.txt" (delaunay3rgl tess)
  display (defaultOpts {optWindowName = "rhombic dodecahedron"})
          (delaunay3vis tess)
