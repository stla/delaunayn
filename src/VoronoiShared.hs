module VoronoiShared
  where

type Index = Int
type Site = [Double]

factor2 :: (Double,Double,Double,Double) -> (Double,Double) -> (Double,Double) -> Double
factor2 box@(xmin, xmax, ymin, ymax) p@(p1,p2) (v1,v2)
  | v1==0 = if v2>0 then (ymax-p2)/v2 else (ymin-p2)/v2
  | v2==0 = if v1>0 then (xmax-p1)/v1 else (xmin-p1)/v1
  | otherwise = min (factor2 box p (v1,0)) (factor2 box p (0,v2))
  --  | v1>0 && v2>0 = min ((r-p1)/v1) ((t-p2)/v2)
  --  | v1>0 && v2<0 = min ((r-p1)/v1) ((b-p2)/v2)
  --  | v1<0 && v2>0 = min ((l-p1)/v1) ((t-p2)/v2)
  --  | v1<0 && v2<0 = min ((l-p1)/v1) ((b-p2)/v2)

approx :: RealFrac a => Int -> a -> a
approx n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
