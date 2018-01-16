setwd("C:/HaskellProjects/delaunayn/rgl")

movie3d(par3dinterp(time = seq(0, 1, len = 5), userMatrix = list(M, rotate3d(M, pi,
  1, 1, 0), rotate3d(M, pi, 0, 1, 1), rotate3d(M, pi, 1, 0, 1), M)), fps = 100,
  duration = 1, dir = ".", convert = "C:/PortableApps/ImageMagick/convert -loop 2 -delay 1x%d %s*.png %s.%s")

movie3d( spin3d(rpm=30, axis=c(1,1,1)), duration = 2, fps=30 , dir=".",
  convert="C:/PortableApps/ImageMagick/convert -loop 0 -delay 1x%d %s*.png %s.%s")
