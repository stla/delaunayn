setwd("C:/HaskellProjects/delaunayn/rgl")
library(rgl)

movie3d(
  par3dinterp(
    time = seq(0, 1, len = 5),
    userMatrix = list(
      M,
      rotate3d(M, pi, 1, 1, 0),
      rotate3d(M, pi, 0, 1, 1),
      rotate3d(M, pi, 1, 0, 1),
      M
    )
  ),
  fps = 100,
  duration = 1,
  dir = ".",
  convert = "C:/PortableApps/ImageMagick/convert -loop 2 -delay 1x%d %s*.png %s.%s"
)

segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0), c(4.0, -5.0, -10.0)),
            color = topo.colors(1, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, -10.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -1.0, -10.0), c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(1, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -1.0, -10.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0), c(4.0, -5.0, -10.0)),
            color = topo.colors(2, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -5.0, 7.0), c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(2, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -5.0, 7.0), c(4.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")
triangles3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0), c(-5.0, 8.0, -10.0)),
            color = topo.colors(3, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(-5.0, 8.0, 3.0), c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(3, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, 8.0, 3.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0), c(-5.0, 8.0, -10.0)),
            color = topo.colors(4, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, -10.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
triangles3d(rbind(c(4.0, -1.0, -10.0), c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(4, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(4.0, -1.0, -10.0), c(-5.0, 8.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, -10.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")
triangles3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0)),
            color = topo.colors(5, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, 8.0, 3.0)), color = "black")
segments3d(rbind(c(4.0, -1.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(4.0, -1.0, 3.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")
triangles3d(rbind(c(-5.0, -5.0, 16.0), c(-5.0, 8.0, 3.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(6, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, -5.0, 16.0), c(-5.0, 8.0, 3.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, -5.0, 16.0)), color = "black")
triangles3d(rbind(c(-5.0, -5.0, 16.0), c(-5.0, 8.0, 3.0), c(4.0, -5.0, 7.0)),
            color = topo.colors(6, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, -5.0, 16.0), c(-5.0, 8.0, 3.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, 16.0)), color = "black")
triangles3d(rbind(c(-5.0, -5.0, 16.0), c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)),
            color = topo.colors(6, alpha = 0.5),
            alpha = 0.9)
segments3d(rbind(c(-5.0, -5.0, 16.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, -5.0, 16.0)), color = "black")
segments3d(rbind(c(-5.0, 8.0, 3.0), c(4.0, -5.0, 7.0)), color = "black")
segments3d(rbind(c(4.0, -5.0, 7.0), c(-5.0, -5.0, -10.0)), color = "black")
segments3d(rbind(c(-5.0, -5.0, -10.0), c(-5.0, 8.0, 3.0)), color = "black")


movie3d(
  par3dinterp(
    time = seq(0, 1, len = 5),
    userMatrix = list(
      M,
      rotate3d(M, pi, 1, 1, 0),
      rotate3d(M, pi, 0, 1, 1),
      rotate3d(M, pi, 1, 0, 1),
      M
    )
  ),
  fps = 100,
  duration = 1,
  dir = ".",
  movie = "rgg",
  convert = "C:/PortableApps/ImageMagick/convert -loop 0 -delay 1x%d %s*.png %s.%s"
)
