# tess <- delaunay cuboctahedron False
# let v = voronoi3 tess
#     code1 = voronoi3ForRgl v Nothing
#     bigcell = snd (last v)
#     verts = cell3Vertices bigcell
# tess2 <- delaunay verts True
# putStrLn $ ppShow $ _facets tess2
# putStrLn $ ppShow $ IM.map (_volume . _simplex) (_facets tess2)
# let code2 = delaunay3rgl tess2 False False False (Just 0.5)
# writeFile "rgl/voronoi_cuboctahedron01.R" (code1 ++ code2)

spheres3d(-1.0,-1.0,0.0, radius=0.1, color="red")
segments3d(rbind(c(-1.0,0.0,0.0), c(-2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(0.0,-1.0,0.0), c(0.0,-2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,-1.0,0.0)))

spheres3d(-1.0,1.0,0.0, radius=0.1, color="red")
segments3d(rbind(c(-1.0,0.0,0.0), c(-2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(0.0,1.0,0.0), c(0.0,2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,1.0,0.0)))

spheres3d(1.0,-1.0,0.0, radius=0.1, color="red")
segments3d(rbind(c(1.0,0.0,0.0), c(2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.0,-1.0,0.0), c(0.0,-2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,-1.0,0.0)))

spheres3d(1.0,1.0,0.0, radius=0.1, color="red")
segments3d(rbind(c(1.0,0.0,0.0), c(2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.0,1.0,0.0), c(0.0,2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,1.0,0.0)))

spheres3d(-1.0,0.0,-1.0, radius=0.1, color="red")
segments3d(rbind(c(-1.0,0.0,0.0), c(-2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-1.0), c(0.0,0.0,-2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,0.0,-1.0)))

spheres3d(-1.0,0.0,1.0, radius=0.1, color="red")
segments3d(rbind(c(-1.0,0.0,0.0), c(-2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,1.0), c(0.0,0.0,2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,0.0,1.0)))

spheres3d(1.0,0.0,-1.0, radius=0.1, color="red")
segments3d(rbind(c(1.0,0.0,0.0), c(2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,-1.0), c(0.0,0.0,-2.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,0.0,-1.0)))

spheres3d(1.0,0.0,1.0, radius=0.1, color="red")
segments3d(rbind(c(1.0,0.0,0.0), c(2.0,0.0,0.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.0,0.0,1.0), c(0.0,0.0,2.0)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,0.0,1.0)))

spheres3d(0.0,-1.0,-1.0, radius=0.1, color="red")
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.0,-1.0,0.0), c(0.0,-2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0773502691896257,-1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.0,0.0,-1.0), c(0.0,0.0,-2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))

spheres3d(0.0,-1.0,1.0, radius=0.1, color="red")
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.0,-1.0,0.0), c(0.0,-2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0773502691896257,-1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.0,0.0,1.0), c(0.0,0.0,2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,0.0,1.0)))

spheres3d(0.0,1.0,-1.0, radius=0.1, color="red")
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.0,1.0,0.0), c(0.0,2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0773502691896257,1.0773502691896257,-1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.0,0.0,-1.0), c(0.0,0.0,-2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,0.0,-1.0)))

spheres3d(0.0,1.0,1.0, radius=0.1, color="red")
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.0,1.0,0.0), c(0.0,2.0,0.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0773502691896257,1.0773502691896257,1.0773502691896257)), col=c("red","red"))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.0,0.0,1.0), c(0.0,0.0,2.0)), col=c("red","red"))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,0.0,1.0)))

spheres3d(0.0,0.0,0.0, radius=0.1, color="red")
segments3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(-0.5,0.5,-0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(-1.0,0.0,0.0)))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.5,-0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,-1.0,0.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(1.0,0.0,0.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,1.0,0.0)))
segments3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(-0.5,0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(-0.5,-0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(-0.5,0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(0.5,-0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(0.5,0.5,-0.5), c(0.0,0.0,-1.0)))
segments3d(rbind(c(0.5,-0.5,0.5), c(0.0,0.0,1.0)))
segments3d(rbind(c(0.5,0.5,0.5), c(0.0,0.0,1.0)))

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0,0.0,0.0), c(-0.5,-0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(-1.0,0.0,0.0), c(-0.5,0.5,-0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-1.0,0.0,0.0), c(-0.5,-0.5,0.5), c(-0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-1.0,0.0,0.0), c(-0.5,0.5,-0.5), c(-0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,0.5,-0.5), c(0.5,0.5,-0.5), c(0.0,0.0,-1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.5,-0.5,-0.5), c(0.5,0.5,-0.5), c(0.0,0.0,-1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(-0.5,0.5,-0.5), c(0.0,0.0,-1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(0.5,-0.5,-0.5), c(0.0,0.0,-1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(-0.5,-0.5,0.5), c(0.0,-1.0,0.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,0.5), c(0.0,-1.0,0.0), c(0.5,-0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,-0.5), c(0.0,-1.0,0.0), c(0.5,-0.5,-0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.0,-1.0,0.0), c(0.5,-0.5,-0.5), c(0.5,-0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,0.5,-0.5), c(0.0,1.0,0.0), c(0.5,0.5,-0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.0,1.0,0.0), c(0.5,0.5,-0.5), c(0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,0.5,-0.5), c(-0.5,0.5,0.5), c(0.0,1.0,0.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,0.5,0.5), c(0.0,1.0,0.0), c(0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.5,-0.5,-0.5), c(1.0,0.0,0.0), c(0.5,0.5,-0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(1.0,0.0,0.0), c(0.5,0.5,-0.5), c(0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.5,-0.5,-0.5), c(1.0,0.0,0.0), c(0.5,-0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(1.0,0.0,0.0), c(0.5,-0.5,0.5), c(0.5,0.5,0.5)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,0.5), c(-0.5,0.5,0.5), c(0.0,0.0,1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,0.5,0.5), c(0.5,0.5,0.5), c(0.0,0.0,1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(-0.5,-0.5,0.5), c(0.5,-0.5,0.5), c(0.0,0.0,1.0)), color="blue", alpha=0.5)

triangles3d(rbind(c(0.5,-0.5,0.5), c(0.5,0.5,0.5), c(0.0,0.0,1.0)), color="blue", alpha=0.5)
