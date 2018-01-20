# delaunayn

Delaunay triangulation and Voronoi diagrams in arbitrary dimension.
Based on the `qhull` C library.

## Delaunay tesselation

Consider this list of vertices (actually these are the vertices of a
polyhedron):

```haskell
vertices = [
            [ -5, -5,  16 ]  -- 0
          , [ -5,  8,   3 ]  -- 1
          , [  4, -1,   3 ]  -- 2
          , [  4, -5,   7 ]  -- 3
          , [  4, -1, -10 ]  -- 4
          , [  4, -5, -10 ]  -- 5
          , [ -5,  8, -10 ]  -- 6
          , [ -5, -5, -10 ]  -- 7
                           ]
```

The `delaunay` function splits the polyhedron into simplices (facets):

```haskell
> d <- delaunay vertices False
> _facets d
fromList
  [ ( 0
    , Facet
        { _simplex =
            Simplex
              { _points =
                  fromList
                    [ ( 2 , [ 4.0 , -1.0 , 3.0 ] )
                    , ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                    , ( 5 , [ 4.0 , -5.0 , -10.0 ] )
                    , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                    ]
              , _circumcenter =
                  [ -0.5000000000000009 , -3.0 , -3.499999999999999 ]
              , _normal =
                  [ -7.03638769136053e-2
                  , -0.4221832614816321
                  , -0.4925471383952373
                  , -0.7577648283003651
                  ]
              , _volume = 78.0
              }
        , _neighbours = fromList [ 1 , 3 ]
        }
    )
  , ( 1
    , Facet
    ............
```

The field `_facets` is a map of `Facet` objects. The keys of the map are
the facets identifiers. A `Facet` object has two fields: `_simplex` and
`_neighbours`. The field `_simplex` is a `Simplex` object, and the field
`_neighbours` is a set of facets identifiers, the neighbours of the facet.

A `Simplex` object has four fields:

-   `_points`, the vertices of the polytope, actually a map of the vertices
identifiers to their coordinates

-   `_circumcenter`, the coordinates of the circumcenter of the simplex;

-   `_normal`, the coordinates of the normal of the simplex;

-   `_volume`, the volume of the simplex (the area in dimension 2, the
  length in dimension 1).

The second field of the output of `delaunay` is `_ridges`:

```haskell
> _ridges d
[ Ridge
    { _subsimplex =
        Simplex
          { _points =
              fromList
                [ ( 2 , [ 4.0 , -1.0 , 3.0 ] )
                , ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                ]
          , _circumcenter =
              [ -0.5000000000000007 , -3.000000000000001 , -3.499999999999999 ]
          , _normal = [ -0.4061384660534476 , 0.9138115486202572 , 0.0 ]
          , _volume = 64.01757571167468
          }
    , _ridgeOf = fromList [ 0 , 3 ]
    }
, Ridge
  ................
```

This is a list of `Ridge` objects. A ridge is a subsimplex, a component of a
facet. A `Ridge` object has two fields: `_subsimplex`, the simplex, and
`_ridgeOf`, the identifiers of the facets the ridge belongs to (a set of one
or two integers).

The third field of the output of `delaunay` is `_sites`, the vertices with
additional information:

```haskell
> _sites d
fromList
  [ ( 0
    , Site
        { _coordinates = [ -5.0 , -5.0 , 16.0 ]
        , _neighSites = fromList [ 1 , 3 , 7 ]
        , _neighRidges =
            fromList
              [ fromList [ 0 , 1 , 3 ]
              , fromList [ 0 , 1 , 7 ]
              , fromList [ 0 , 3 , 7 ]
              ]
        , _neighFacets = fromList [ 5 ]
        }
    )
  , ( 1
    , Site
    ...................
```

This is a map of `Site` objects. The keys of the map are the identifiers of
the vertices. A `Site` object has four fields:

-   `_coordinates`, the coordinates of the vertex;

-   `_neighSites`, the identifiers of the connected vertices;

-   `_neighRidges`, a set of sets of integers, the set of the ridges the
vertex belongs to, each ridge represented by the set of its vertices
identifiers;

-   `_neighFacets`, the set of the identifiers of the facets the vertex belongs
to.

[![rgg.gif](https://s13.postimg.org/6h1r72k3r/rgg.gif)](https://postimg.org/image/ojutyafyb/)


## Voronoi diagrams

The library allows to get the Voronoi diagram of a list of sites (vertices)
from the Delaunay tesselation. Here is a 3D example.

```haskell
centricCuboctahedron :: [[Double]]
centricCuboctahedron = [[i,j,0] | i <- [-1,1], j <- [-1,1]] ++
                       [[i,0,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,i,j] | i <- [-1,1], j <- [-1,1]] ++
                       [[0,0,0]]
import Delaunay
import Voronoi3D
d <- delaunay centricCuboctahedron False
v = voronoi3 d
```

The output of `voronoi3` is a list of Voronoi cells given as pairs, each pair
consisting of a site and a list of edges.
This is the cell of the center `[0,0,0]`:

```haskell
> last v
( [ 0.0 , 0.0 , 0.0 ]
, [ Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , 0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 0.0 , 0.0 , 1.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , 0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , 0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , -0.5 , -0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 0.0 , 0.0 , -1.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 0.0 , 1.0 , 0.0 ) )
  , Edge3 ( ( 0.5 , 0.5 , -0.5 ) , ( 1.0 , 0.0 , 0.0 ) )
  ]
)
```

This is a bounded cell: it has finite edges only. The other ones are not
bounded, they have infinite edges:

```haskell
> head v
( [ -1.0 , -1.0 , 0.0 ]
, [ Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , 0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , IEdge3
      ( ( -0.5 , -0.5 , 0.5 )
      , ( -0.5773502691896258
        , -0.5773502691896258
        , 0.5773502691896258
        )
      )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( 0.0 , -1.0 , 0.0 ) )
  , Edge3 ( ( -0.5 , -0.5 , -0.5 ) , ( -1.0 , 0.0 , 0.0 ) )
  , IEdge3
      ( ( -0.5 , -0.5 , -0.5 )
      , ( -0.5773502691896258
        , -0.5773502691896258
        , -0.5773502691896258
        )
      )
  , IEdge3 ( ( -1.0 , 0.0 , 0.0 ) , ( 1.0 , 0.0 , 0.0 ) )
  , IEdge3 ( ( 0.0 , -1.0 , 0.0 ) , ( 0.0 , -1.0 , 0.0 ) )
  ]
)
```


[![voronoi_cuboctahedron.gif](https://s13.postimg.org/6e0vngu1j/voronoi_cuboctahedron.gif)](https://postimg.org/image/ceykkjgnn/)
