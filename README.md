# delaunayn

Delaunay triangulation in arbitrary dimension.

## Example

Consider a polyhedron with these vertices:

```haskell
vertices = [
            [ -5,-5, 16 ]  -- 0
          , [ -5, 8, 3 ]    -- 1
          , [ 4, -1, 3 ]    -- 2
          , [ 4, -5, 7 ]    -- 3
          , [ 4, -1, -10 ]  -- 4
          , [ 4, -5, -10 ]  -- 5
          , [ -5, 8, -10 ]  -- 6
          , [ -5, -5, -10 ] -- 7
                           ]
```

The `delaunay` function splits the polyhedron into simplices (facets):

```haskell
> d <- delaunay vertices
> _facets d
fromList
  [ ( 0
    , Facet
        { _simplex =
            Polytope
              { _points =
                  fromList
                    [ ( 2 , [ 4.0 , -1.0 , 3.0 ] )
                    , ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                    , ( 5 , [ 4.0 , -5.0 , -10.0 ] )
                    , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                    ]
              , _center = [ -0.5000000000000009 , -3.0 , -3.499999999999999 ]
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
`_neighbours`. The field `_simplex` is a `Polytope` object, and the field
`_neighbours` is a set of facets identifiers, the neighbours of the facet.

A `Polytope` object has four fields:

-   `_points`, the vertices of the polytope, actually a map of the vertices
identifiers to their coordinates

-   `_center`, the coordinates of the center of the polytope;

-   `_normal`, the coordinates of the normal of the polytope;

-   `_volume`, the volume of the polytope (the area in dimension 2, the
  length in dimension 1).

The second field of the output of `delaunay` is `_ridges`:

```haskell
> _ridges d
[ Ridge
    { _polytope =
        Polytope
          { _points =
              fromList
                [ ( 2 , [ 4.0 , -1.0 , 3.0 ] )
                , ( 4 , [ 4.0 , -1.0 , -10.0 ] )
                , ( 7 , [ -5.0 , -5.0 , -10.0 ] )
                ]
          , _center =
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
facet. A `Ridge` object has two fields: `_polytope`, the subsimplex, and
`_ridgeOf`, the identifiers of the facets the ridge belongs to (a set of one
or two integers).

The third field of the output of `delaunay` is `_vertices`:

```haskell
> _vertices d
fromList
  [ ( 0
    , Vertex
        { _coordinates = [ -5.0 , -5.0 , 16.0 ]
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
    , Vertex
    ...................
```

This is a map of `Vertex` objects. The keys of the map are the identifiers of
the vertices. A `Vertex` object has three fields:

-   `_coordinates`, the coordinates of the vertex;

-   `_neighRidges`, a set of sets of integers, the set of the ridges the
vertex belongs to, each ridge represented by the set of its vertices
identifiers;

-   `_neighFacets`, the set of the identifiers of the facets the vertex belongs
to.

[![movie.gif](https://s13.postimg.org/agcabu3p3/movie.gif)](https://postimg.org/image/oms172ek3/)
