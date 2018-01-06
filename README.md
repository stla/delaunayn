# delaunayn

Delaunay triangulation in arbitrary dimension.

## Example

Consider a polyhedron with these vertices:

```haskell
vertices = [
            [-5,-5,16]  -- 0
          , [-5,8,3]    -- 1
          , [4,-1,3]    -- 2
          , [4,-5,7]    -- 3
          , [4,-1,-10]  -- 4
          , [4,-5,-10]  -- 5
          , [-5,8,-10]  -- 6
          , [-5,-5,-10] -- 7
                       ]
```

The `delaunay` function splits the polyhedron into simplices:

```haskell
> delaunay vertices
([[5,7,4,2], [5,3,7,2], [6,7,1,2], [6,7,4,2], [3,7,1,2], [3,7,1,0]]
,[[3,1],     [4,0],     [4,3],     [0,2],     [2,1,5],   [4]]
,[78.0,      102.0,     253.5,     253.5,     156.0,     507.0])
```

The first component gives the simplices. For example `[5,7,4,2]` corresponds
to the simplex whose vertices are `vertices!!5`, `vertices!!7`, `vertices!!4`
and `vertices!!2`.

The second component gives the neighbours of each simplex. It refers to the
first component.
For example, the first list `[3,1]` in the second component means that the
first simplex has two neighbours: the simplex indexed by `3` and the simplex
indexed by `1`.

The third component gives the volumes of the simplices.
