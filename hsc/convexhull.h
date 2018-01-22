typedef struct Vertex {
  unsigned id;
  double*  point;
} VertexT;

typedef struct FullVertex {
  unsigned  id;
  double*   point;
  unsigned* neighfacets;
  unsigned  nneighfacets;
  unsigned* neighvertices;
  unsigned  nneighsvertices;
  unsigned* neighridges;
  unsigned  nneighridges;
} FullVertexT;

typedef struct Ridge {
    VertexT* vertices;
    unsigned ridgeOf1;
    unsigned ridgeOf2;
    unsigned nvertices;
} RidgeT;

typedef struct Face {
  VertexT*  vertices;
  RidgeT*   ridges;
  unsigned  nridges;
  double*   center;
  double*   normal;
  double    offset;
  double    area;
  unsigned* neighbors;
  unsigned  neighborsize;
  int       family;
} FaceT;

typedef struct ConvexHull {
  unsigned     dim;
  FullVertexT* vertices;
  unsigned     nvertices;
  FaceT*       faces;
  unsigned*    facesizes;
  unsigned     nfaces;
  RidgeT*      ridges;
  unsigned     nridges;
  unsigned**   edges;
  unsigned     nedges;
} ConvexHullT;
