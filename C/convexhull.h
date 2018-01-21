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
  unsigned* neighedges;
  unsigned  nneighedges;
} FullVertexT;

typedef struct Ridge {
    VertexT* vertices;
    unsigned ridgeOf1;
    unsigned ridgeOf2;
} RidgeT;

typedef struct Face {
  VertexT*  vertices;
  RidgeT*   edges;
  unsigned  nedges;
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
  RidgeT*      edges;
  unsigned     nedges;
} ConvexHullT;
