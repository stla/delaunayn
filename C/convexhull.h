typedef struct Vertex {
  unsigned id;
  double* point;
} VertexT;

typedef struct FullVertex {
  unsigned id;
  double* point;
  unsigned* neighfacets;
  unsigned nneighfacets;
} FullVertexT;

typedef struct Edge {
    VertexT v1;
    VertexT v2;
} EdgeT;

typedef struct Face {
  VertexT*  vertices;
  EdgeT*    edges;
  double*   center;
  double*   normal;
  double    offset;
  double    area;
  unsigned* neighbors;
  unsigned  neighborsize;
} FaceT;

typedef struct ConvexHull {
  unsigned     dim;
  FullVertexT* vertices;
  unsigned     nvertices;
  FaceT*       faces;
  unsigned*    facesizes;
  unsigned     nfaces;
  EdgeT*       edges;
  unsigned     nedges;
} ConvexHullT;
