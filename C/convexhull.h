typedef struct Vertex {
  unsigned id;
  double* point;
} VertexT;

typedef struct Edge {
    VertexT v1;
    VertexT v2;
} EdgeT;

typedef struct Face {
  VertexT*  vertices;
  EdgeT*    edges;
  double*   center;
  double*   normal;
  double    area;
  unsigned* neighbors;
  unsigned  neighborsize;
} FaceT;

typedef struct ConvexHull {
  unsigned   dim;
  VertexT*   vertices;
  unsigned   nvertices;
  FaceT*     faces;
  unsigned*  facesizes;
  unsigned   nfaces;
} ConvexHullT;
