typedef struct ConvexHull {
  unsigned   dim;
  unsigned*  vertices;
  unsigned   nvertices;
  unsigned** faces;
  unsigned*  facesizes;
  unsigned   nfaces;
} ConvexHullT;
