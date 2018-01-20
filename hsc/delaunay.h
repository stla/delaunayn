typedef struct Delaunay {
  unsigned dim;      // to remove
  unsigned nfaces;   
  unsigned* indices; // to rename to vertices
  double* fvolumes;
  unsigned* owners;
  unsigned* neighbors;
  double* centers;
  unsigned* toporient;
  unsigned* ridges;
  unsigned nridges;
  double* rvolumes;
  double* rcenters;
  double* rnormals;
  double* fnormals;
  double* rdistances;
  unsigned* vrneighbors;
  unsigned* vrnsizes;
  unsigned* vfneighbors;
  unsigned* vfnsizes;
  unsigned* vvneighbors;
  unsigned* vvnsizes;
} DelaunayT;
