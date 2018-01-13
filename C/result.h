typedef struct Result {
  unsigned dim;      // to remove
  unsigned length;   // to remove
  unsigned* indices; // to rename to vertices
  double* areas;
  unsigned* neighbors;
  double* centers;
  unsigned* toporient;
  unsigned* ridges;
  double* rcenters;
  double* rnormals;
  double* fnormals;
  double* rdistances;
  unsigned* vrneighbors;
  unsigned* vrnsizes;
  unsigned* vfneighbors;
  unsigned* vfnsizes;
} ResultT;
