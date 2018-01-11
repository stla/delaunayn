typedef struct Result {
  unsigned dim;
  unsigned length;
  unsigned* indices;
  double* areas;
  unsigned* neighbors;
  double* centers;
  unsigned* toporient;
} ResultT;
