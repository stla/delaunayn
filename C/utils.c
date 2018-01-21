// to use the qsort function
int cmpfunc (const void * a, const void * b) {
   return ( *(int*)a - *(int*)b );
}

int cmpfuncdbl (const void * a, const void * b) {
   return ( *(double*)a - *(double*)b > 0 ? 1 : -1);
}

double square(double x){
  return x*x;
}
