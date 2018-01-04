#define qh_QHimport
#include "qhull_ra.h"
#include "result.h"

struct Result* delaunay(
	double* vertices,
	unsigned dim,
	unsigned n,
	unsigned* nf,
	unsigned* exitcode,
	char* tmpfile
)
{
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull d Qt", "");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK
	boolT ismalloc = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile = NULL;
	int curlong, totlong;
  unsigned* indices;
	double* areas;

  FILE* tmpstdout = fopen(tmpfile, "w");
	exitcode[0] = qh_new_qhull(qh, dim, n, vertices, ismalloc, flags, tmpstdout, errfile);
  fclose(tmpstdout);
	qh_getarea(qh, qh->facet_list);
	if (!exitcode[0]) {                    /* 0 if no error from qhull */
		facetT *facet;                  /* set by FORALLfacets */
		vertexT *vertex, **vertexp;
//    facetT *neighbor, **neighborp;
    /* Count the number of facets so we know how much space to allocate */
		nf[0]=0;                 /* Number of facets */
		FORALLfacets {
			if (!facet->upperdelaunay && facet->simplicial && !facet->degenerate) {
        nf[0]++;
      }
		}
    /* Alocate the space */
    indices = (unsigned*) malloc(nf[0] * (dim+1) * sizeof(unsigned));
		areas = (double*) malloc(nf[0] * sizeof(double));
    /* Iterate through facets to extract information */
    int i=0;
    FORALLfacets {
		  if (!facet->upperdelaunay && facet->simplicial && !facet->degenerate) {
				areas[i] = facet->f.area;
	      int j=0;
	      FOREACHvertex_ (facet->vertices) {
          indices[i*(dim+1)+j] = qh_pointid(qh, vertex->point);
          j++;
				}
        /* Neighbours */
      //   PROTECT(neighbour = allocVector(INTSXP, qh_setsize(facet->neighbors)));
      //   j=0;
      //   FOREACHneighbor_(facet) {
      //     INTEGER(neighbour)[j] = neighbor->visitid ? neighbor->visitid: 0 - neighbor->id;
      //     j++;
      //   }
      //   SET_VECTOR_ELT(neighbours, i, neighbour);
      //   UNPROTECT(1);
				i++;
			}
		}
	}
  /* Do cleanup regardless of whether there is an error */
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort (qh, &curlong, &totlong);   /* free short memory and memory allocator */
  // if (exitcode) {
	// 	error("Received error code %d from qhull.", exitcode);
	// }
	struct Result* out = malloc (sizeof(ResultT));
	if(!exitcode[0]){
	  out->dim = dim;
	  out->length = nf[0];
	  out->indices = indices;
		out->areas = areas;
	}
  return out;
}
