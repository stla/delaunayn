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
  sprintf(flags, "qhull d Qt Fn Qbb", "");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK
	boolT ismalloc = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile = NULL;
	int curlong, totlong;
  unsigned* indices;
	double* areas;
//	int* sizneighbors;
	unsigned* neighbors;

  FILE* tmpstdout = fopen(tmpfile, "w");
	exitcode[0] = qh_new_qhull(qh, dim, n, vertices, ismalloc, flags, tmpstdout, errfile);
  fclose(tmpstdout);
	qh_getarea(qh, qh->facet_list);
	if (!exitcode[0]) {                    /* 0 if no error from qhull */
		facetT *facet;                  /* set by FORALLfacets */
		vertexT *vertex, **vertexp;
    facetT *neighbor, **neighborp;
    /* Count the number of facets so we know how much space to allocate */
		nf[0]=0; /* Number of facets */
		// int* k = malloc(1+sizeof(int)*qh->num_facets);
		// k[0] = 0;
		// int l =0;
		// int m =0;
		FORALLfacets {
			//printf("visitid: %d - id: %d\n", facet->visitid, facet->id);
			if (!facet->upperdelaunay && facet->simplicial && !facet->degenerate) {
        nf[0]++;
				facet->id = nf[0];
//				facet->id = l>0 && facet->id > 0 && facet->id <=m && k[facet->id-1]>0 ? facet->id - k[facet->id-1] : facet->id;
      }else{
				qh_removefacet(qh, facet);
//				l++;
				// il faut réindexer les neighbours !
			}
			// m++;
			// k[m] = l;
			// printf("%d", l);
		}

//		qh->facet_list = qh->newfacet_list;
		// faire une nouvelle liste de facettes pour que les neighbours soient ok
    /* Alocate the space */
    indices = (unsigned*) malloc(nf[0] * (dim+1) * sizeof(unsigned));
		areas = (double*) malloc(nf[0] * sizeof(double));
		//sizneighbors = (int*) malloc(nf[0] * sizeof(int));
		neighbors = (unsigned*) malloc(nf[0] * (dim+1) * sizeof(unsigned));
    /* Iterate through facets to extract information */
    int i=0;
    FORALLfacets {
//			printf("facet id: %d\n", facet->id);
			areas[i] = facet->f.area;
			//sizneighbors[i] = qh_setsize(qh, facet->neighbors);
			//printf("%d\n", sizneighbors[i]); // toujours dim+1
      int j=0;
      FOREACHvertex_(facet->vertices) {
        indices[i*(dim+1)+j] = qh_pointid(qh, vertex->point);
        j++;
			}
			j=0;
			FOREACHneighbor_(facet) {
				//printf("visitid: %d - id: %d\n", neighbor->visitid, neighbor->id); // ? neighbor->visitid: 0 - neighbor->id));
				neighbors[i*(dim+1)+j] =
					neighbor->visitid == 0 || neighbor->id > nf[0] ?
					(unsigned)(0) : (unsigned)(neighbor->id);
				j++;
			}
			i++;
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
		out->neighbors = neighbors;
	}
  return out;
}
