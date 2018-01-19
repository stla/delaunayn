#define qh_QHimport
#include "qhull_ra.h"
#include "convexhull.h"
#include "utils.h"

ConvexHullT* convexHull(
	double*   points,
	unsigned  dim,
	unsigned  n,
  unsigned  triangulate,
	unsigned* exitcode
)
{
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull s FF %s", triangulate ? "Qt" : "");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK
  qh_meminit(qh, stderr);
	boolT ismalloc  = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile   = NULL;
  FILE* tmpstdout = stdout;
  qh_zero(qh, errfile);
	exitcode[0] = qh_new_qhull(qh, dim, n, points, ismalloc, flags, tmpstdout,
		                         errfile);
  //fclose(tmpstdout);
  printf("exitcode: %d\n", exitcode[0]);

  ConvexHullT* out = malloc(sizeof(ConvexHullT));

	if (!exitcode[0]) {             /* 0 if no error from qhull */
    // FILE* summaryFile = fopen(tmpFile, "w");
  	// qh_printsummary(qh, summaryFile);
  	// fclose(summaryFile);
  	qh_getarea(qh, qh->facet_list);

    unsigned   nfaces    = qh->num_facets;
    unsigned** faces     = malloc(nfaces * sizeof(unsigned*));
    unsigned*  facesizes = malloc(nfaces * sizeof(unsigned));
    double*    areas     = malloc(nfaces * sizeof(double));
    double**   centers   = malloc(nfaces * sizeof(double*));
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        areas[i_facet]     = facet->f.area;
        centers[i_facet]   = facet->center;
        facesizes[i_facet] =
          (unsigned) qh_setsize(qh, facet->vertices);
        faces[i_facet]     =
          (unsigned*) malloc(facesizes[i_facet] * sizeof(unsigned));
        { // vertices
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          unsigned facetvertices[facesizes[i_facet]];
          FOREACHvertex_(facet->vertices){
            facetvertices[i_vertex] = qh_pointid(qh, vertex->point);
            i_vertex++;
          }
          qsort(facetvertices, facesizes[i_facet], sizeof(unsigned), cmpfunc);
          for(i_vertex=0; i_vertex<facesizes[i_facet]; i_vertex++){
            faces[i_facet][i_vertex] = facetvertices[i_vertex];
          }
          i_facet++;
        }
        { // neighbours
          facet->id = i_facet;
        }
      }
    }

    unsigned  nvertices = qh->num_vertices;
    unsigned* vertices  = malloc(nvertices * sizeof(unsigned));
    {
      vertexT *vertex;
      unsigned i_vertex=0;
      unsigned vertices_[nvertices];
      FORALLvertices{
        vertices_[i_vertex] = qh_pointid(qh, vertex->point);
        i_vertex++;
      }
      qsort(vertices_, nvertices, sizeof(unsigned), cmpfunc);
      vertices = vertices_;
    }

    free(areas);
    free(centers)
    out->dim       = dim;
    out->vertices  = vertices;
    out->nvertices = nvertices;
    out->faces     = faces;
    out->facesizes = facesizes;
    out->nfaces    = nfaces;
  } // end if exitocde

  return out;
}
