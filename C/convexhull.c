#define qh_QHimport
#include "qhull_ra.h"
#include "convexhull.h"
#include "utils.h"

// to use the qsort function
int cmpvertices (const void * a, const void * b) {
   return ( (*((VertexT*)a)).id - (*((VertexT*)b)).id );
}

double* getpoint(double* points, unsigned dim, unsigned id){
  double* out = malloc(dim * sizeof(double));
  for(unsigned i=0; i<dim; i++){
    out[i] = points[id*dim+i];
  }
  return out;
}

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
    FaceT*     faces     = malloc(nfaces * sizeof(FaceT));
    unsigned*  facesizes = malloc(nfaces * sizeof(unsigned));
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        facet->id              = i_facet; // for neighbors
        faces[i_facet].area    = facet->f.area;
        faces[i_facet].center  = facet->center;
        faces[i_facet].normal  = facet->normal;
        facesizes[i_facet] =
          (unsigned) qh_setsize(qh, facet->vertices);
        faces[i_facet].vertices =
          (VertexT*) malloc(facesizes[i_facet] * sizeof(VertexT));
        { // facet vertices
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices){
            (faces[i_facet].vertices)[i_vertex].id =
              (unsigned) qh_pointid(qh, vertex->point);
            (faces[i_facet].vertices)[i_vertex].point =
              malloc(dim * sizeof(double));
            for(unsigned i=0; i<dim; i++){
              (faces[i_facet].vertices)[i_vertex].point =
                getpoint(points, dim, (faces[i_facet].vertices)[i_vertex].id);
              // ((faces[i_facet].vertices)[i_vertex].point)[i] =
              //   points[(faces[i_facet].vertices)[i_vertex].id*dim+i];
            }
            // plante dans Haskell: (faces[i_facet].vertices)[i_vertex].point = vertex->point;
            i_vertex++;
          }
          qsort(faces[i_facet].vertices, facesizes[i_facet],
                sizeof(VertexT), cmpvertices);
        }
        { /* facet edges */
          qh_makeridges(qh, facet);
          unsigned n_ridges    = qh_setsize(qh, facet->ridges);
          faces[i_facet].edges = malloc(n_ridges * sizeof(EdgeT));
          printf("nridges: %d, nvertices: %d\n", n_ridges, facesizes[i_facet]);
          ridgeT *ridge, **ridgep;
          unsigned i_ridge = 0;
          FOREACHridge_(facet->ridges){
            faces[i_facet].edges[i_ridge].v1.id =
              qh_pointid(qh, ((vertexT*)ridge->vertices->e[0].p)->point);
            faces[i_facet].edges[i_ridge].v1.point =
              getpoint(points, dim, faces[i_facet].edges[i_ridge].v1.id);
            faces[i_facet].edges[i_ridge].v2.id =
              qh_pointid(qh, ((vertexT*)ridge->vertices->e[1].p)->point);
            faces[i_facet].edges[i_ridge].v2.point =
              getpoint(points, dim, faces[i_facet].edges[i_ridge].v2.id);
            i_ridge++;
          }
        }
        i_facet++;
      }
    }
    {
      facetT *facet;
      unsigned i_facet = 0;
      FORALLfacets{
        faces[i_facet].neighborsize = qh_setsize(qh, facet->neighbors);
        faces[i_facet].neighbors =
          malloc(faces[i_facet].neighborsize * sizeof(unsigned));
        unsigned i_neighbor = 0;
        facetT *neighbor, **neighborp;
        FOREACHneighbor_(facet){
          (faces[i_facet].neighbors)[i_neighbor] = (unsigned) neighbor->id;
          i_neighbor++;
        }
        qsort(faces[i_facet].neighbors, faces[i_facet].neighborsize,
              sizeof(unsigned), cmpfunc);
        i_facet++;
      }
    }

    unsigned nvertices = qh->num_vertices;
    //unsigned* vertices  = malloc(nvertices * sizeof(unsigned));
    VertexT* vertices = malloc(nvertices * sizeof(VertexT));
    {
      vertexT *vertex;
      unsigned i_vertex=0;
      //VertexT vertices_[nvertices];
      FORALLvertices{
//        printf("ivertex: %d\n", i_vertex);
        vertices[i_vertex].id = (unsigned) qh_pointid(qh, vertex->point);
//        vertices[i_vertex].point = vertex->point;
        vertices[i_vertex].point = malloc(dim * sizeof(double));
        for(unsigned i=0; i<dim; i++){
          (vertices[i_vertex].point)[i] = points[vertices[i_vertex].id*dim+i];
        }
//        printf("point: %f %f %f\n", vertices[i_vertex].point[0], vertices[i_vertex].point[1], vertices[i_vertex].point[2]);
        i_vertex++;
      }
      qsort(vertices, nvertices, sizeof(VertexT), cmpvertices);
    }

    out->dim       = dim;
    out->vertices  = vertices;
    out->nvertices = nvertices;
    out->faces     = faces;
    out->facesizes = facesizes;
    out->nfaces    = nfaces;
  } // end if exitocde

  /* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong);   /* free short memory and memory allocator */

  return out;
}
