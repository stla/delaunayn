#define qh_QHimport
#include "qhull_ra.h"
#include "convexhull.h"
#include "utils.h"

// to use the qsort function
int cmpvertices (const void * a, const void * b) {
   return ( (*((VertexT*)a)).id - (*((VertexT*)b)).id );
}
int cmpfullvertices (const void * a, const void * b) {
   return ( (*((FullVertexT*)a)).id - (*((FullVertexT*)b)).id );
}

double* getpoint(double* points, unsigned dim, unsigned id){
  double* out = malloc(dim * sizeof(double));
  for(unsigned i=0; i<dim; i++){
    out[i] = points[id*dim+i];
  }
  return out;
}

void appendu(unsigned x, unsigned** array, unsigned length, unsigned* flag){
  *flag = 1;
  for(unsigned i=0; i<length; i++){
    if(x==*(*array + i)){
      *flag = 0;
      break;
    }
  }
  if(*flag==1){
    *array = realloc(*array, (length+1)*sizeof(unsigned));
    *(*array + length) = x;
  }
}

EdgeT* allEdges(FaceT *faces, unsigned* edgesizes, unsigned nfaces, unsigned* length){
  EdgeT* out = malloc(edgesizes[0] * sizeof(EdgeT));
  for(unsigned i=0; i<edgesizes[0]; i++){
    out[i] = faces[0].edges[i];
  }
  *length = edgesizes[0];
  unsigned n = edgesizes[0];
  for(unsigned f=1; f<nfaces; f++){
    for(unsigned j=0; j<edgesizes[f]; j++){
      unsigned flag = 0;
      for(unsigned i=0; i<n; i++){
        if(faces[f].edges[j].v1.id != out[i].v1.id ||
           faces[f].edges[j].v2.id != out[i].v2.id)
        {
          flag++;
        }else{
          break;
        }
      }
      if(flag==n){
        out = realloc(out, (*length+1) * sizeof(EdgeT));
        out[*length] = faces[f].edges[j];
        (*length)++;
      }
    }
    n = *length;
  }
  return out;
}

// TODO neighbor facets of vertices (vertex->neighbors)

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
    unsigned   edgesizes[nfaces]; // inutile c'est égal à facesizes...
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        facet->id              = i_facet; // for neighbors
        faces[i_facet].area    = facet->f.area;
        faces[i_facet].center  = facet->center;
        faces[i_facet].normal  = facet->normal;
        faces[i_facet].offset  = facet->offset;
        facesizes[i_facet] =
          (unsigned) qh_setsize(qh, facet->vertices);
        faces[i_facet].vertices =
          (VertexT*) malloc(facesizes[i_facet] * sizeof(VertexT));
        { // face vertices
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices){
            faces[i_facet].vertices[i_vertex].id =
              (unsigned) qh_pointid(qh, vertex->point);
            faces[i_facet].vertices[i_vertex].point =
              malloc(dim * sizeof(double));
            for(unsigned i=0; i<dim; i++){
              faces[i_facet].vertices[i_vertex].point =
                getpoint(points, dim, faces[i_facet].vertices[i_vertex].id);
              // ((faces[i_facet].vertices)[i_vertex].point)[i] =
              //   points[(faces[i_facet].vertices)[i_vertex].id*dim+i];
            }
            // plante dans Haskell: (faces[i_facet].vertices)[i_vertex].point = vertex->point;
            i_vertex++;
          }
          qsort(faces[i_facet].vertices, facesizes[i_facet],
                sizeof(VertexT), cmpvertices);
        }
        { /* face edges */
          qh_makeridges(qh, facet);
          unsigned n_ridges    = qh_setsize(qh, facet->ridges);
          printf("nridges: %d, nvertices: %d\n", n_ridges, facesizes[i_facet]);
          edgesizes[i_facet]   = n_ridges;
          faces[i_facet].edges = malloc(n_ridges * sizeof(EdgeT));
          ridgeT *ridge, **ridgep;
          unsigned i_ridge = 0;
          FOREACHridge_(facet->ridges){
            unsigned ids[2] =
              { qh_pointid(qh, ((vertexT*)ridge->vertices->e[0].p)->point)
              , qh_pointid(qh, ((vertexT*)ridge->vertices->e[1].p)->point) };
            qsort(ids, 2, sizeof(unsigned), cmpfunc);
            faces[i_facet].edges[i_ridge].v1.id = ids[0];
//              qh_pointid(qh, ((vertexT*)ridge->vertices->e[0].p)->point);
            faces[i_facet].edges[i_ridge].v1.point =
              getpoint(points, dim, ids[0]); //faces[i_facet].edges[i_ridge].v1.id);
            faces[i_facet].edges[i_ridge].v2.id = ids[1];
//              qh_pointid(qh, ((vertexT*)ridge->vertices->e[1].p)->point);
            faces[i_facet].edges[i_ridge].v2.point =
              getpoint(points, dim, ids[1]); //faces[i_facet].edges[i_ridge].v2.id);
            i_ridge++;
          }
        }
        i_facet++;
      }
    }
    unsigned n_alledges;
    EdgeT* alledges = allEdges(faces, edgesizes, nfaces, &n_alledges);

    { /* neighbor faces */
      facetT *facet;
      unsigned i_facet = 0;
      FORALLfacets{
        faces[i_facet].neighborsize = qh_setsize(qh, facet->neighbors);
        faces[i_facet].neighbors =
          malloc(faces[i_facet].neighborsize * sizeof(unsigned));
        unsigned i_neighbor = 0;
        facetT *neighbor, **neighborp;
        FOREACHneighbor_(facet){
          faces[i_facet].neighbors[i_neighbor] = (unsigned) neighbor->id;
          i_neighbor++;
        }
        qsort(faces[i_facet].neighbors, faces[i_facet].neighborsize,
              sizeof(unsigned), cmpfunc);
        i_facet++;
      }
    }

    /* vertices */
    unsigned nvertices = qh->num_vertices;
    //unsigned* vertices  = malloc(nvertices * sizeof(unsigned));
    FullVertexT* vertices = malloc(nvertices * sizeof(FullVertexT));
    {
      vertexT *vertex;
      unsigned i_vertex=0;
      //VertexT vertices_[nvertices];
      FORALLvertices{
        printf("vertex %d\n", vertex->id);
        vertices[i_vertex].id = (unsigned) qh_pointid(qh, vertex->point);
        vertices[i_vertex].point = getpoint(points, dim, vertices[i_vertex].id);
        vertices[i_vertex].nneighfacets = qh_setsize(qh, vertex->neighbors);
        vertices[i_vertex].neighfacets =
          malloc(vertices[i_vertex].nneighfacets * sizeof(unsigned));
        facetT *neighbor, **neighborp;
        unsigned i_neighbor = 0;
        FOREACHneighbor_(vertex){
          vertices[i_vertex].neighfacets[i_neighbor] = neighbor->id;
          i_neighbor++;
        }
        qsort(vertices[i_vertex].neighfacets, vertices[i_vertex].nneighfacets,
              sizeof(unsigned), cmpfunc);
        i_vertex++;
      }
      qsort(vertices, nvertices, sizeof(FullVertexT), cmpfullvertices);
    }

    out->dim       = dim;
    out->vertices  = vertices;
    out->nvertices = nvertices;
    out->faces     = faces;
    out->facesizes = facesizes;
    out->nfaces    = nfaces;
    out->edges     = alledges;
    out->nedges    = n_alledges;
  } // end if exitocde

  /* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong);   /* free short memory and memory allocator */

  return out;
}
