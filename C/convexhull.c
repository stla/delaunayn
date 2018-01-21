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

RidgeT* allEdges(FaceT *faces, unsigned* edgesizes, unsigned nfaces,
                 unsigned dim, unsigned* length) // dim-1 = ridgeSize
{
  RidgeT* out = malloc(edgesizes[0] * sizeof(RidgeT));
  for(unsigned i=0; i<edgesizes[0]; i++){
    out[i] = faces[0].edges[i];
  }
  *length    = edgesizes[0];
  unsigned n = edgesizes[0];
  for(unsigned f=1; f<nfaces; f++){
    for(unsigned j=0; j<edgesizes[f]; j++){
      unsigned count = 0;
      for(unsigned i=0; i<n; i++){
        unsigned flag = 0;
        for(unsigned v=0; v<dim-1; v++){
          if(faces[f].edges[j].vertices[v].id != out[i].vertices[v].id){
            flag = 1;
            break;
          }
        }
        if(flag){
          count++;
        }else{
          break;
        }
      }
      if(count==n){
        out          = realloc(out, (*length+1) * sizeof(RidgeT));
        out[*length] = faces[f].edges[j];
        (*length)++;
      }
    }
    n = *length;
  }
  return out;
}

double squaredDistance(double* p1, double* p2, unsigned dim){
  double out = 0;
  for(unsigned i=0; i<dim; i++){
    out += square(p1[i] - p2[i]);
  }
  return out;
}

// double* middle(double* p1, double* p2, unsigned dim){
//   double* out = malloc(dim * sizeof(double));
//   for(unsigned i=0; i<dim; i++){
//     out[i] = (p1[i] + p2[i])/2;
//   }
//   return out;
// }
//
// // unsigned connectedPoints(double* p1, double* p2, double* o, unsigned dim){
// //   double d1 = squaredDistance(p1, o, dim);
// //   double d2 = squaredDistance(p2, o, dim);
// //   double d  = squaredDistance(middle(p1, p2, dim), o, dim);
// //   printf("d1: %f, d2: %f, d: %f\n", d1, d2, d);
// //   return d>=d1 || d>=d2;
// // } NIMP
//
// double* ridgeCentroid(RidgeT ridge, unsigned dim){
//   double* out = malloc(dim * sizeof(double));
//   for(unsigned i=0; i<dim; i++){
//     out[i] = 0;
//     for(unsigned v=0; v<dim-1; v++){
//       out[i] += ridge.vertices[v].point[i];
//     }
//     out[i] /= dim - 1;
//   }
//   return out;
// }

double ridgeMaxDistance(RidgeT ridge, unsigned dim){
  double dists[(dim-1)*(dim-2)/2]; // 0 for dim2
  unsigned count=0;
  for(unsigned v1=0; v1<dim-2; v1++){
    for(unsigned v2=v1+1; v2<dim-1; v2++){
      dists[count] = squaredDistance(ridge.vertices[v1].point, ridge.vertices[v2].point, dim);
    }
  }
  qsort(dists, (dim-1)*(dim-2)/2, sizeof(double), cmpfuncdbl);
  return dists[dim-2];
}

unsigned* neighVertices(unsigned id, RidgeT* alledges, unsigned nedges,
                        unsigned dim, unsigned* length)
{ // does not work for dim 2: ridges are singletons !
  // wrong in dim4 : two points in a ridge are not necessarily connected
  unsigned* neighs = malloc(0);
  *length = 0;
  for(unsigned e=0; e<nedges; e++){
    for(unsigned v=0; v<dim-1; v++){
      if(id == alledges[e].vertices[v].id){
        for(unsigned vv=0; vv<dim-1; vv++){
          if(vv != v && squaredDistance(alledges[e].vertices[vv].point, alledges[e].vertices[v].point, dim) <= ridgeMaxDistance(alledges[e], dim)){
            unsigned pushed;
            appendu(alledges[e].vertices[vv].id, &neighs, *length, &pushed);
            if(pushed){
              (*length)++;
            }
          }
        }
        break;
      }
    }
    // if(id == alledges[e].v1.id){
    //   neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
    //   neighs[*length] = alledges[e].v2.id;
    //   (*length)++;
    // }else if(id == alledges[e].v2.id){
    //   neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
    //   neighs[*length] = alledges[e].v1.id;
    //   (*length)++;
    // }
  }
  return neighs;
}

unsigned* neighEdges(unsigned id, RidgeT* alledges, unsigned nedges,
                     unsigned dim, unsigned* length)
{
  unsigned* neighs = malloc(0);
  *length = 0;
  for(unsigned e=0; e<nedges; e++){
    unsigned flag=0;
    for(unsigned v=0; v<dim-1; v++){
      if(id == alledges[e].vertices[v].id){
        flag = 1;
        break;
      }
    }
    if(flag){
      neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
      neighs[*length] = e;
      (*length)++;
    }
  }
  return neighs;
}

// ATTENTION avec Qt le center est le centre de l'union des triangles, ainsi que normal et center
// dim 4: les "triangles" ont 4 vertices

// un ridge est simplicial ; pour l'hypercube il y a 2 ridges entre 2 faces,
// ils forment le carré à l'intersection

ConvexHullT* convexHull(
	double*   points,
	unsigned  dim,
	unsigned  n,
  unsigned  triangulate,
	unsigned* exitcode
)
{
	char flags[250]; /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull s FF %s", triangulate ? "Qt" : "");
	qhT qh_qh;       /* Qhull's data structure */
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

	if (!exitcode[0]) {  /* 0 if no error from qhull */
    // FILE* summaryFile = fopen(tmpFile, "w");
  	// qh_printsummary(qh, summaryFile);
  	// fclose(summaryFile);

    //qh_getarea(qh, qh->facet_list);

    unsigned   nfaces    = qh->num_facets;
    FaceT*     faces     = malloc(nfaces * sizeof(FaceT));
    unsigned*  facesizes = malloc(nfaces * sizeof(unsigned));
    unsigned   edgesizes[nfaces]; // inutile c'est égal à facesizes... not in 4D!
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        facet->id              = i_facet; // for neighbors
        faces[i_facet].area    = qh_facetarea(qh, facet); // facet->f.area;
        faces[i_facet].center  = qh_getcenter(qh, facet->vertices);
        faces[i_facet].normal  = facet->normal;
        faces[i_facet].offset  = facet->offset;
        facesizes[i_facet] =
          (unsigned) qh_setsize(qh, facet->vertices);
        { /* face vertices */
          faces[i_facet].vertices =
            (VertexT*) malloc(facesizes[i_facet] * sizeof(VertexT));
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices){
            faces[i_facet].vertices[i_vertex].id =
              (unsigned) qh_pointid(qh, vertex->point);
            faces[i_facet].vertices[i_vertex].point =
              malloc(dim * sizeof(double));
            faces[i_facet].vertices[i_vertex].point =
              getpoint(points, dim, faces[i_facet].vertices[i_vertex].id);
            // plante dans Haskell: (faces[i_facet].vertices)[i_vertex].point = vertex->point;
            i_vertex++;
          }
          qsort(faces[i_facet].vertices, facesizes[i_facet], sizeof(VertexT),
                cmpvertices);
        }
        /**/
        i_facet++;
      }
    }

    { /* neighbor faces, faces families, and ridges */
      facetT *facet;
      unsigned i_facet = 0;
      FORALLfacets{
        {
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
        }
        { /* face family, when option Qt */
          if(facet->tricoplanar){
            faces[i_facet].family = facet->f.triowner->id;
            // faire ça dans delaunay.c ; il ne faut pas faire qh_getarea
            // vertexT* apex = facet->vertices->e[0].p;
            // facetT *neighbor, **neighborp;
            // FOREACHneighbor_(apex){
            //   if(neighbor->keepcentrum){
            //     faces[i_facet].family = neighbor->id;
            //     break;
            //   }
            // }
          }else{
            faces[i_facet].family = -1;
          }
        }
        { /* face edges */
          qh_makeridges(qh, facet);
          faces[i_facet].nedges = qh_setsize(qh, facet->ridges);
          printf("nridges: %d, nvertices: %d\n", faces[i_facet].nedges, facesizes[i_facet]); // the same - not in 4D !
          edgesizes[i_facet]    = faces[i_facet].nedges;
          faces[i_facet].edges  = malloc(faces[i_facet].nedges * sizeof(RidgeT));
          ridgeT *ridge, **ridgep;
          unsigned i_ridge = 0;
          FOREACHridge_(facet->ridges){
            unsigned ridgeSize = qh_setsize(qh, ridge->vertices); // dim-1
            printf("ridge size: %d\n", ridgeSize);
            unsigned ids[ridgeSize];
            for(unsigned v=0; v<ridgeSize; v++){
              ids[v] =
                qh_pointid(qh, ((vertexT*)ridge->vertices->e[v].p)->point);
            }
            qsort(ids, ridgeSize, sizeof(unsigned), cmpfunc);
            faces[i_facet].edges[i_ridge].vertices =
              malloc(ridgeSize * sizeof(VertexT));
            for(unsigned v=0; v<ridgeSize; v++){
              faces[i_facet].edges[i_ridge].vertices[v].id = ids[v];
              faces[i_facet].edges[i_ridge].vertices[v].point =
                getpoint(points, dim, ids[v]);
            }
            faces[i_facet].edges[i_ridge].ridgeOf1 = ridge->bottom->id;
            faces[i_facet].edges[i_ridge].ridgeOf2 = ridge->top->id;
            /**/
            i_ridge++;
          }
        }
        /**/
        i_facet++;
      }
    }
    unsigned n_alledges;
    RidgeT* alledges = allEdges(faces, edgesizes, nfaces, dim, &n_alledges);

    /* all vertices */
    unsigned nvertices = qh->num_vertices;
    FullVertexT* vertices = malloc(nvertices * sizeof(FullVertexT));
    {
      vertexT *vertex;
      unsigned i_vertex=0;
      FORALLvertices{
        /* vertex id and coordinates */
        vertices[i_vertex].id    = (unsigned) qh_pointid(qh, vertex->point);
        vertices[i_vertex].point = getpoint(points, dim, vertices[i_vertex].id);
        /* neighbor faces of the vertex */
        vertices[i_vertex].nneighfacets = qh_setsize(qh, vertex->neighbors); // 0 for dim 2!
        printf("nneighfacets: %d\n", qh_setsize(qh, vertex->neighbors));
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
        /* neighbor vertices of the vertex */
        unsigned nneighsvertices;
        vertices[i_vertex].neighvertices =
          neighVertices(vertices[i_vertex].id, alledges, n_alledges,
                        dim, &nneighsvertices);
        qsort(vertices[i_vertex].neighvertices, nneighsvertices,
              sizeof(unsigned), cmpfunc);
        vertices[i_vertex].nneighsvertices = nneighsvertices;
        /* neighbor edges of the vertex */
        unsigned nneighedges;
        vertices[i_vertex].neighedges =
          neighEdges(vertices[i_vertex].id, alledges, n_alledges,
                     dim, &nneighedges);
        qsort(vertices[i_vertex].neighedges, nneighedges,
              sizeof(unsigned), cmpfunc);
        vertices[i_vertex].nneighedges = nneighedges;
        /**/
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
	qh_freeqhull(qh, !qh_ALL);               /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong); /* free short memory and memory allocator */

  printf("RETURN\n");
  return out;
}
