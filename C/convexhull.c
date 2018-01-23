#define qh_QHimport
#include "qhull_ra.h"
#include "convexhull.h"
#include "utils.h"

// to use the qsort function - sort vertices according to their ids
int cmpvertices (const void * a, const void * b) {
   return ( (*((VertexT*)a)).id - (*((VertexT*)b)).id );
}
// - sort full vertices
int cmpfullvertices (const void * a, const void * b) {
   return ( (*((FullVertexT*)a)).id - (*((FullVertexT*)b)).id );
}


/* test equality of two _sorted_ arrays */
unsigned equalarraysu(unsigned* array1, unsigned* array2, unsigned length){
  unsigned i;
  for(i=0; i < length; i++){
    if(array1[i] != array2[i]){
      break;
    }
  }
  return i == length;
}

/* return ids of a vector of VertexT */
unsigned* map_vertexid(VertexT* vertices, unsigned nvertices){
  unsigned* ids = malloc(nvertices * sizeof(unsigned));
  for(unsigned v=0; v < nvertices; v++){
    ids[v] = vertices[v].id;
  }
  return ids;
}

// void deepCopyRidge(RidgeT* src, RidgeT* dest) { et dim !
//     dest = malloc(sizeof(RidgeT));
//     *dest = *src;
//     dest->vertices = malloc(src->nvertices * sizeof(VertexT));
//     for(unsigned v=0; v<src->nvertices; v++){
//       dest->vertices[v].id = src.vertices[v].id;
//       memcpy(dest->vertices[v].point, dest->vertices[v].point, dim * sizeof(double));
//     }
// }

/* deep copy of a ridge */
RidgeT copyRidge(RidgeT ridge, unsigned dim){
  RidgeT out;
  out.ridgeOf1  = ridge.ridgeOf1;
  out.ridgeOf2  = ridge.ridgeOf2;
  out.nvertices = ridge.nvertices;
  out.vertices  = malloc(out.nvertices * sizeof(VertexT));
  for(unsigned v=0; v<out.nvertices; v++){
    out.vertices[v].id    = ridge.vertices[v].id;
    out.vertices[v].point = malloc(dim * sizeof(double));
    for(unsigned i=0; i<dim; i++){
      out.vertices[v].point[i] = ridge.vertices[v].point[i];
    }
  }
  return out;
}

/* append to a vector of VertexT */
void appendv(VertexT x, VertexT** array, unsigned length, unsigned* flag){
  *flag = 1;
  for(unsigned i=0; i<length; i++){
    if(x.id == (*(*array + i)).id){
      *flag = 0;
      break;
    }
  }
  if(*flag==1){
    *array = realloc(*array, (length+1)*sizeof(VertexT));
    *(*array + length) = x;
  }
}

/* union of two vectors of VertexT */
void unionv(VertexT** vs1, VertexT* vs2, unsigned l1, unsigned l2, unsigned* l){
  *l = l1;
  for(unsigned v=0; v<l2; v++){
    unsigned pushed;
    appendv(vs2[v], vs1, *l, &pushed);
    if(pushed){
      (*l)++;
    }
  }
  /* sort vertices according to their ids */
  qsort(*vs1, *l, sizeof(VertexT), cmpvertices);
}

/* merge ridges with same ridgeOf's */
RidgeT* mergeRidges(RidgeT* ridges, unsigned nridges, unsigned* newlength){
  // http://www.c4learn.com/c-programs/to-delete-duplicate-elements-in-array.html
  *newlength = nridges;
  unsigned i,j,k;
  for(i = 0; i < nridges; i++){
    for(j = i+1; j < nridges; ){
      if(ridges[i].ridgeOf1 == ridges[j].ridgeOf1 &&
         ridges[i].ridgeOf2 == ridges[j].ridgeOf2)
      {
        unsigned l;
        unionv(&(ridges[i].vertices), ridges[j].vertices,
                 ridges[i].nvertices, ridges[j].nvertices, &l);
        ridges[i].nvertices = l;
        // for(unsigned v=0; v<ridges[j].nvertices; v++){
        //   // unsigned pushed;
        //   // appendv(ridges[j].vertices[v], &(ridges[i].vertices), ridges[i].nvertices, &pushed);
        //   // if(pushed){
        //   //   ridges[i].nvertices++;
        //   // }
        //   unsigned flag = 1;
        //   for(unsigned r=0; r<ridges[i].nvertices; r++){
        //     if(ridges[j].vertices[v].id == ridges[i].vertices[r].id){
        //       flag = 0;
        //       break;
        //     }
        //   }
        //   if(flag==1){
        //     ridges[i].vertices = realloc(ridges[i].vertices, (ridges[i].nvertices+1)*sizeof(VertexT));
        //     ridges[i].vertices[ridges[i].nvertices].id = ridges[j].vertices[v].id;
        //     ridges[i].vertices[ridges[i].nvertices].point = ridges[j].vertices[v].point;
        //     // ridges[i].vertices[ridges[i].nvertices].point = malloc(4*sizeof(double));//ridges[j].vertices[v].point;
        //     // //ridges[i].vertices[ridges[i].nvertices].point = ridges[j].vertices[v].point;
        //     // ridges[i].vertices[ridges[i].nvertices].point[0] = 0;//ridges[j].vertices[v].point[0];
        //     // ridges[i].vertices[ridges[i].nvertices].point[1] = 1;//ridges[j].vertices[v].point[1];
        //     // ridges[i].vertices[ridges[i].nvertices].point[2] = 2;//ridges[j].vertices[v].point[2];
        //     // ridges[i].vertices[ridges[i].nvertices].point[3] = 3;//ridges[j].vertices[v].point[3];
        //     ridges[i].nvertices++;
        //   }
        // }
        // printf("nvertices: %u\n", ridges[i].nvertices);
        // printf("%u\n", ridges[i].vertices[ridges[i].nvertices-1].id);
        // printf("%f\n", ridges[i].vertices[ridges[i].nvertices-1].point[0]);
        (*newlength)--;
        for(k = j; k < nridges; k++){
          ridges[k] = ridges[k+1];
        }
        nridges--;
      }else{
        j++;
      }
    }
  }
  // for(i=0; i<nridges; i++){
  //   unsigned flag = 0;
  //   for(j=i+1; j<nridges; j++){
  //     if(ridges[i].ridgeOf1==ridges[j].ridgeOf1 &&
  //        ridges[i].ridgeOf2==ridges[j].ridgeOf2)
  //     {
  //       flag = 1;
  //       break;
  //     }
  //   }
  //    /* if none of the values in index[0..j] of array is not same as array[i],
  //       then copy the current value to corresponding new position in array */
  //   if(flag){
  //     printf("to merge\n");
  //     unsigned l;
  //     ridges[*newlength].vertices =
  //       unionv(ridges[i].vertices, ridges[j].vertices,
  //              ridges[i].nvertices, ridges[j].nvertices, &l);
  //     ridges[*newlength].nvertices = l;
  //     printf("nvertices: %d\n", l);
  //     ridges[*newlength].ridgeOf1 = ridges[i].ridgeOf1;
  //     ridges[*newlength].ridgeOf2 = ridges[i].ridgeOf2;
  //     (*newlength)++;
  //   }
  // }
  RidgeT* out = malloc(*newlength * sizeof(RidgeT));
  for(unsigned r=0; r<*newlength; r++){
    out[r] = ridges[r];
  }
  return out;
}

/* all ridges from the ridges stored in the faces */
RidgeT* allRidges(FaceT *faces, unsigned nfaces, unsigned dim, unsigned* length){
  RidgeT* out = malloc(faces[0].nridges * sizeof(RidgeT));
  for(unsigned i=0; i < faces[0].nridges; i++){
    out[i] = copyRidge(faces[0].ridges[i], dim);
    out[i].id = i;
    // RidgeT out[i];
    // deepCopyRidge(&(faces[0].ridges[i]), &(out[i]));
  }
  *length    = faces[0].nridges;
  unsigned n = faces[0].nridges;
  for(unsigned f=1; f < nfaces; f++){
    for(unsigned j=0; j < faces[f].nridges; j++){
      unsigned count = 0;
      for(unsigned i=0; i<n; i++){
        unsigned flag = 0;
        for(unsigned v=0; v<faces[f].ridges[j].nvertices; v++){
          if(faces[f].ridges[j].vertices[v].id != out[i].vertices[v].id){
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
      if(count == n){
        out          = realloc(out, (*length+1) * sizeof(RidgeT));
        out[*length] = copyRidge(faces[f].ridges[j], dim);
        out[*length].id = *length;
        // RidgeT out[*length];
        // deepCopyRidge(&(faces[f].ridges[j]), &(out[*length]));
        (*length)++;
      }
    }
    n = *length;
  }
  return out;
}

/* assign ids to the ridges stored in the faces */
void assignRidgesIds(FaceT** faces, unsigned nfaces, RidgeT* allridges,
                     unsigned nallridges)
{
  for(unsigned f=0; f < nfaces; f++){
    for(unsigned fr=0; fr < (*(*faces + f)).nridges; fr++){
      for(unsigned r=0; r < nallridges; r++){
        if((allridges[r].nvertices == (*(*faces + f)).ridges[fr].nvertices) &&
            equalarraysu(map_vertexid(allridges[r].vertices,
                                      allridges[r].nvertices),
                         map_vertexid((*(*faces + f)).ridges[fr].vertices,
                                      allridges[r].nvertices),
                         allridges[r].nvertices))
        {
          (*(*faces + f)).ridges[fr].id = allridges[r].id;
          break;
        }
      }
    }
  }
}

/* squared distance between two points */
double squaredDistance(double* p1, double* p2, unsigned dim){
  double out = 0;
  for(unsigned i=0; i < dim; i++){
    out += square(p1[i] - p2[i]);
  }
  return out;
}


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

/* the threshold distance to detect neighbor vertices */
double ridgeMaxDistance(RidgeT ridge, unsigned v, unsigned dim){
  double dists[ridge.nvertices-1];
  unsigned count = 0;
  for(unsigned w=0; w < ridge.nvertices; w++){
    if(w != v){
      dists[count] = squaredDistance(ridge.vertices[v].point,
                                     ridge.vertices[w].point, dim);
      count++;
    }
  }
  qsort(dists, ridge.nvertices-1, sizeof(double), cmpfuncdbl);
  return dists[1];
}

/* neighbor vertices of a vertex from all ridges*/
unsigned* neighVertices(unsigned id, RidgeT* allridges, unsigned nridges,
                        unsigned dim, unsigned* lengthout)
{ // does not work for dim 2: ridges are singletons ! => les neighs avec les faces - fonction à part
  // wrong in dim4 : two points in a ridge are not necessarily connected
  //    ok with merged ridges
  // other way: qhull/html/qh-code.htm
  unsigned* neighs = malloc(0);
  *lengthout = 0;
  for(unsigned e=0; e<nridges; e++){
    for(unsigned v=0; v<allridges[e].nvertices; v++){
      if(id == allridges[e].vertices[v].id){
        for(unsigned w=0; w<allridges[e].nvertices; w++){
          if(w != v && (dim == 3 || // dim3 pas besoin de tester la distance: il n'y a que deux vertices connectés
             squaredDistance(allridges[e].vertices[w].point,
                             allridges[e].vertices[v].point, dim) <=
              ridgeMaxDistance(allridges[e], v, dim)))
          {
            unsigned pushed;
            appendu(allridges[e].vertices[w].id, &neighs, *lengthout, &pushed);
            if(pushed){
              (*lengthout)++;
            }
          }
        }
        break;
      }
    }
    // if(id == allridges[e].v1.id){
    //   neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
    //   neighs[*length] = allridges[e].v2.id;
    //   (*length)++;
    // }else if(id == allridges[e].v2.id){
    //   neighs = realloc(neighs, (*length+1)*sizeof(unsigned));
    //   neighs[*length] = allridges[e].v1.id;
    //   (*length)++;
    // }
  }
  return neighs;
}

/* neighbor ridges of a vertex */
unsigned* neighRidges(unsigned id, RidgeT* allridges, unsigned nridges,
                     unsigned* length)
{
  unsigned* neighs = malloc(0);
  *length = 0;
  for(unsigned e=0; e<nridges; e++){
    unsigned flag=0;
    for(unsigned v=0; v < allridges[e].nvertices; v++){
      if(id == allridges[e].vertices[v].id){
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

/* whether x1 and x2 belong to array */
unsigned areElementsOf(unsigned x1, unsigned x2, unsigned* array,
                       unsigned length)
{
  unsigned count = 0;
  for(unsigned i=0; (i < length) && (count < 2) ; i++){
    if(x1 == array[i] || x2 == array[i]){
      count++;
    }
  }
  return count==2;
}

/* make face edges from all edges */
unsigned** faceEdges(FaceT face, unsigned** alledges, unsigned nalledges,
                      unsigned* lengthout)
{
  *lengthout = 0;
  // unsigned faceverticesids[face.nvertices];
  // for(unsigned v=0; v < face.nvertices; v++){
  //   faceverticesids[v] = face.vertices[v].id;
  // }
  unsigned* faceverticesids = map_vertexid(face.vertices, face.nvertices);
  unsigned flags[nalledges];
  for(unsigned e=0; e < nalledges; e++){
    if(areElementsOf(alledges[e][0], alledges[e][1], faceverticesids,
                     face.nvertices))
    {
      flags[e] = 1;
      (*lengthout)++;
    }else{
      flags[e] = 0;
    }
  }
  unsigned** out = malloc(*lengthout * sizeof(unsigned*));
  unsigned count = 0;
  for(unsigned e=0; e < nalledges; e++){
    if(flags[e] == 1){
      out[count] = alledges[e];
      count++;
    }
  }
  return out;
}

/* all edges from all vertices */
unsigned** allEdges(FullVertexT* vertices, unsigned nvertices,
                    unsigned outlength)
{
  unsigned** out = malloc(outlength * sizeof(unsigned*));
  for(unsigned i=0; i < vertices[0].nneighsvertices; i++){
    out[i] = malloc(2 * sizeof(unsigned));
    out[i][0] = vertices[0].id;
    out[i][1] = vertices[0].neighvertices[i];
    qsort(out[i], 2, sizeof(unsigned), cmpfunc);
  }
  unsigned n = vertices[0].nneighsvertices;
  for(unsigned v=1; v < nvertices; v++){
    unsigned ids[2];
    for(unsigned i=0; i < vertices[v].nneighsvertices; i++){
      ids[0] = vertices[v].id;
      ids[1] = vertices[v].neighvertices[i];
      qsort(ids, 2, sizeof(unsigned), cmpfunc);
      unsigned j;
      for(j=0; j < n; j++){
        if(ids[0] == out[j][0] && ids[1] == out[j][1]){
          break;
        }
      }
      if(j == n){
        out[n] = malloc(2 * sizeof(unsigned));
        out[n][0] = ids[0]; out[n][1] = ids[1];
        n++;
      }
      if(n == outlength){
        break;
      }
    }
    if(n == outlength){
      break;
    }
  }
  return out;
}

// ATTENTION avec Qt le center dans facet->center est le centre de l'union des triangles,
//  (ainsi que normal et offset mais ça ok)
// dim 4: les "triangles" ont 4 vertices

// un ridge est simplicial ; pour l'hypercube il y a 2 ridges entre 2 faces,
// ils forment le carré à l'intersection

/* main function */
ConvexHullT* convexHull(
	double*   points,
	unsigned  dim,
	unsigned  n,
  unsigned  triangulate,
  unsigned  print,
  char*     summaryFile,
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
  FILE* outfile;
  if(print){
    outfile = stdout;
  }else{
    outfile = NULL;
  }
  qh_zero(qh, errfile);
	exitcode[0] = qh_new_qhull(qh, dim, n, points, ismalloc, flags, outfile,
		                         errfile);
  //fclose(tmpstdout);
  printf("exitcode: %u\n", exitcode[0]);

  ConvexHullT* out = malloc(sizeof(ConvexHullT));

	if (!exitcode[0]) {  /* 0 if no error from qhull */

    /* print summary to file */
    if(*summaryFile != 0){
      FILE* sfile = fopen(summaryFile, "w");
    	qh_printsummary(qh, sfile);
    	fclose(sfile);
    }

    //qh_getarea(qh, qh->facet_list); // no triowner if I do that; do qh_facetarea, not facet->f.area

    unsigned   nfaces    = qh->num_facets;
    FaceT*     faces     = malloc(nfaces * sizeof(FaceT));
    {
      facetT *facet; unsigned i_facet = 0;
      FORALLfacets{
        facet->id                = i_facet; /* for neighbors and ridgeOf */
        faces[i_facet].area      = qh_facetarea(qh, facet);
        faces[i_facet].center    = qh_getcenter(qh, facet->vertices);
        faces[i_facet].normal    = facet->normal;
        faces[i_facet].offset    = facet->offset;
        faces[i_facet].nvertices = (unsigned) qh_setsize(qh, facet->vertices);
        { /* face vertices */
          faces[i_facet].vertices =
            (VertexT*) malloc(faces[i_facet].nvertices * sizeof(VertexT));
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
          qsort(faces[i_facet].vertices, faces[i_facet].nvertices,
                sizeof(VertexT), cmpvertices);
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
        { /* face ridges */
          qh_makeridges(qh, facet);
          unsigned nridges = qh_setsize(qh, facet->ridges);
          RidgeT* ridges = malloc(nridges * sizeof(RidgeT));
          ridgeT *ridge, **ridgep;
          unsigned i_ridge = 0;
          FOREACHridge_(facet->ridges){
            unsigned ridgeSize = qh_setsize(qh, ridge->vertices); // dim-1
//            printf("ridge size: %u\n", ridgeSize);
            ridges[i_ridge].nvertices = ridgeSize;
            unsigned ids[ridgeSize];
            for(unsigned v=0; v<ridgeSize; v++){
              ids[v] =
                qh_pointid(qh, ((vertexT*)ridge->vertices->e[v].p)->point);
            }
            qsort(ids, ridgeSize, sizeof(unsigned), cmpfunc);
            ridges[i_ridge].vertices =
              malloc(ridgeSize * sizeof(VertexT));
            for(unsigned v=0; v < ridgeSize; v++){
              ridges[i_ridge].vertices[v].id = ids[v];
              ridges[i_ridge].vertices[v].point =
                getpoint(points, dim, ids[v]);
            }
            unsigned ridgeofs[2];
            ridgeofs[0] = ridge->bottom->id;
            ridgeofs[1] = ridge->top->id;
            qsort(ridgeofs, 2, sizeof(unsigned), cmpfunc);
            ridges[i_ridge].ridgeOf1 = ridgeofs[0];
            ridges[i_ridge].ridgeOf2 = ridgeofs[1];
            /**/
            i_ridge++;
          }
          /* merge triangulated ridges */
          if(dim > 3){
            unsigned l;
            faces[i_facet].ridges  = mergeRidges(ridges, nridges, &l);
            faces[i_facet].nridges = l;
          }else{ /* dim=2 */
            faces[i_facet].ridges  = ridges;
            faces[i_facet].nridges = nridges;
          }
        }
        /**/
        i_facet++;
      }
    }

    /* make unique ridges */
    unsigned n_allridges;
    RidgeT* allridges = allRidges(faces, nfaces, dim, &n_allridges);
//    printf("nallridges: %u\n", n_allridges);

    /* assign ridges ids to the ridges stored in the faces */
    assignRidgesIds(&faces, nfaces, allridges, n_allridges);

    /* all vertices */
    unsigned nvertices = qh->num_vertices;
    FullVertexT* vertices = malloc(nvertices * sizeof(FullVertexT));
    {
      vertexT *vertex;
      unsigned i_vertex=0;
      FORALLvertices{
        // // test qh_vertexridges
        // setT* vertexRidges = qh_vertexridges(qh, vertex);
        // unsigned nvertexRidges = qh_setsize(qh, vertexRidges);
        // printf("vertex: %d\n", qh_pointid(qh, vertex->point));
        // for(unsigned r=0; r<nvertexRidges; r++){
        //   printf("nvertices in the vertex ridge: %d\n",
        //           qh_setsize(qh, ((ridgeT*)vertexRidges->e[r].p)->vertices));
        //   for(unsigned v=0; v<qh_setsize(qh, ((ridgeT*)vertexRidges->e[r].p)->vertices); v++){
        //     printf("vid: %d - ",
        //           qh_pointid(qh, ((vertexT*)((ridgeT*)vertexRidges->e[r].p)->vertices->e[v].p)->point));
        //   }
        //   printf("\n");
        // }

        /* vertex id and coordinates */
        vertices[i_vertex].id    = (unsigned) qh_pointid(qh, vertex->point);
        vertices[i_vertex].point = getpoint(points, dim, vertices[i_vertex].id);

        /* neighbor faces of the vertex */
        if(dim > 2){ /* because bug for dim=2: qh_setsize(qh, vertex->neighbors) = 0 */
          vertices[i_vertex].nneighfacets = qh_setsize(qh, vertex->neighbors);
          vertices[i_vertex].neighfacets =
            malloc(vertices[i_vertex].nneighfacets * sizeof(unsigned));
          facetT *neighbor, **neighborp;
          unsigned i_neighbor = 0;
          FOREACHneighbor_(vertex){
            vertices[i_vertex].neighfacets[i_neighbor] = neighbor->id;
            i_neighbor++;
          }
        }else{ /* dim=2  - we also treat the neighbor vertices here */
          vertices[i_vertex].nneighfacets    = 2;
          vertices[i_vertex].neighfacets     = malloc(2 * sizeof(unsigned));
          vertices[i_vertex].nneighsvertices = 2;
          vertices[i_vertex].neighvertices   = malloc(2 * sizeof(unsigned));
          unsigned count = 0;
          for(unsigned f=0; f < nfaces; f++){
            for(unsigned i=0; i < 2; i++){
              if(faces[f].vertices[i].id == vertices[i_vertex].id){
                vertices[i_vertex].neighfacets[count]   = f;
                vertices[i_vertex].neighvertices[count] =
                  faces[f].vertices[1-i].id;
                count++;
                break;
              }
            }
            if(count == 2){
              break;
            }
          }
        }
        qsort(vertices[i_vertex].neighfacets, vertices[i_vertex].nneighfacets,
              sizeof(unsigned), cmpfunc);

        /* neighbor vertices of the vertex */
        if(dim > 2){ /* already done for dim=2 */
          unsigned nneighsvertices;
          vertices[i_vertex].neighvertices =
            neighVertices(vertices[i_vertex].id, allridges, n_allridges,
                          dim, &nneighsvertices);
          vertices[i_vertex].nneighsvertices = nneighsvertices;
        }
        qsort(vertices[i_vertex].neighvertices,
              vertices[i_vertex].nneighsvertices, sizeof(unsigned), cmpfunc);

        /* neighbor ridges of the vertex */
        if(dim > 2){
          unsigned nneighridges;
          vertices[i_vertex].neighridges =
            neighRidges(vertices[i_vertex].id, allridges, n_allridges,
                       &nneighridges);
          qsort(vertices[i_vertex].neighridges, nneighridges,
                sizeof(unsigned), cmpfunc);
          vertices[i_vertex].nneighridges = nneighridges;
        }else{ /* dim=2 */
          vertices[i_vertex].nneighridges = 0; /* ridge = vertex singleton */
        }
        /**/
        i_vertex++;
      }
      /* sort vertices according to their ids */
      qsort(vertices, nvertices, sizeof(FullVertexT), cmpfullvertices);
    }

    /* all edges */
    unsigned nalledges = 0;
    for(unsigned v=0; v < nvertices; v++){
      nalledges += vertices[v].nneighsvertices;
    }
    nalledges /= 2;
    unsigned** alledges = allEdges(vertices, nvertices, nalledges);

    { /* faces edges */
      facetT *facet; unsigned i_facet=0;
      FORALLfacets{
        unsigned nfaceedges;
        faces[i_facet].edges  =
          faceEdges(faces[i_facet], alledges, nalledges, &nfaceedges);
        faces[i_facet].nedges = nfaceedges;
        /**/
        i_facet++;
      }
    }

    /* output */
    out->dim       = dim;
    out->vertices  = vertices;
    out->nvertices = nvertices;
    out->faces     = faces;
    out->nfaces    = nfaces;
    out->ridges    = allridges;
    out->nridges   = n_allridges;
    out->edges     = alledges;
    out->nedges    = nalledges;

  } // end if exitocde

  /* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);               /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong); /* free short memory and memory allocator */

  printf("RETURN\n");
  return out;
}
