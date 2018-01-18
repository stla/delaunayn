#define qh_QHimport
#include "qhull_ra.h"
#include "result.h"
#include <math.h> // to use NAN

double* nanvector(int dim){
  double* out = malloc(dim*sizeof(double));
  for(unsigned i=0; i<dim; i++){
    out[i] = NAN;
  }
  return out;
}

void append(unsigned x, unsigned** array, unsigned size, unsigned* flag){
  unsigned _flag = 1;
  for(unsigned i=0; i<size; i++){
    //printf("array[i]: %d\n", *array[i]);
    if(x==*array[i]){
      _flag = 0;
      break;
    }
  }
  if(_flag==1){
    *array = realloc(*array, (size+1)*sizeof(unsigned));
    *flag = _flag;
  }
}

struct mat3X3 {
    double mat[3][3];
};

struct mat3X3 inverse3X3(struct mat3X3 m){
	double det = det3_(m.mat[0][0], m.mat[0][1], m.mat[0][2],
	                   m.mat[1][0], m.mat[1][1], m.mat[1][2],
									   m.mat[2][0], m.mat[2][1], m.mat[2][2]);
	double d00 = det2_(m.mat[1][1], m.mat[1][2], m.mat[2][1], m.mat[2][2]);
	double d01 = det2_(m.mat[1][0], m.mat[1][2], m.mat[2][0], m.mat[2][2]);
	double d02 = det2_(m.mat[1][0], m.mat[1][1], m.mat[2][0], m.mat[2][1]);
	double d10 = det2_(m.mat[0][1], m.mat[0][2], m.mat[2][1], m.mat[2][2]);
	double d11 = det2_(m.mat[0][0], m.mat[0][2], m.mat[2][0], m.mat[2][2]);
	double d12 = det2_(m.mat[0][0], m.mat[0][1], m.mat[2][0], m.mat[2][1]);
	double d20 = det2_(m.mat[0][1], m.mat[0][2], m.mat[1][1], m.mat[1][2]);
	double d21 = det2_(m.mat[0][0], m.mat[0][2], m.mat[1][0], m.mat[1][2]);
	double d22 = det2_(m.mat[0][0], m.mat[0][1], m.mat[1][0], m.mat[1][1]);

	struct mat3X3 out = {
												{ { d00/det, -d10/det,  d20/det}
          					 		, {-d01/det,  d11/det, -d21/det}
			    			 		 		, { d02/det, -d12/det,  d22/det} }
											};
	return out;
}

double* solve3X3(struct mat3X3 mat, double* vec){
	struct mat3X3 m = inverse3X3(mat);
	double* out = malloc(3*sizeof(double));
	out[0] = m.mat[0][0]*vec[0]+m.mat[0][1]*vec[1]+m.mat[0][2]*vec[2];
	out[1] = m.mat[1][0]*vec[0]+m.mat[1][1]*vec[1]+m.mat[1][2]*vec[2];
	out[2] = m.mat[2][0]*vec[0]+m.mat[2][1]*vec[1]+m.mat[2][2]*vec[2];
	return out;
}

unsigned upow(unsigned base, unsigned exp)
{
    unsigned result = 1;
    while (exp)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        base *= base;
    }
    return result;
}

double square(double x){
  return x*x;
}

int cmpfunc (const void * a, const void * b) {
   return ( *(int*)a - *(int*)b );
}

struct Result* delaunay(
	double* vertices,
	unsigned dim,
	unsigned n,
  unsigned degenerate,
	unsigned* nf,
	unsigned* exitcode,
	char* tmpFile
)
{
  printf("HELLO\n");
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull d Qt Fn Qbb", "");
  printf("HELLO\n");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  printf("HELLO\n");
  QHULL_LIB_CHECK
  printf("HELLO\n");

  qh_meminit(qh, stderr);
  // qh_meminitbuffers(qh, qh->IStracing, qh_MEMalign, 7, qh_MEMbufsize,qh_MEMinitbuf);
  // qh_memsize(qh, (int)sizeof(facetT));
  //qh_memsetup(qh);

	boolT ismalloc = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile = NULL;
  FILE* tmpstdout = tmpfile();
  qh_zero(qh, errfile);
  printf("HELLOOO\n");
	exitcode[0] = qh_new_qhull(qh, dim, n, vertices, ismalloc, flags, tmpstdout,
		                         errfile);
  fclose(tmpstdout);
  printf("exitcode: %d\n", exitcode[0]);

  int curlong, totlong; /* to free the memory later */
  struct Result* out = malloc(sizeof(ResultT));

	if (!exitcode[0]) {             /* 0 if no error from qhull */
    printf("START\n");
    FILE* summaryFile = fopen(tmpFile, "w");
  	qh_printsummary(qh, summaryFile);
  	fclose(summaryFile);
  	qh_getarea(qh, qh->facet_list);

		int numfacets = qh->num_facets;
    /* Count the number of facets so we know how much space to allocate */
		nf[0]=0; /* Number of facets */
		int* facetsvisitid = malloc(numfacets * sizeof(int));
    facetT *facet;                  /* set by FORALLfacets */
		FORALLfacets {
			if (!facet->upperdelaunay && facet->simplicial &&
          (degenerate || !facet->degenerate)) // && !facet->redundant
      {
        facetsvisitid[nf[0]] = facet->visitid;
	      nf[0]++;
				facet->id = nf[0];
      }else{
				qh_removefacet(qh, facet);
			}
		}

		// unsigned connectedVertices[n][n];
		// for(unsigned m1=0; m1<n; m1++){
		// 	for(unsigned m2=0; m2<n; m2++){
		// 		connectedVertices[m1][m2] = 0;
		// 	}
		// }

    /* Alocate the space */
    unsigned* owners = malloc(nf[0] * sizeof(unsigned));
    unsigned* facetsIndices = malloc(nf[0] * (dim+1) * sizeof(unsigned));
		double* facetsVolumes = malloc(nf[0] * sizeof(double));
		unsigned* neighbors = malloc(nf[0] * (dim+1) * sizeof(unsigned));
		double* centers = malloc(nf[0] * dim * sizeof(double));
		unsigned* toporient = malloc(nf[0] * sizeof(unsigned));
		double* facetsNormals = malloc(nf[0] * (dim+1) * sizeof(double));
		unsigned neighborok[nf[0]][numfacets];
		for(unsigned f1=0; f1<nf[0]; f1++){
      for(unsigned f2=0; f2<numfacets; f2++){
			  neighborok[f1][f2] = 0;
      }
		}

    /* Iterate through facets to extract information - first pass */
		unsigned i_facet = 0; // facet counter
    FORALLfacets {

			unsigned j;

			facetsVolumes[i_facet] = facet->f.area;
      toporient[i_facet] = facet->toporient;

      vertexT *vertex, **vertexp;
			j = 0;
      FOREACHvertex_(facet->vertices) {
        facetsIndices[i_facet*(dim+1)+j] = (unsigned) qh_pointid(qh, vertex->point);
        j++;
			}

			for(j=0; j<dim+1; j++){
				facetsNormals[i_facet*(dim+1)+j] = facet->normal[j];
			}

      facetT *neighbor, **neighborp;
			j = 0;
			FOREACHneighbor_(facet) {
				if(neighbor->id > nf[0] ||
           neighbor->visitid != facetsvisitid[neighbor->id-1])
        {
					neighbors[i_facet*(dim+1)+j] = (unsigned) 0;
				}else{
					neighbors[i_facet*(dim+1)+j] = (unsigned) neighbor->id;
					neighborok[facet->id-1][neighbor->id-1] = 1;
				}
				j++;
			}

      if(facet->tricoplanar){
        vertexT* apex = facet->vertices->e[0].p;
        FOREACHneighbor_(apex){
          if(neighbor->keepcentrum){
            owners[i_facet] = neighbor->id;
            break;
          }
        }
      }else{
        owners[i_facet] = 0;
      }
      printf("owner: %d\n", owners[i_facet]);

			i_facet++;
		}
    free(facetsvisitid);

		/* count number of neighbor facets per vertex */
    unsigned combinations[dim+1][dim];
    for(unsigned m=0; m<dim+1; m++){
      unsigned kk=0;
      for(unsigned k=0; k<dim+1; k++){
        if(k!=m){
          combinations[m][kk] = k; kk++;
        }
      }
    }
		unsigned* n_facets_per_vertex = malloc(n * sizeof(unsigned));
		unsigned n_total_vertex_neighbors_facets = 0;
		unsigned verticesFacetsNeighbours[n][nf[0]]; // 0/1 if not neighbour/neighbour
    unsigned* connectedVertices[n];
    unsigned* n_vertices_per_vertex = malloc(n * sizeof(unsigned));
		for(unsigned v=0; v<n; v++){
      n_vertices_per_vertex[v] = 0;
      connectedVertices[v] = malloc(0);
			n_facets_per_vertex[v] = 0;
			for(unsigned f=0; f<nf[0]; f++){
				verticesFacetsNeighbours[v][f] = 0;
			}
		}
    for(unsigned f=0; f<nf[0]; f++){
      for(unsigned j=0; j<dim+1; j++){
        unsigned vertexid = facetsIndices[f*(dim+1)+j];
        if(verticesFacetsNeighbours[vertexid][f] == 0){
          verticesFacetsNeighbours[vertexid][f] = 1;
          n_facets_per_vertex[vertexid]++;
          n_total_vertex_neighbors_facets++;
        }
        for(unsigned k=0; k<dim; k++){
          unsigned vertexid2 = facetsIndices[f*(dim+1)+combinations[j][k]];
          unsigned push = 1;
          for(unsigned i=0; i<n_vertices_per_vertex[vertexid]; i++){
            if(vertexid2==connectedVertices[vertexid][i]){
              push = 0; break;
            }
          }
          if(push==1){
            connectedVertices[vertexid] = realloc(connectedVertices[vertexid],
                        (n_vertices_per_vertex[vertexid]+1) * sizeof(unsigned));
            connectedVertices[vertexid][n_vertices_per_vertex[vertexid]] =
              vertexid2;
            n_vertices_per_vertex[vertexid]++;
          }
        }
      }
    }
		// vertexT *vertex;
		// FORALLvertices {
		// 	int vertexid = qh_pointid(qh, vertex->point);
    //   printf("vertexid: %d\n", vertexid);
		// 	facetT *neighbor, **neighborp;
		// 	FOREACHneighbor_(vertex){
		// 		if(verticesFacetsNeighbours[vertexid][neighbor->id-1]==0)// &&
		// 			 //neighborok[neighbor->id-1]==1)
		// 		{
    //       //printf("neighborid: %d", neighbor->id-1);
		// 			verticesFacetsNeighbours[vertexid][neighbor->id-1] = 1;
		// 			n_facets_per_vertex[vertexid]++;
		// 			n_total_vertex_neighbors_facets++;
		// 		}
    //     for(unsigned v=0; v<dim+1; v++){
    //       unsigned pid = qh_pointid(qh, ((vertexT*)neighbor->vertices->e[v].p)->point);
    //       if(pid != vertexid){
    //         unsigned pushed = 1;
    //         for(unsigned i=0; i<n_vertices_per_vertex[vertexid]; i++){
    //           if(pid==connectedVertices[vertexid][i]){
    //             pushed = 0; break;
    //           }
    //         }
    //         if(pushed==1){
    //           connectedVertices[vertexid] = realloc(connectedVertices[vertexid],
    //                           (n_vertices_per_vertex[vertexid]+1) * sizeof(unsigned));
    //           connectedVertices[vertexid][n_vertices_per_vertex[vertexid]] = pid;
    //           n_vertices_per_vertex[vertexid]++;
    //         }
    //       }
    //     }
		// 	}
		// }
    for(unsigned v=0; v<n_vertices_per_vertex[0]; v++){
      printf("connected: %d", connectedVertices[0][v]);
    }


    /************************************************************/
    /* second pass: ridges, centers and normals                 */
    unsigned n_ridges_dup = nf[0] * (dim+1);
    unsigned i_ridge_dup = 0;
    unsigned ridges_dup[n_ridges_dup][2+dim];
    double ridgesCenters[n_ridges_dup][dim];
		double ridgesNormals[n_ridges_dup][dim];
    double* ridgesAreas = malloc(n_ridges_dup * sizeof(double));
    unsigned flag_vertex_for_ridge[n_ridges_dup][n];
		for(unsigned adj=0; adj<n_ridges_dup; adj++){
			for(unsigned v=0; v<n; v++){
				flag_vertex_for_ridge[adj][v] = 0;
			}
		}
    unsigned* n_ridges_per_vertex = malloc(n * sizeof(unsigned));
    for(unsigned m=0; m<n; m++){
      n_ridges_per_vertex[m] = 0;
    }
    i_facet = 0;
    FORALLfacets {

      coordT* center = facet->degenerate ?
                        nanvector(dim)
                        : qh_facetcenter(qh, facet->vertices);
      if(facet->degenerate){
        for(unsigned j=0; j<dim; j++){
  				centers[i_facet*dim+j] = NAN;
  			}
      }else{
  			for(unsigned j=0; j<dim; j++){
  				centers[i_facet*dim+j] = center[j];
  			}
      }
      for(unsigned m=0; m<dim+1; m++){
        ridges_dup[i_ridge_dup][0] = (unsigned) facet->id-1;
        ridges_dup[i_ridge_dup][1] = nf[0];
        pointT* points[dim];
        unsigned ids[dim];
        for(unsigned i=0; i<dim; i++){
          points[i] =
            ((vertexT*)facet->vertices->e[combinations[m][i]].p)->point;
          ids[i] = (unsigned) qh_pointid(qh, points[i]);
        }
        qsort(ids, dim, sizeof(unsigned), cmpfunc);
        for(unsigned i=0; i<dim; i++){
          ridges_dup[i_ridge_dup][2+i] = ids[i];
          flag_vertex_for_ridge[i_ridge_dup][ids[i]] = 1;
        }
  	    facetT *neighbor, **neighborp;
        FOREACHneighbor_(facet){
          if(neighborok[facet->id-1][neighbor->id-1]==1){
            unsigned ok;
            for(unsigned mm=0; mm<dim+1; mm++){
              ok = 0;
              unsigned ids2[dim];
              for(unsigned i=0; i<dim; i++){
                ids2[i] = (unsigned)
                          qh_pointid(qh,
                            ((vertexT*)neighbor->vertices->
                                              e[combinations[mm][i]].p)->point);
              }
              qsort(ids2, dim, sizeof(unsigned), cmpfunc);
              for(unsigned i=0; i<dim; i++){
                if(ids2[i] != ids[i]){
                  break;
                }else{
                  ok++;
                }
              }
              if(ok==dim){
                break;
              }
            }
            if(ok==dim){
              ridges_dup[i_ridge_dup][1] = (unsigned) neighbor->id-1;
              printf("facet: %d\n", facet->id-1);
              printf("neighbor: %d\n", neighbor->id-1);
              break;
            }
          }
        } // end FOREACHneighbor_(facet)
        double normal[dim];
        if(dim==2){
          double u1 = points[1][0] - points[0][0];
          double v1 = points[1][1] - points[0][1];
          printf("u1: %f", u1);
          printf("v1: %f", v1);
          ridgesAreas[i_ridge_dup] = sqrt(square(u1)+square(v1));
          ridgesCenters[i_ridge_dup][0] = (points[0][0]+points[1][0])/2;
          ridgesCenters[i_ridge_dup][1] = (points[0][1]+points[1][1])/2;
          normal[0] = v1;
          normal[1] = -u1;
        }else{
          int parity=1;
          double squaredNorm = 0;
          for(unsigned i=0; i<dim; i++){
            double** rows = malloc((dim-1) * sizeof(double*));
            for(unsigned j=0; j<dim-1; j++){
              rows[j] = (double*) malloc((dim-1) * sizeof(double));
              for(unsigned k=0; k<dim-1; k++){
                unsigned kk = k<i ? k : k+1;
                rows[j][k] = points[j+1][kk] - points[0][kk];
              }
            }
            boolT nearzero;
            normal[i] = parity * qh_determinant(qh, rows, dim-1, &nearzero);
            squaredNorm += square(normal[i]);
            for(unsigned j=0; j<dim-1; j++){
              free(rows[j]);
            }
            free(rows);
            parity = -parity;
          }
          double surface = sqrt(squaredNorm);
          for(unsigned k=2; k<dim-1; k++){
            surface /= k;
          }
          ridgesAreas[i_ridge_dup] = surface;
        }
        qh_normalize2(qh, normal, dim, 1, NULL, NULL);
        for(unsigned i=0; i<dim; i++){
          ridgesNormals[i_ridge_dup][i] = normal[i];
        }
        if(dim>2){
          if(facet->degenerate){
            for(unsigned i=0; i<dim; i++){
              ridgesCenters[i_ridge_dup][i] = NAN;
            }
          }else{
            double scal = 0;
            for(unsigned i=0; i<dim; i++){
              scal += (points[0][i]-center[i]) * normal[i];
            }
            for(unsigned i=0; i<dim; i++){
              ridgesCenters[i_ridge_dup][i] = center[i] + scal*normal[i];
            }
          }
        }
        if(!facet->degenerate || dim==2){
          double h = 0;
          pointT* otherpoint = ((vertexT*)facet->vertices->e[m].p)->point;
          for(unsigned i=0; i<dim; i++){
            h += (ridgesCenters[i_ridge_dup][i]-otherpoint[i]) *
                 ridgesNormals[i_ridge_dup][i];
          }
          if(h < 0){
            for(unsigned i=0; i<dim; i++){
              ridgesNormals[i_ridge_dup][i] *= -1;
            }
          }
        }
        i_ridge_dup++;
      } // end loop combinations (m)
      i_facet++;
    } // end FORALLfacets

		printf("LEAVING LOOP");

    for(unsigned l=0; l<n_ridges_dup; l++){
			for(unsigned v=0; v<n; v++){
        if(flag_vertex_for_ridge[l][v] == 1){
          n_ridges_per_vertex[v]++;
        }
			}
		}

		unsigned* ridges_ = malloc(n_ridges_dup * (2+dim) * sizeof(unsigned));
		for(unsigned l=0; l<n_ridges_dup; l++){
//			printf("l=%d:\n", l);
			for(unsigned ll=0; ll<2+dim; ll++){
				ridges_[l*(2+dim)+ll] = ridges_dup[l][ll];
//				printf("ll=%d - id=%d ", ll, ridges_[l*dim+ll]);
			}
		}
		double* ridgesCenters_ = malloc(n_ridges_dup * dim * sizeof(double));
		double* ridgesNormals_ = malloc(n_ridges_dup * dim * sizeof(double));
		for(unsigned l=0; l<n_ridges_dup; l++){
			for(unsigned ll=0; ll<dim; ll++){
				ridgesCenters_[l*dim+ll] = ridgesCenters[l][ll];
				ridgesNormals_[l*dim+ll] = ridgesNormals[l][ll];
			}
		}

    unsigned n_ridges_per_vertex_total = 0;
    unsigned*** verticesRidgesNeighbours = malloc(n * sizeof(unsigned**));
		for(unsigned v=0; v<n; v++){
			verticesRidgesNeighbours[v] =
        (unsigned**) malloc(n_ridges_per_vertex[v] * sizeof(unsigned*));
      n_ridges_per_vertex_total += n_ridges_per_vertex[v];
			unsigned increment = 0;
			unsigned count = 0;
			while(increment < n_ridges_dup && count<n_ridges_per_vertex[v]){
				if(flag_vertex_for_ridge[increment][v] == 1){
					verticesRidgesNeighbours[v][count] =
            (unsigned*) malloc(dim * sizeof(unsigned));
          for(unsigned i=0; i<dim; i++){
					  verticesRidgesNeighbours[v][count][i] = ridges_dup[increment][2+i];
          }
					count++;
				}
				increment++;
			}
		}
		unsigned* verticesRidgesNeighbours_ =
      malloc(n_ridges_per_vertex_total * dim * sizeof(unsigned));
		unsigned count = 0;
		for(unsigned v=0; v<n; v++){
			for(unsigned r=0; r<n_ridges_per_vertex[v]; r++){
				for(unsigned l=0; l<dim; l++){
					verticesRidgesNeighbours_[count] = verticesRidgesNeighbours[v][r][l];
					count++;
				}
			}
		}

		unsigned* verticesFacetsNeighbours_ =
			malloc(n_total_vertex_neighbors_facets * sizeof(unsigned));
		unsigned inc_vfn_tot = 0;
		for(unsigned v=0; v<n; v++){
			unsigned inc_facet = 0; unsigned inc_vfn = 0;
			while(inc_vfn < n_facets_per_vertex[v]){
				if(verticesFacetsNeighbours[v][inc_facet] == 1){
					verticesFacetsNeighbours_[inc_vfn_tot] = inc_facet;
					inc_vfn++; inc_vfn_tot++;
				}
				inc_facet++;
			}
		}

    unsigned n_total_vertex_neighbors_vertices = 0;
    for(unsigned v=0; v<n; v++){
      n_total_vertex_neighbors_vertices += n_vertices_per_vertex[v];
    }
    unsigned* connectedVertices_ =
			malloc(n_total_vertex_neighbors_vertices * sizeof(unsigned));
    unsigned inc_vvn_tot=0;
		for(unsigned v=0; v<n; v++){
      for(unsigned inc_vvn=0; inc_vvn<n_vertices_per_vertex[v]; inc_vvn++){
        connectedVertices_[inc_vvn_tot] = connectedVertices[v][inc_vvn];
        inc_vvn_tot++;
      }
		}

    // double* distances = malloc(n_ridges_dup * sizeof(double));
    // for(unsigned i=0; i<n_ridges_dup; i++){
    //   distances[i] = 1;
    // }

	  out->dim         = dim;
	  out->length      = nf[0];
	  out->indices     = facetsIndices;
		out->fvolumes    = facetsVolumes;
    out->owners      = owners;
		out->neighbors   = neighbors;
		out->centers     = centers;
		out->toporient   = toporient;
		out->ridges      = ridges_;
    out->rvolumes    = ridgesAreas;
		out->rcenters    = ridgesCenters_;
		out->rnormals    = ridgesNormals_;
		out->fnormals    = facetsNormals;
//		out->rdistances = distances;
		out->vrneighbors = verticesRidgesNeighbours_;
		out->vrnsizes    = n_ridges_per_vertex;
		out->vfneighbors = verticesFacetsNeighbours_;
		out->vfnsizes    = n_facets_per_vertex;
    out->vvneighbors = connectedVertices_;
    out->vvnsizes    = n_vertices_per_vertex;

    for(unsigned v=0; v<n; v++){
			free(connectedVertices[v]);
			for(unsigned r=0; r<n_ridges_per_vertex[v]; r++){
				free(verticesRidgesNeighbours[v][r]);
			}
      free(verticesRidgesNeighbours[v]);
		}
    free(verticesRidgesNeighbours);

	}

	/* Do cleanup regardless of whether there is an error */
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort (qh, &curlong, &totlong);   /* free short memory and memory allocator */

	printf("RETURN\n");
  return out;
}
