#define qh_QHimport
#include "qhull_ra.h"
#include "result.h"

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

struct Result* delaunay(
	double* vertices,
	unsigned dim,
	unsigned n,
	unsigned* nf,
	unsigned* exitcode,
	char* tmpFile
)
{
  printf("HELLO\n");
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull d Qt Fn FF Qbb", "");
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
    // unsigned oldids[numfacets];
    // unsigned tricoid[numfacets];
    facetT *facet;                  /* set by FORALLfacets */
    // unsigned ii_facet = 0;
    // FORALLfacets {
    //   if(facet->tricoplanar){
    //     if (facet->f.triowner){
    //       tricoid[ii_facet] = (unsigned) facet->f.triowner->id;
    //     }
    //   }
    //   ii_facet++;
    // }
		FORALLfacets {
			if (!facet->upperdelaunay && facet->simplicial && !facet->degenerate) {
//        oldids[nf[0]] = facet->id;
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
    unsigned* indices = malloc(nf[0] * (dim+1) * sizeof(unsigned));
		double* facetsVolumes = malloc(nf[0] * sizeof(double));
		unsigned* neighbors = malloc(nf[0] * (dim+1) * sizeof(unsigned));
		double* centers = malloc(nf[0] * dim * sizeof(double));
		unsigned* toporient = malloc(nf[0] * sizeof(unsigned));
		double* facetsNormals = malloc(nf[0] * (dim+1) * sizeof(double));
		unsigned neighborok[numfacets];
		for(unsigned f=0; f<numfacets; f++){
			neighborok[f] = 0;
		}

    /* Iterate through facets to extract information - first pass */
		unsigned i_facet = 0; // facet counter
    FORALLfacets {
			vertexT *vertex, **vertexp;
	    facetT *neighbor, **neighborp;
			unsigned j;

			facetsVolumes[i_facet] = facet->f.area;
      toporient[i_facet] = facet->toporient;

			j = 0;
      FOREACHvertex_(facet->vertices) {
        indices[i_facet*(dim+1)+j] = (unsigned) qh_pointid(qh, vertex->point);
        j++;
			}

			for(j=0; j<dim+1; j++){
				facetsNormals[i_facet*(dim+1)+j] = facet->normal[j];
			}

			j = 0;
			FOREACHneighbor_(facet) {
				if(neighbor->id > nf[0] ||
           neighbor->visitid != facetsvisitid[neighbor->id-1])
        {
					neighbors[i_facet*(dim+1)+j] = (unsigned)0;
				}else{
					neighbors[i_facet*(dim+1)+j] = (unsigned)(neighbor->id);
					neighborok[neighbor->id-1] = 1;
				}
				j++;
			}

      if(facet->tricoplanar){
        if(facet->keepcentrum){
          owners[i_facet] = facet->id;
        }else{
          owners[i_facet] = 99;// facet->f.triowner->id;
          // FOREACHneighbor_(facet){
          //   if(neighborok[neighbor->id-1]){//} && neighbor->keepcentrum){
          //     owners[i_facet] = neighbor->f.triowner->id;
          //     //break;
          //   }
          // }
        }
      }else{
        owners[i_facet] = 0;
      }
      printf("owner: %d\n", owners[i_facet]);

			i_facet++;
		}
    free(facetsvisitid);

		/* count number of neighbor facets per vertex */
		unsigned* n_facets_per_vertex = malloc(n * sizeof(unsigned));
		unsigned n_total_vertex_neighbors_facets = 0;
		unsigned verticesFacetsNeighbours[n][nf[0]]; // 0/1 if not neighbour/neighbour
		for(unsigned v=0; v<n; v++){
			n_facets_per_vertex[v] = 0;
			for(unsigned f=0; f<nf[0]; f++){
				verticesFacetsNeighbours[v][f] = 0;
			}
		}
		vertexT *vertex;
		FORALLvertices {
			int vertexid = qh_pointid(qh, vertex->point);
			facetT *neighbor, **neighborp;
			FOREACHneighbor_(vertex){
				if(verticesFacetsNeighbours[vertexid][neighbor->id-1]==0 &&
					 neighborok[neighbor->id-1]==1)
				{
          //printf("neighborid: %d", neighbor->id-1);
					verticesFacetsNeighbours[vertexid][neighbor->id-1] = 1;
					n_facets_per_vertex[vertexid]++;
					n_total_vertex_neighbors_facets++;
				}
			}
		}

    /* second pass: ridges centers and normals */
		unsigned n_adjacencies = nf[0] * (dim+1);
		double* distances = malloc(n_adjacencies * sizeof(double));
		unsigned** ridges = malloc(n_adjacencies * sizeof(unsigned*)); // size n_adjacencies X (2+dim) ; first and second column for facets id
		double** ridgesCenters = malloc(n_adjacencies * sizeof(double*));
		double** ridgesNormals = malloc(n_adjacencies * sizeof(double*));
    double* ridgesAreas = malloc(n_adjacencies * sizeof(double));
//		unsigned* ridgesIds = malloc(n_adjacencies * sizeof(unsigned));
		unsigned flag_vertex_for_ridge[n_adjacencies][n];
		for(unsigned adj=0; adj<n_adjacencies; adj++){
			for(unsigned v=0; v<n; v++){
				flag_vertex_for_ridge[adj][v] = 0;
			}
		}
    unsigned* n_ridges_per_vertex = malloc(n * sizeof(unsigned));
    for(unsigned m=0; m<n; m++){
      n_ridges_per_vertex[m] = 0;
    }
    int isdone[upow(n,dim)];
    for(unsigned u=0; u<upow(n,dim); u++){
      isdone[u] = -1;
    }

		unsigned i_adjacencies = 0;
		i_facet = 0; // facet counter
    FORALLfacets {

			coordT* center = qh_facetcenter(qh, facet->vertices);
			for(unsigned j=0; j<dim; j++){
				centers[i_facet*dim+j] = center[j];
			}

			for(unsigned m=0; m<dim+1; m++){
//				ridgesIds[i_adjacencies] = n_ridges;
				ridges[i_adjacencies] = (unsigned*) malloc((2+dim)*sizeof(unsigned));
				ridges[i_adjacencies][0] = (unsigned)facet->id - 1;
				ridges[i_adjacencies][1] = nf[0];
				unsigned* combination = malloc(dim*sizeof(unsigned));
				unsigned kk=0;
				for(unsigned k=0; k<dim+1; k++){
					if(k!=m){
						combination[kk] = k;
						kk++;
					}
				}
        pointT* points[dim];
        unsigned ids[dim];
        unsigned index=0;
        for(unsigned i=0; i<dim; i++){
          points[i] = ((vertexT*)facet->vertices->e[combination[i]].p)->point;
          ids[i] = (unsigned) qh_pointid(qh, points[i]);
          ridges[i_adjacencies][2+i] = ids[i];
          index += ids[i] * upow(n,i);
        }
				ridgesCenters[i_adjacencies] = (double*) malloc(dim*sizeof(double));
				ridgesNormals[i_adjacencies] = (double*) malloc(dim*sizeof(double));

        if(isdone[index] == -1){
          isdone[index] = (int) i_adjacencies;
          for(unsigned i=0; i<dim; i++){
            flag_vertex_for_ridge[i_adjacencies][ids[i]] = 1;
  					n_ridges_per_vertex[ids[i]]++;
          }
          double* normal = malloc(dim*sizeof(double));
          // setT* vertices = qh_settemp(qh, 3);
          // qh_setappend(qh, &vertices, point1);
          // qh_setappend(qh, &vertices, point2);
          // qh_setappend(qh, &vertices, point3);
          // coordT* ridgeCenter = qh_facetcenter(qh, vertices);
          // qh_settempfree(qh, &vertices);
          if(dim==3){
            pointT* point1 = points[0];
            pointT* point2 = points[1];
            pointT* point3 = points[2];
  					double u1 = point2[0]-point1[0];
  					double v1 = point2[1]-point1[1];
  					double w1 = point2[2]-point1[2];
  					double u2 = point3[0]-point1[0];
  					double v2 = point3[1]-point1[1];
  					double w2 = point3[2]-point1[2];
  					normal[0] = det2_(v1, v2, w1, w2);
  					normal[1] = det2_(u2, u1, w2, w1);
  					normal[2] = det2_(u1, u2, v1, v2);
            ridgesAreas[i_adjacencies] = sqrt(square(normal[0]) +
                                              square(normal[1]) +
                                              square(normal[2]))/2;
  					qh_normalize2(qh, normal, 3, 1, NULL, NULL); // 3:dim 1:toporient
  					ridgesNormals[i_adjacencies][0] = normal[0];
  					ridgesNormals[i_adjacencies][1] = normal[1];
  					ridgesNormals[i_adjacencies][2] = normal[2];
            double offset = -(point1[0]*normal[0]+point1[1]*normal[1]+point1[2]*normal[2]);
            double scal = (point1[0]-center[0])*normal[0]+(point1[1]-center[1])*normal[1]+(point1[2]-center[2])*normal[2];
            // center-M = scal * normal !  pas besoin de solve !!!
            printf("scal: %f\n", scal);
            struct mat3X3 mat ={ { {u1, v1, w1}
  															 , {u2, v2, w2}
  															 , {normal[0], normal[1], normal[2]} } };
  					double rhs[3] = { center[0]*u1 + center[1]*v1 + center[2]*w1
  													, center[0]*u2 + center[1]*v2 + center[2]*w2
  													, -offset };
  					double* ridgeCenter = solve3X3(mat, rhs);
  					ridgesCenters[i_adjacencies][0] = ridgeCenter[0];
  					ridgesCenters[i_adjacencies][1] = ridgeCenter[1];
  					ridgesCenters[i_adjacencies][2] = ridgeCenter[2];
  					printf("Vertex1: %f %f %f\n", point1[0], point1[1], point1[2]);
  					printf("Vertex2: %f %f %f\n", point2[0], point2[1], point2[2]);
  					printf("Vertex3: %f %f %f\n", point3[0], point3[1], point3[2]);
  					printf("NORMAL: %f %f %f\n", normal[0], normal[1], normal[2]);
  					printf("MYDISTANCE: %f\n", qh_distnorm(3, center, normal, &offset));
  					qh_normalize2(qh, normal, 3, 0, NULL, NULL); // 3:dim 1:toporient
            offset = -(point1[0]*normal[0]+point1[1]*normal[1]+point1[2]*normal[2]);
  					printf("OTHERDISTANCE: %f\n", qh_distnorm(3, center, normal, &offset));
  					printf("RIDGE CENTER: %f %f %f\n", ridgeCenter[0], ridgeCenter[1], ridgeCenter[2]);
  					printf("FACET CENTER: %f %f %f\n", center[0], center[1], center[2]);
  					free(ridgeCenter);
          }else if(dim==2){
            pointT* point1 = points[0];
            pointT* point2 = points[1];
            double u1 = point2[0]-point1[0];
            double v1 = point2[1]-point1[1];
            ridgesAreas[i_adjacencies] = sqrt(square(u1)+square(v1));
            ridgesCenters[i_adjacencies][0] = (point1[0]+point2[0])/2;
            ridgesCenters[i_adjacencies][1] = (point1[1]+point2[1])/2;
            normal[0] = v1;
            normal[1] = -u1;
            qh_normalize2(qh, normal, 2, 1, NULL, NULL);
            ridgesNormals[i_adjacencies][0] = normal[0];
            ridgesNormals[i_adjacencies][1] = normal[1];
            double offset = -(point1[0]*normal[0]+point1[1]*normal[1]);
            printf("Vertex1: %f %f\n", point1[0], point1[1]);
            printf("Vertex2: %f %f\n", point2[0], point2[1]);
            printf("NORMAL: %f %f\n", normal[0], normal[1]);
            printf("MYDISTANCE: %f\n", qh_distnorm(2, center, normal, &offset));
            printf("RIDGE CENTER: %f %f\n", ridgesCenters[i_adjacencies][0], ridgesCenters[i_adjacencies][1]);
            printf("FACET CENTER: %f %f\n", center[0], center[1]);
          }else{ // general dimension
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
            ridgesAreas[i_adjacencies] = surface;
            printf("ridge area: %f\n", surface);
            qh_normalize2(qh, normal, dim, 1, NULL, NULL);
            double scal = 0;
            for(unsigned i=0; i<dim; i++){
              scal += (points[0][i]-center[i])*normal[i];
            }
            for(unsigned i=0; i<dim; i++){
              ridgesNormals[i_adjacencies][i] = normal[i];
              ridgesCenters[i_adjacencies][i] = center[i] + scal*normal[i];
            } //M-center = scal * normal
          }
          double squaredDistance = 0;
          double h = 0;
          pointT* otherpoint = ((vertexT*)facet->vertices->e[m].p)->point;
          for(unsigned i=0; i<dim; i++){
            squaredDistance += square(ridgesCenters[i_adjacencies][i]-center[i]);
            h += (ridgesCenters[i_adjacencies][i]-otherpoint[i])*ridgesNormals[i_adjacencies][i];
          }
          distances[i_adjacencies] = sqrt(squaredDistance);
          printf("MYDISTANCE2: %f\n", sqrt(squaredDistance));
          if(h < 0){
            for(unsigned i=0; i<dim; i++){
              ridgesNormals[i_adjacencies][i] *= -1;
            }
          }
          free(normal);
				}else{
					int idone = isdone[index];
					printf("isdone: %d\n", idone);
					ridges[i_adjacencies][1] = ridges[idone][0];
					ridges[idone][1] = (unsigned)facet->id - 1; // (= ridges[i_adjacencies][0])
          ridgesAreas[i_adjacencies] = ridgesAreas[idone];
          double squaredDistance = 0;
          for(unsigned i=0; i<dim; i++){
            ridgesCenters[i_adjacencies][i] = ridgesCenters[idone][i];
            ridgesNormals[i_adjacencies][i] = ridgesNormals[idone][i];
            squaredDistance += square(ridgesCenters[idone][i]-center[i]);
          }
					distances[i_adjacencies] = sqrt(squaredDistance);
				}
				free(combination);

				i_adjacencies++;
			}

			i_facet++;
		}

		printf("LEAVING LOOP");

//		free(ridgesIds);

    unsigned nridges_total = 0;
    unsigned*** verticesRidgesNeighbours = malloc(n * sizeof(unsigned**));
		for(unsigned v=0; v<n; v++){
			// printf("v: %d - n_ridges_per_vertex[v]: %d\n", v, n_ridges_per_vertex[v]);
			verticesRidgesNeighbours[v] = (unsigned**) malloc(n_ridges_per_vertex[v] * sizeof(unsigned*));
			nridges_total += n_ridges_per_vertex[v];
			unsigned increment = 0;
			unsigned count = 0;
			while(increment < n_adjacencies && count<n_ridges_per_vertex[v]){
				// printf("increment: %d\n", increment);
				if(flag_vertex_for_ridge[increment][v] == 1){
					// printf("count: %d\n", count);
					verticesRidgesNeighbours[v][count] = (unsigned*) malloc(dim * sizeof(unsigned));
          for(unsigned i=0; i<dim; i++){
					  verticesRidgesNeighbours[v][count][i] = ridges[increment][2+i];
          }
					count++;
				}
				increment++;
			}
		}
		printf("nridges_total%d:\n", nridges_total);
		printf("LEAVING LOOP\n");

		unsigned* ridges_ = malloc(n_adjacencies*(2+dim)*sizeof(unsigned));
		for(unsigned l=0; l<n_adjacencies; l++){
//			printf("l=%d:\n", l);
			for(unsigned ll=0; ll<2+dim; ll++){
				ridges_[l*(2+dim)+ll] = ridges[l][ll];
//				printf("ll=%d - id=%d ", ll, ridges_[l*dim+ll]);
			}
		}
		double* ridgesCenters_ = malloc(n_adjacencies*dim*sizeof(double));
		double* ridgesNormals_ = malloc(n_adjacencies*dim*sizeof(double));
		for(unsigned l=0; l<n_adjacencies; l++){
			for(unsigned ll=0; ll<dim; ll++){
				ridgesCenters_[l*dim+ll] = ridgesCenters[l][ll];
				ridgesNormals_[l*dim+ll] = ridgesNormals[l][ll];
			}
		}

		unsigned* verticesRidgesNeighbours_ = malloc(nridges_total*dim*sizeof(unsigned));
		unsigned count = 0;
		for(unsigned v=0; v<n; v++){
			for(unsigned r=0; r<n_ridges_per_vertex[v]; r++){
				for(unsigned l=0; l<dim; l++){
					// printf("v: %d, r: %d, l: %d\n", v, r, l);
					// printf("v*n_ridges_per_vertex[v]*3+r*3+l: %d\n", v*n_ridges_per_vertex[v]*3+r*3+l);
					// printf("verticesRidgesNeighbours[v][r][l]: %d\n", verticesRidgesNeighbours[v][r][l]);
					verticesRidgesNeighbours_[count] = verticesRidgesNeighbours[v][r][l];
					count++;
				}
			}
		}
		printf("total count:%d\n", count);

		unsigned* verticesFacetsNeighbours_ =
			malloc(n_total_vertex_neighbors_facets * sizeof(unsigned));
		unsigned inc_vfn_tot = 0;
		for(unsigned v=0; v<n; v++){
			unsigned inc_facet = 0;
			unsigned inc_vfn = 0;
			while(inc_vfn < n_facets_per_vertex[v]){
				if(verticesFacetsNeighbours[v][inc_facet] == 1){
					verticesFacetsNeighbours_[inc_vfn_tot] = inc_facet;
//					printf("v: %d - inc_facet: %d\n", v, inc_facet);
					inc_vfn++;
					inc_vfn_tot++;
				}
				inc_facet++;
			}
		}

	  out->dim       = dim;
	  out->length    = nf[0];
	  out->indices   = indices;
		out->fvolumes  = facetsVolumes;
    out->owners     = owners;
		out->neighbors = neighbors;
		out->centers   = centers;
		out->toporient = toporient;
		out->ridges    = ridges_;
    out->rvolumes  = ridgesAreas;
		out->rcenters  = ridgesCenters_;
		out->rnormals  = ridgesNormals_;
		out->fnormals  = facetsNormals;
		out->rdistances = distances;
		out->vrneighbors = verticesRidgesNeighbours_;
		out->vrnsizes = n_ridges_per_vertex;
		out->vfneighbors = verticesFacetsNeighbours_;
		out->vfnsizes = n_facets_per_vertex;

    for(unsigned l=0; l<n_adjacencies; l++){
				free(ridgesCenters[l]); free(ridgesNormals[l]); free(ridges[l]);
		}
    free(ridgesCenters); free(ridgesNormals); free(ridges);
    for(unsigned v=0; v<n; v++){
			for(unsigned r=0; r<n_ridges_per_vertex[v]; r++){
				free(verticesRidgesNeighbours[v][r]);
			}
      free(verticesRidgesNeighbours[v]);
		}
    // for(unsigned v=0; v<n; v++){
		// 	free(verticesRidgesNeighbours[v]);
		// }
    free(verticesRidgesNeighbours);

	}

	/* Do cleanup regardless of whether there is an error */
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort (qh, &curlong, &totlong);   /* free short memory and memory allocator */

	printf("RETURN\n");
  return out;
}
