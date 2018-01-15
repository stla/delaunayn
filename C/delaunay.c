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
  sprintf(flags, "qhull d Qt Fn Qbb", "");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK

  qh_meminit(qh, stderr);
  // qh_meminitbuffers(qh, qh->IStracing, qh_MEMalign, 7, qh_MEMbufsize,qh_MEMinitbuf);
  // qh_memsize(qh, (int)sizeof(facetT));
  //qh_memsetup(qh);

	boolT ismalloc = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile = NULL;
  FILE* tmpstdout = tmpfile();
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

    unsigned* indices;
    double* areas;
    unsigned* neighbors;
    double* centers;
    double* facetsNormals;
    unsigned* toporient;
    unsigned** ridges; // size n_adjacencies X (2+dim) ; first and second column for facets id
    double** ridgesCenters;
    double** ridgesNormals;
    double* distances;
    unsigned n_adjacencies;
    unsigned*** verticesRidgesNeighbours = malloc(n * sizeof(unsigned**));
    unsigned nridges_total;
    unsigned* n_ridges_per_vertex = malloc(n * sizeof(unsigned));
    for(unsigned m=0; m<n; m++){
      n_ridges_per_vertex[m] = 0;
    }
    unsigned* verticesFacetsNeighbours_;

		int numfacets = qh->num_facets;
    /* Count the number of facets so we know how much space to allocate */
		nf[0]=0; /* Number of facets */
		int* facetsvisitid = malloc(numfacets * sizeof(int));
    facetT *facet;                  /* set by FORALLfacets */
		FORALLfacets {
			if (!facet->upperdelaunay && facet->simplicial && !facet->degenerate) {
	        nf[0]++;
					facet->id = nf[0];
					facetsvisitid[nf[0]] = facet->visitid;
      }else{
				qh_removefacet(qh, facet);
			}
		}

		// unsigned connectedVertices[n][n]; // rempli plus bas
		// for(unsigned m1=0; m1<n; m1++){
		// 	for(unsigned m2=0; m2<n; m2++){
		// 		connectedVertices[m1][m2] = 0;
		// 	}
		// }

    /* Alocate the space */
    indices = (unsigned*) malloc(nf[0] * (dim+1) * sizeof(unsigned));
		areas = (double*) malloc(nf[0] * sizeof(double));
		neighbors = (unsigned*) malloc(nf[0] * (dim+1) * sizeof(unsigned));
		centers = (double*) malloc(nf[0] * dim * sizeof(double));
		toporient = (unsigned*) malloc(nf[0] * sizeof(unsigned));
		facetsNormals = (double*) malloc(nf[0] * dim * sizeof(double));
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

			areas[i_facet] = facet->f.area;

			j = 0;
      FOREACHvertex_(facet->vertices) {
        indices[i_facet*(dim+1)+j] = qh_pointid(qh, vertex->point);
        j++;
			}

			for(j=0; j<dim; j++){
				facetsNormals[i_facet*dim+j] = facet->normal[j];
			}

			j = 0;
			FOREACHneighbor_(facet) {
				if(neighbor->id > nf[0] || neighbor->visitid != facetsvisitid[neighbor->id]){
					neighbors[i_facet*(dim+1)+j] = (unsigned)0;
				}else{
					neighbors[i_facet*(dim+1)+j] = (unsigned)(neighbor->id);
					neighborok[neighbor->id-1] = 1;
				}
				j++;
			}

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
          printf("neighborid: %d", neighbor->id-1);
					verticesFacetsNeighbours[vertexid][neighbor->id-1] = 1;
					n_facets_per_vertex[vertexid]++;
					n_total_vertex_neighbors_facets++;
				}
			}
		}

    /* second pass: ridges centers and normals */
		n_adjacencies = nf[0] * (dim+1);
		unsigned n_ridges = 0;
		distances = malloc(n_adjacencies * sizeof(double));
		ridges = malloc(n_adjacencies * sizeof(unsigned*)); // (intersections, adjacencies, ridges)
		ridgesCenters = malloc(n_adjacencies * sizeof(double*));
		ridgesNormals = malloc(n_adjacencies * sizeof(double*));
		unsigned* ridgesIds = malloc(n_adjacencies * sizeof(unsigned));
		unsigned flag_vertex_for_ridge[n_adjacencies][n];
		for(unsigned adj=0; adj<n_adjacencies; adj++){
			for(unsigned v=0; v<n; v++){
				flag_vertex_for_ridge[adj][v] = 0;
			}
		}
		// pour ne pas calculer 2 fois:
		// int isdone[n][n][n];
		// for(unsigned n1=0; n1<n; n1++){
		// 	for(unsigned n2=0; n2<n; n2++){
		// 		for(unsigned n3=0; n3<n; n3++){
		// 			isdone[n1][n2][n3] = -1;
		// 		}
		// 	}
		// }
    int isdone[upow(n,dim)];
    for(unsigned u=0; u<upow(n,dim); u++){
      isdone[u] = -1;
    }

		unsigned i_adjacencies = 0;
		i_facet = 0; // facet counter
    FORALLfacets {

			toporient[i_facet] = facet->toporient;

			coordT* center = qh_facetcenter(qh, facet->vertices);
			for(unsigned j=0; j<dim; j++){
				centers[i_facet*dim+j] = center[j];
			}

			for(unsigned m=0; m<dim+1; m++){
				ridgesIds[i_adjacencies] = n_ridges;
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
				// pointT* point1 = ((vertexT*)facet->vertices->e[combination[0]].p)->point;
				// pointT* point2 = ((vertexT*)facet->vertices->e[combination[1]].p)->point;
				// pointT* point3 = ((vertexT*)facet->vertices->e[combination[2]].p)->point;
        // unsigned id1 = (unsigned)qh_pointid(qh, point1);
				// unsigned id2 = (unsigned)qh_pointid(qh, point2);
				// unsigned id3 = (unsigned)qh_pointid(qh, point3);
				// ridges[i_adjacencies][2+0] = id1;
				// ridges[i_adjacencies][2+1] = id2;
				// ridges[i_adjacencies][2+2] = id3;
        pointT* points[dim];
        unsigned ids[dim];
        unsigned index=0;
        for(unsigned i=0; i<dim; i++){
          points[i] = ((vertexT*)facet->vertices->e[combination[i]].p)->point;
          ids[i] = (unsigned) qh_pointid(qh, points[i]);
          ridges[i_adjacencies][2+i] = ids[i];
          index += ids[i] * upow(n,i);
        }
        pointT* otherpoint = ((vertexT*)facet->vertices->e[m].p)->point;

				ridgesCenters[i_adjacencies] = (double*) malloc(dim*sizeof(double));
				ridgesNormals[i_adjacencies] = (double*) malloc(dim*sizeof(double));

				// if(isdone[id1][id2][id3] == -1){
        if(isdone[index] == -1){
					// isdone[id1][id2][id3] = (int)i_adjacencies;
					// isdone[id2][id3][id1] = (int)i_adjacencies; // pas besoin car ma comb est toujours croissante
					// isdone[id3][id1][id2] = (int)i_adjacencies;
					// isdone[id3][id2][id1] = (int)i_adjacencies;
					// isdone[id2][id1][id3] = (int)i_adjacencies;
					// isdone[id1][id3][id2] = (int)i_adjacencies;
          isdone[index] = (int) i_adjacencies;
					n_ridges++;
					// connectedVertices[id1][id2] = 1;
					// connectedVertices[id1][id3] = 1;
					// connectedVertices[id2][id3] = 1;
					// connectedVertices[id2][id1] = 1;
					// connectedVertices[id3][id1] = 1;
					// connectedVertices[id3][id2] = 1;
					// flag_vertex_for_ridge[i_adjacencies][id1] = 1;
					// flag_vertex_for_ridge[i_adjacencies][id2] = 1;
					// flag_vertex_for_ridge[i_adjacencies][id3] = 1;
					// n_ridges_per_vertex[id1]++;
					// n_ridges_per_vertex[id2]++;
					// n_ridges_per_vertex[id3]++;
          for(unsigned i=0; i<dim; i++){
            flag_vertex_for_ridge[i_adjacencies][ids[i]] = 1;
  					n_ridges_per_vertex[ids[i]]++;
          }

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
  					double* normal = malloc(3*sizeof(double));
  					normal[0] = det2_(v1, v2, w1, w2);
  					normal[1] = det2_(u2, u1, w2, w1);
  					normal[2] = det2_(u1, u2, v1, v2);
  					qh_normalize2(qh, normal, 3, 1, NULL, NULL); // 3:dim 1:toporient
  					ridgesNormals[i_adjacencies][0] = normal[0];
  					ridgesNormals[i_adjacencies][1] = normal[1];
  					ridgesNormals[i_adjacencies][2] = normal[2];
            double offset = -(point1[0]*normal[0]+point1[1]*normal[1]+point1[2]*normal[2]);
//  					distances[i_adjacencies] = qh_distnorm(3, center, normal, &offset);
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
  					double dd = sqrt ((center[0]-ridgeCenter[0])*(center[0]-ridgeCenter[0]) +
  					                  (center[1]-ridgeCenter[1])*(center[1]-ridgeCenter[1]) +
  														(center[2]-ridgeCenter[2])*(center[2]-ridgeCenter[2]));
  					printf("MYDISTANCE2: %f\n", dd);
  					printf("RIDGE CENTER: %f %f %f\n", ridgeCenter[0], ridgeCenter[1], ridgeCenter[2]);
  					printf("FACET CENTER: %f %f %f\n", center[0], center[1], center[2]);
  					free(ridgeCenter);
          }else if(dim==2){
            pointT* point1 = points[0];
            pointT* point2 = points[1];
            double u1 = point2[0]-point1[0];
            double v1 = point2[1]-point1[1];
            ridgesCenters[i_adjacencies][0] = (point1[0]+point2[0])/2;
            ridgesCenters[i_adjacencies][1] = (point1[1]+point2[1])/2;
            double* normal = malloc(2*sizeof(double));
            normal[0] = v1;
            normal[1] = -u1;
            qh_normalize2(qh, normal, 2, 1, NULL, NULL);
            ridgesNormals[i_adjacencies][0] = normal[0];
            ridgesNormals[i_adjacencies][1] = normal[1];
            double offset = -(point1[0]*normal[0]+point1[1]*normal[1]);
//            distances[i_adjacencies] = qh_distnorm(2, center, normal, &offset);
            printf("Vertex1: %f %f\n", point1[0], point1[1]);
            printf("Vertex2: %f %f\n", point2[0], point2[1]);
            printf("NORMAL: %f %f\n", normal[0], normal[1]);
            printf("MYDISTANCE: %f\n", qh_distnorm(2, center, normal, &offset));
            double dd = sqrt ((center[0]-ridgesCenters[i_adjacencies][0])*(center[0]-ridgesCenters[i_adjacencies][0]) +
                              (center[1]-ridgesCenters[i_adjacencies][1])*(center[1]-ridgesCenters[i_adjacencies][1]));
            printf("MYDISTANCE2: %f\n", dd);
            printf("RIDGE CENTER: %f %f\n", ridgesCenters[i_adjacencies][0], ridgesCenters[i_adjacencies][1]);
            printf("FACET CENTER: %f %f\n", center[0], center[1]);
            double xx = atan2(normal[1], normal[0]) - atan2(v1, u1);
            printf("angle: %f\n", xx);
            double xxx = (center[0]-qh->interior_point[0])*normal[0] + (center[1]-qh->interior_point[1])*normal[1];
            printf("scalar product: %f\n", xxx);
          }
          double squaredDistance = 0;
          double h = 0;
          for(unsigned i=0; i<dim; i++){
            squaredDistance += square(ridgesCenters[i_adjacencies][i]-center[i]);
            h += (ridgesCenters[i_adjacencies][i]-otherpoint[i])*ridgesNormals[i_adjacencies][i];
          }
          distances[i_adjacencies] = sqrt(squaredDistance);
          if(h < 0){
            for(unsigned i=0; i<dim; i++){
              ridgesNormals[i_adjacencies][i] *= -1;
            }
          }
				}else{
					int idone = isdone[index];
					printf("isdone %d: ", idone);
					ridges[i_adjacencies][1] = ridges[idone][0];
					ridges[idone][1] = (unsigned)facet->id - 1; // (= ridges[i_adjacencies][0])
//          double offset=0;
          double squaredDistance = 0;
//          double h = 0;
          for(unsigned i=0; i<dim; i++){
            ridgesCenters[i_adjacencies][i] = ridgesCenters[idone][i];
            ridgesNormals[i_adjacencies][i] = ridgesNormals[idone][i];
            squaredDistance += square(ridgesCenters[idone][i]-center[i]);
//            h += (ridgesCenters[idone][i]-otherpoint[i])*ridgesNormals[idone][i];
          }
					distances[i_adjacencies] = sqrt(squaredDistance);
//  					qh_distnorm(dim, center, ridgesNormals[idone], &offset);
				}
				free(combination);

				i_adjacencies++;
			}
			i_facet++;
		}

		printf("LEAVING LOOP");

		free(ridgesIds);

		nridges_total = 0;
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
					verticesRidgesNeighbours_[count] =
						verticesRidgesNeighbours[v][r][l];
					count++;
				}
			}
		}
		printf("total count:%d\n", count);

		verticesFacetsNeighbours_ =
			malloc(n_total_vertex_neighbors_facets * sizeof(unsigned));
		unsigned inc_vfn_tot = 0;
		for(unsigned v=0; v<n; v++){
			unsigned inc_facet = 0;
			unsigned inc_vfn = 0;
			while(inc_vfn < n_facets_per_vertex[v]){
				if(verticesFacetsNeighbours[v][inc_facet] == 1){
					verticesFacetsNeighbours_[inc_vfn_tot] = inc_facet;
					printf("v: %d - inc_facet: %d\n", v, inc_facet);
					inc_vfn++;
					inc_vfn_tot++;
				}
				inc_facet++;
			}
		}

	  out->dim       = dim;
	  out->length    = nf[0];
	  out->indices   = indices;
		out->areas     = areas;
		out->neighbors = neighbors;
		out->centers   = centers;
		out->toporient = toporient;
		out->ridges    = ridges_;
		out->rcenters  = ridgesCenters_;
		out->rnormals  = ridgesNormals_;
		out->fnormals  = facetsNormals;
		out->rdistances = distances;
		out->vrneighbors = verticesRidgesNeighbours_;
		out->vrnsizes = n_ridges_per_vertex;
		out->vfneighbors = verticesFacetsNeighbours_;
		out->vfnsizes = n_facets_per_vertex;

		free(ridges);
		free(ridgesCenters);
		free(ridgesNormals);
    free(verticesRidgesNeighbours);

	}


	/* Do cleanup regardless of whether there is an error */
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort (qh, &curlong, &totlong);   /* free short memory and memory allocator */
	// if (exitcode) {
	// 	error("Received error code %d from qhull.", exitcode);
	// }


	printf("RETURN\n");
  return out;
}
