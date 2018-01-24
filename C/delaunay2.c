#define qh_QHimport
#include "qhull_ra.h"
#include "delaunay2.h"
#include "utils.h"
// void printfacet(qhT* qh, facetT* facet){
//   vertexT *vertex, **vertexp;
//   FOREACHvertex_(facet->vertices){
//     printf("facetid: %d, pointid: %d ", facet->id, qh_pointid(qh, vertex->point));
//   }
// }

unsigned facetOK_(facetT* facet, unsigned degenerate){
  return !facet->upperdelaunay && (degenerate || !facet->degenerate);
} // && simplicial, && !facet->redundant - pas de simplicial avec Qt


TesselationT* tesselation(
	double*   sites,
	unsigned  dim,
	unsigned  n,
  unsigned  degenerate,
	unsigned* exitcode
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
	boolT ismalloc  = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile   = NULL;
  FILE* outfile = stdout;
  qh_zero(qh, errfile);
  printf("HELLOOO\n");
	exitcode[0] = qh_new_qhull(qh, dim, n, sites, ismalloc, flags, outfile,
		                         errfile);
  //fclose(tmpstdout);
  printf("exitcode: %u\n", exitcode[0]);

  TesselationT* out = malloc(sizeof(TesselationT));

	if (!exitcode[0]) {             /* 0 if no error from qhull */
    printf("START\n");

  	//qh_getarea(qh, qh->facet_list);

    /* Count the number of facets so we know how much space to allocate */
		unsigned nfacets = 0; /* Number of facets */
    {
      facetT *facet;  /* set by FORALLfacets */
  		FORALLfacets {

        printf("facet:%u\n", nfacets);

  			if(facetOK_(facet, degenerate)){
          facet->id = nfacets;
  	      nfacets++;
        }else{
  				qh_removefacet(qh, facet);
  			}
  		}
    }

    /* Alocate the space */
    TileT* allfacets = malloc(nfacets * sizeof(TileT));

    // unsigned* owners        = malloc(nf * sizeof(unsigned));
    // unsigned* facetsIndices = malloc(nf * (dim+1) * sizeof(unsigned));
		// double*   facetsVolumes = malloc(nf * sizeof(double));
		// unsigned* neighbors     = malloc(nf * (dim+1) * sizeof(unsigned));
		// double*   centers       = malloc(nf * dim * sizeof(double));
		// unsigned* toporient     = malloc(nf * sizeof(unsigned));
		// double*   facetsNormals = malloc(nf * (dim+1) * sizeof(double));

  	{ /* Iterate through facets to extract information - first pass */
      facetT* facet;
      unsigned i_facet = 0;
      FORALLfacets {

        printf("facet:%u\n", facet->id);

        // ceci fait planter: fais getarea après le test tricoplanar
//        allfacets[i_facet].simplex.volume = qh_facetarea(qh, facet);
        allfacets[i_facet].orientation    = facet->toporient ? 1 : -1;
        allfacets[i_facet].simplex.normal = facet->normal;
        allfacets[i_facet].simplex.offset = facet->offset;

        { /* vertices ids */
          allfacets[i_facet].simplex.sitesids =
            malloc((dim+1) * sizeof(unsigned));
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices) {

            printf("i_vertex:%u\n", i_vertex);

            allfacets[i_facet].simplex.sitesids[i_vertex] =
              qh_pointid(qh, vertex->point);
            i_vertex++;
    			}
          qsort(allfacets[i_facet].simplex.sitesids, dim+1, sizeof(unsigned),
                cmpfunc);
        }

        { /* neighbors facets */
          facetT *neighbor, **neighborp;
    			unsigned flag[dim+1];
          allfacets[i_facet].nneighbors = 0;
          unsigned i_neighbor = 0;
    			FOREACHneighbor_(facet) {

            printf("neighborid:%u\n", neighbor->id);

            flag[i_neighbor] = facetOK_(neighbor, degenerate);
            if(flag[i_neighbor]){
              allfacets[i_facet].nneighbors++;
            }
            i_neighbor++;
          }
          allfacets[i_facet].neighbors = malloc(allfacets[i_facet].nneighbors * sizeof(unsigned));
          unsigned countok = 0;
          i_neighbor = 0;
          FOREACHneighbor_(facet) {
            if(flag[i_neighbor]){

              printf("flag neigbor:%u\n", neighbor->id);

              allfacets[i_facet].neighbors[countok] = neighbor->id;
              countok++;
            }
            /**/
            i_neighbor++;
          }
        }

        /* facet family */
        if(facet->tricoplanar){
          printf("tricoplanar\n");
          allfacets[i_facet].family = facet->f.triowner->id;
        }else{
          allfacets[i_facet].family = -1;
        }

        /**/
  			i_facet++;
  		}
    }

		/* count number of neighbor facets and neighbor vertices per vertex */
    /* --- we will use the following combinations, also used later */
    unsigned combinations[dim+1][dim];
    for(unsigned m=0; m<dim+1; m++){
      unsigned kk=0;
      for(unsigned k=0; k<dim+1; k++){
        if(k!=m){
          combinations[m][kk] = k;
          kk++;
        }
      }
    }
    /* --- initialize all sites */
    SiteT* allsites = malloc(n * sizeof(SiteT));
    /* --- array to flag neighbors - 0/1 if not neighbour/neighbour */
		unsigned verticesFacetsNeighbours[n][nfacets];
    { /* loop on all vertices */
      vertexT* vertex;
      unsigned i_vertex = 0;
      FORALLvertices{

        printf("i_vertex again:%u\n", i_vertex);

        allsites[i_vertex].id = qh_pointid(qh, vertex->point);
        allsites[i_vertex].nneighsites = 0;
        allsites[i_vertex].neighsites = malloc(0);
        allsites[i_vertex].nneightiles = 0;
        for(unsigned i_facet=0; i_facet < nfacets; i_facet++){
          verticesFacetsNeighbours[i_vertex][i_facet] = 0;
        }
        /**/
        i_vertex++;
      }
    }

    for(unsigned i_facet=0; i_facet < nfacets; i_facet++){

      printf("i_facet:%u \n", i_facet);

      for(unsigned j=0; j<dim+1; j++){
        unsigned vertexid = allfacets[i_facet].simplex.sitesids[j];
        if(verticesFacetsNeighbours[vertexid][i_facet] == 0){
          verticesFacetsNeighbours[vertexid][i_facet] = 1;
          allsites[vertexid].nneightiles++;
        }
        for(unsigned k=0; k<dim; k++){
          unsigned vertexid2 =
            allfacets[i_facet].simplex.sitesids[combinations[j][k]];
          unsigned pushed;

          printf("appendu\n");

          appendu(vertexid2, &allsites[vertexid].neighsites,
                  allsites[vertexid].nneighsites, &pushed);
          if(pushed){

            printf("push\n");

            allsites[vertexid].nneighsites++;
          }
        }
      }
    }

    printf("BYEBYE LOOP\n");

    /************************************************************/
    /* second pass: ridges, centers, normals and volumes        */
    unsigned n_ridges_dup = nfacets * (dim+1); /* number of ridges with duplicates */
    SubTileT* allridges_dup = malloc(n_ridges_dup * sizeof(SubTileT));
    for(unsigned r=0; r < n_ridges_dup; r++){
      allridges_dup[r].simplex.sitesids = malloc(dim * sizeof(unsigned));
      allridges_dup[r].flag = 0;
    }
    for(unsigned v=0; v < n; v++){
      allsites[v].nneighridges = 0;
    }
    qh_getarea(qh, qh->facet_list); /* make facets volumes, available in facet->f.area */
    unsigned n_ridges = 0; /* count distinct ridges */
    { /* loop on facets */
      facetT *facet;
      unsigned i_ridge_dup  = 0;
      unsigned i_facet = 0;
      FORALLfacets {

        printf("loop on facets again\n");
        printf("ifacet: %u\n", i_facet);

        allfacets[i_facet].simplex.volume = facet->f.area;
        allfacets[i_facet].simplex.center =
          facet->degenerate ? nanvector(dim)
                              : qh_facetcenter(qh, facet->vertices);

        printf("facetcenter ok ?\n");

        allfacets[i_facet].nridges   = dim+1;
        allfacets[i_facet].ridgesids = malloc((dim+1) * sizeof(unsigned));

        /* loop on the combinations - it increments i_ridge_dup */
        for(unsigned m=0; m < dim+1; m++){

          printf("m(combination)=%u\n", m);

          allridges_dup[i_ridge_dup].ridgeOf1 = facet->id;
          allridges_dup[i_ridge_dup].ridgeOf2 = -1; /* this means "nothing" */
          unsigned ids[dim];
          for(unsigned i=0; i < dim; i++){

            printf("combinations[m][i]=%u\n", combinations[m][i]);

            ids[i] = allfacets[i_facet].simplex.sitesids[combinations[m][i]];
          }
          unsigned done = 0;
          for(unsigned r=0; r < i_ridge_dup; r++){
            if(allridges_dup[r].ridgeOf2 == (int) facet->id &&
               allridges_dup[r].flag==1)
            {
              printf("FLLLLLLLAAAG");
              unsigned ids2[dim];
              unsigned i;
              for(i=0; i<dim; i++){
                ids2[i] = allridges_dup[r].simplex.sitesids[i];
                if(ids2[i] != ids[i]){
                  break;
                }
              }
              if(i==dim){
                printf("DONE = 1\n");
                allfacets[i_facet].ridgesids[m] = allridges_dup[r].id;
                done = 1;
                break;
              }
            }
          }
          if(done==0){

            printf("done=0\n");

            allridges_dup[i_ridge_dup].flag = 1;
            allridges_dup[i_ridge_dup].id = n_ridges;
            allfacets[i_facet].ridgesids[m] = n_ridges;
            n_ridges++;
            for(unsigned i=0; i<dim; i++){

              printf("ids[i]:%u\n", ids[i]);

              allridges_dup[i_ridge_dup].simplex.sitesids[i] = ids[i];
              allsites[ids[i]].nneighridges++;
            }

            {
              facetT *neighbor, **neighborp;
              FOREACHneighbor_(facet){

                printf("loop on neighbor - %u\n", neighbor->id);

                unsigned fnid = neighbor->id;
                if(facetOK_(neighbor, degenerate)){
                  unsigned ok;
                  for(unsigned mm=0; mm<dim+1; mm++){
                    ok = 0;
                    for(unsigned i=0; i<dim; i++){
                      if(allfacets[fnid].simplex.sitesids[combinations[mm][i]] != ids[i]){
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

                    printf("ok=dim\n");

                    allridges_dup[i_ridge_dup].ridgeOf2 = (int) fnid;
                    break;
                  }
                }
              } // end FOREACHneighbor_(facet)
            }

            printf("end loop on neighbors\n");

            pointT* points[dim];
            for(unsigned i=0; i<dim; i++){
              points[i] = getpoint(sites, dim, ids[i]);
            }
            double normal[dim];
            if(dim==2){

              printf("calculate center\n");

              double u1 = points[1][0] - points[0][0];
              double v1 = points[1][1] - points[0][1];
              allridges_dup[i_ridge_dup].simplex.volume = sqrt(square(u1)+square(v1));
              allridges_dup[i_ridge_dup].simplex.center = middle(points[0], points[1], dim);
              normal[0] = v1; normal[1] = -u1;
            }else{
              int parity=1;
              double squaredNorm = 0;
              for(unsigned i=0; i < dim; i++){
                double** rows = malloc((dim-1) * sizeof(double*));
                for(unsigned j=0; j < dim-1; j++){
                  rows[j] = (double*) malloc((dim-1) * sizeof(double));
                  for(unsigned k=0; k < dim-1; k++){
                    unsigned kk = k<i ? k : k+1;
                    rows[j][k] = points[j+1][kk] - points[0][kk];
                  }
                }
                boolT nearzero;
                normal[i] = parity * qh_determinant(qh, rows, dim-1, &nearzero);
                squaredNorm += square(normal[i]);
                for(unsigned j=0; j < dim-1; j++){
                  free(rows[j]);
                }
                free(rows);
                parity = -parity;
              }
              double surface = sqrt(squaredNorm);
              for(unsigned k=2; k < dim-1; k++){
                surface /= k;
              }
              allridges_dup[i_ridge_dup].simplex.volume = surface;
            }
            qh_normalize2(qh, normal, dim, 1, NULL, NULL);
            allridges_dup[i_ridge_dup].simplex.normal =
              malloc(dim * sizeof(double));
            for(unsigned i=0; i < dim; i++){
              allridges_dup[i_ridge_dup].simplex.normal[i] = normal[i];
            }
            allridges_dup[i_ridge_dup].simplex.offset =
              - dotproduct(points[0], normal, dim);
            if(dim>2){
              if(facet->degenerate){
                allridges_dup[i_ridge_dup].simplex.center = nanvector(dim);
              }else{
                allridges_dup[i_ridge_dup].simplex.center =
                  malloc(dim * sizeof(double));
                double scal = 0;
                for(unsigned i=0; i < dim; i++){
                  scal += (points[0][i]-allfacets[i_facet].simplex.center[i]) *
                            normal[i];
                }
                for(unsigned i=0; i<dim; i++){
                  allridges_dup[i_ridge_dup].simplex.center[i] =
                    allfacets[i_facet].simplex.center[i] + scal*normal[i];
                }
              }
            }
            if(!facet->degenerate || dim==2){
              double h = 0;
              pointT* otherpoint = ((vertexT*)facet->vertices->e[m].p)->point;
              for(unsigned i=0; i<dim; i++){
                h += (allridges_dup[i_ridge_dup].simplex.center[i]-otherpoint[i]) *
                     allridges_dup[i_ridge_dup].simplex.normal[i];
              }
              if(h < 0){
                for(unsigned i=0; i<dim; i++){
                  allridges_dup[i_ridge_dup].simplex.normal[i] *= -1;
                }
              }
            }
            for(unsigned i=0; i<dim; i++){

              printf("free points\n");

              free(points[i]);
            }
          }
          i_ridge_dup++;
        } // end loop combinations (m)
        qsort(allfacets[i_facet].ridgesids, dim+1, sizeof(unsigned), cmpfunc); 
        i_facet++;
      } // end FORALLfacets
    }

/////////////////////////////////////////////////////////////////////////

		printf("LEAVING LOOP");

    /* make unique ridges */
    SubTileT* allridges = malloc(n_ridges * sizeof(SubTileT));
    unsigned inc_ridge = 0;
		for(unsigned l=0; l <n_ridges_dup; l++){
      if(allridges_dup[l].flag){
        allridges[inc_ridge] = allridges_dup[l];
        inc_ridge++;
      }
		}

    /* make neighbor ridges per vertex */
    unsigned* i_ridges_per_vertex = malloc(n * sizeof(unsigned));
		for(unsigned v=0; v < n; v++){
      allsites[v].neighridgesids =
        malloc(allsites[v].nneighridges * sizeof(unsigned));
      i_ridges_per_vertex[v] = 0;
    }
    for(unsigned l=0; l < n_ridges_dup; l++){
      if(allridges_dup[l].flag){
        for(unsigned i=0; i < dim; i++){
          unsigned v = allridges_dup[l].simplex.sitesids[i];
          allsites[v].neighridgesids[i_ridges_per_vertex[v]] =
            allridges_dup[l].id;
          i_ridges_per_vertex[v]++;
        }
      }
    }

		//unsigned inc_vfn_tot = 0;
    /* make neighbor tiles per vertex */
		for(unsigned v=0; v < n; v++){
      allsites[v].neightiles = malloc(allsites[v].nneightiles * sizeof(unsigned));
			unsigned inc_facet = 0; unsigned inc_vfn = 0;
			while(inc_vfn < allsites[v].nneightiles){
				if(verticesFacetsNeighbours[v][inc_facet] == 1){
          allsites[v].neightiles[inc_vfn] = inc_facet;
					inc_vfn++; //inc_vfn_tot++;
				}
				inc_facet++;
			}
		}

		for(unsigned v=0; v<n; v++){
      qsort(allsites[v].neighsites, allsites[v].nneighsites, sizeof(unsigned),
            cmpfunc);
		}

	  out->sites      = allsites;
    out->tiles      = allfacets;
	  out->ntiles     = nfacets;
	  out->subtiles   = allridges;
    out->nsubtiles  = n_ridges;

    free(allridges_dup);
    free(i_ridges_per_vertex);

	}

	/* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong);   /* free short memory and memory allocator */

	printf("RETURN\n");
  return out;
}


void testdel2(){
  double sites[27] = {0,0,0, 0,0,1, 0,1,0, 0,1,1, 1,0,0, 1,0,1, 1,1,0, 1,1,1, 0.5,0.5,0.5};
  unsigned exitcode;
  unsigned dim = 3;
  TesselationT* x = tesselation(sites, dim, 9, 0, &exitcode);
  printf("TESTDEL2 - nfacets:%u\n", x->ntiles);
  for(unsigned f=0; f < x->ntiles; f++){
    printf("facet %u - sites:\n", f);
    for(unsigned i=0; i < dim+1; i++){
      printf("%u - ", x->tiles[f].simplex.sitesids[i]);
    }
    printf("\n");
    printf("facet %u - ridges:\n", f);
    for(unsigned i=0; i < dim+1; i++){
      printf("%u - ", x->tiles[f].ridgesids[i]);
    }
    printf("\n");
    printf("facet %u - neighbors:\n", f);
    for(unsigned i=0; i < x->tiles[f].nneighbors; i++){
      printf("%u - ", x->tiles[f].neighbors[i]);
    }
    printf("\n");
  }
  printf("nallridges:%u\n", x->nsubtiles);
  for(unsigned r=0; r < x->nsubtiles; r++){
    printf("ridge %u - id %u:\n", r, x->subtiles[r].id);
    for(unsigned i=0; i < dim; i++){
      printf("%u - ", x->subtiles[r].simplex.sitesids[i]);
    }
    printf("ridgeOf: %u %d", x->subtiles[r].ridgeOf1, x->subtiles[r].ridgeOf2);
    printf("\n");
  }
  free(x);
}