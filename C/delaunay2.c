#define qh_QHimport
#include "qhull_ra.h"
#include "delaunay2.h"
#include "utils.h"
#include <math.h> /* to use NAN */

// void printfacet(qhT* qh, facetT* facet){
//   vertexT *vertex, **vertexp;
//   FOREACHvertex_(facet->vertices){
//     printf("facetid: %d, pointid: %d ", facet->id, qh_pointid(qh, vertex->point));
//   }
// }

unsigned facetOK_(facetT* facet, unsigned degenerate){
  return !facet->upperdelaunay && (degenerate || !facet->degenerate);
} // && simplicial, && !facet->redundant - pas de simplicial avec Qt

// // to use the qsort function - sort sites according to their ids
// int cmpsites (const void * a, const void * b) {
//    return ( (*((SiteT*)a)).id - (*((SiteT*)b)).id );
// }


TesselationT* tesselation(
	double*   sites,
	unsigned  dim,
	unsigned  n,
  unsigned  degenerate,
	unsigned* exitcode
)
{
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags, "qhull d Qt Fn Qbb", "");
	qhT qh_qh;                /* Qhull's data structure.  First argument of most calls */
  qhT *qh= &qh_qh;
  QHULL_LIB_CHECK
  qh_meminit(qh, stderr);
	boolT ismalloc  = False; /* True if qhull should free points in qh_freeqhull() or reallocation */
	FILE *errfile   = NULL;
  FILE* outfile   = NULL;
  qh_zero(qh, errfile);
	exitcode[0] = qh_new_qhull(qh, dim, n, sites, ismalloc, flags, outfile,
		                         errfile);
  //fclose(tmpstdout);
  printf("exitcode: %u\n", exitcode[0]);

  TesselationT* out = malloc(sizeof(TesselationT)); /* output */

	if (!exitcode[0]) { /* 0 if no error from qhull */

    /* Count the number of facets so we know how much space to allocate */
		unsigned nfacets = 0; /* to store the number of facets */
    {
      facetT *facet;  /* set by FORALLfacets */
  		FORALLfacets {
  			if(facetOK_(facet, degenerate)){
          facet->id = nfacets;
  	      nfacets++;
        }else{
  				qh_removefacet(qh, facet);
  			}
  		}
    }

    /* Initialize the tiles */
    TileT* allfacets = malloc(nfacets * sizeof(TileT));
  	{ /* facets ids, orientations, centers, normals, offsets, sites ids, neighbors, families  */
      facetT* facet;
      unsigned i_facet = 0; /* facet counter */
      FORALLfacets {
        // ceci fait planter: fais getarea après le test tricoplanar
//        allfacets[i_facet].simplex.volume = qh_facetarea(qh, facet);
        allfacets[i_facet].id             = facet->id;
        allfacets[i_facet].orientation    = facet->toporient ? 1 : -1;
        allfacets[i_facet].simplex.center =
          facet->degenerate ? nanvector(dim)
                              : qh_facetcenter(qh, facet->vertices);
        allfacets[i_facet].simplex.normal = facet->normal;
        allfacets[i_facet].simplex.offset = facet->offset;
        /* calculate circumradius */
        if(!facet->degenerate){
          pointT* point = ((vertexT*)facet->vertices->e[0].p)->point;
          allfacets[i_facet].simplex.radius =
            sqrt(squaredDistance(point, allfacets[i_facet].simplex.center,
                                 dim));
        }else{
          allfacets[i_facet].simplex.radius = NAN;
        }


        { /* vertices ids of the facet */
          allfacets[i_facet].simplex.sitesids =
            malloc((dim+1) * sizeof(unsigned));
          vertexT *vertex, **vertexp;
          unsigned i_vertex = 0;
          FOREACHvertex_(facet->vertices) {
            allfacets[i_facet].simplex.sitesids[i_vertex] =
              qh_pointid(qh, vertex->point);
            i_vertex++;
    			}
          qsortu(allfacets[i_facet].simplex.sitesids, dim+1);
//          qsort(allfacets[i_facet].simplex.sitesids, dim+1, sizeof(unsigned), cmpfunc);
        }

        { /* neighbors facets of the facet */
          facetT *neighbor, **neighborp;
    			unsigned flag[dim+1];
          allfacets[i_facet].nneighbors = 0;
          unsigned i_neighbor = 0;
    			FOREACHneighbor_(facet) {
            //flag[i_neighbor] = facetOK_(neighbor, degenerate);
            if(flag[i_neighbor] = facetOK_(neighbor, degenerate)){
              allfacets[i_facet].nneighbors++;
            }
            i_neighbor++;
          }
          allfacets[i_facet].neighbors =
            malloc(allfacets[i_facet].nneighbors * sizeof(unsigned));
          unsigned countok = 0;
          i_neighbor = 0;
          FOREACHneighbor_(facet) {
            if(flag[i_neighbor]){
              allfacets[i_facet].neighbors[countok] = neighbor->id;
              countok++;
            }
            /**/
            i_neighbor++;
          }
        }

        /* facet family */
        if(facet->tricoplanar){
          allfacets[i_facet].family = facet->f.triowner->id;
        }else{
          allfacets[i_facet].family = -1;
        }

        /**/
  			i_facet++;
  		}
    }

		/* neighbor facets and neighbor vertices per vertex */
    /* --- we will use the following combinations, also used later */
    unsigned combinations[dim+1][dim];
    for(unsigned m=0; m < dim+1; m++){
      unsigned kk=0;
      for(unsigned k=0; k < dim+1; k++){
        if(k != m){
          combinations[m][kk] = k;
          kk++;
        }
      }
    }
    /* --- initialize all sites */
    SiteT* allsites = malloc(n * sizeof(SiteT));
    /* --- array to flag neighbors - 0/1 if not neighbour/neighbour */
    unsigned** verticesFacetsNeighbours = malloc(n * sizeof(unsigned*));
//		unsigned verticesFacetsNeighbours[n][nfacets]; stackoverflow !
    for(unsigned v=0; v < n; v++){
      allsites[v].id          = v;
      allsites[v].nneighsites = 0;
      allsites[v].neighsites  = malloc(0);
      allsites[v].nneightiles = 0;
      verticesFacetsNeighbours[v] = uzeros(nfacets);
    }

    /* --- fill verticesFacetsNeighbours, derive number of neighbor facets */
    /* --- and derive neighbor sites */
    for(unsigned i_facet=0; i_facet < nfacets; i_facet++){
      for(unsigned j=0; j < dim+1; j++){
        unsigned vertexid = allfacets[i_facet].simplex.sitesids[j];
        if(verticesFacetsNeighbours[vertexid][i_facet] == 0){
          verticesFacetsNeighbours[vertexid][i_facet] = 1;
          allsites[vertexid].nneightiles++;
        }
        for(unsigned k=0; k < dim; k++){
          unsigned vertexid2 =
            allfacets[i_facet].simplex.sitesids[combinations[j][k]];
          unsigned pushed;
          appendu(vertexid2, &allsites[vertexid].neighsites,
                  allsites[vertexid].nneighsites, &pushed);
          if(pushed){
            allsites[vertexid].nneighsites++;
          }
        }
      }
    }

    /************************************************************/
    /* second pass on facets: ridges and facet volumes          */
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
      unsigned i_ridge_dup = 0; /* ridge counter */
      unsigned i_facet = 0; /* facet counter */
      FORALLfacets {

        allfacets[i_facet].simplex.volume = facet->f.area;
        allfacets[i_facet].nridges   = dim+1;
        allfacets[i_facet].ridgesids = malloc((dim+1) * sizeof(unsigned));

        /* loop on the combinations - it increments i_ridge_dup */
        for(unsigned m=0; m < dim+1; m++){
          allridges_dup[i_ridge_dup].ridgeOf1 = facet->id;
          allridges_dup[i_ridge_dup].ridgeOf2 = -1; /* this means "nothing" */
          unsigned ids[dim];
          for(unsigned i=0; i < dim; i++){
            ids[i] = allfacets[i_facet].simplex.sitesids[combinations[m][i]];
          }
          unsigned done = 0; /* flag ridge is already done */
          for(unsigned r=0; r < i_ridge_dup; r++){
            if(allridges_dup[r].ridgeOf2 == (int) facet->id &&
               allridges_dup[r].flag==1)
            {
              unsigned ids2[dim];
              unsigned i;
              for(i=0; i < dim; i++){
                ids2[i] = allridges_dup[r].simplex.sitesids[i];
                if(ids2[i] != ids[i]){
                  break;
                }
              }
              if(i == dim){
                allfacets[i_facet].ridgesids[m] = allridges_dup[r].id;
                done = 1;
                break;
              }
            }
          }
          if(done == 0){ /* do ridge */
            allridges_dup[i_ridge_dup].flag = 1;
            allridges_dup[i_ridge_dup].id   = n_ridges;
            allfacets[i_facet].ridgesids[m] = n_ridges;
            n_ridges++;
            for(unsigned i=0; i < dim; i++){
              allridges_dup[i_ridge_dup].simplex.sitesids[i] = ids[i];
              allsites[ids[i]].nneighridges++;
            }

            { /* loop on facet neighbors to derive ridgeOf2 */
              facetT *neighbor, **neighborp;
              FOREACHneighbor_(facet){
                unsigned fnid = neighbor->id;
                if(facetOK_(neighbor, degenerate)){
                  unsigned ok;
                  for(unsigned mm=0; mm < dim+1; mm++){
                    ok = 0;
                    for(unsigned i=0; i < dim; i++){
                      if(allfacets[fnid].simplex.sitesids[combinations[mm][i]]
                          != ids[i])
                      {
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
                    allridges_dup[i_ridge_dup].ridgeOf2 = (int) fnid;
                    break;
                  }
                }
              } /* end FOREACHneighbor_(facet) */
            }

            pointT* points[dim]; /* the points corresponding to the combination */
            for(unsigned i=0; i < dim; i++){
              points[i] = getpoint(sites, dim, ids[i]);
            }
            double normal[dim]; /* to store the ridge normal */
            if(dim == 2){
              double u1 = points[1][0] - points[0][0];
              double v1 = points[1][1] - points[0][1];
              allridges_dup[i_ridge_dup].simplex.volume =
                sqrt(square(u1)+square(v1));
              allridges_dup[i_ridge_dup].simplex.center =
                middle(points[0], points[1], dim);
              allridges_dup[i_ridge_dup].simplex.radius =
                sqrt(squaredDistance(allridges_dup[i_ridge_dup].simplex.center,
                                     points[0], dim));
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
            if(dim > 2){ /* ridge center is already done if dim 2 */
              if(facet->degenerate){
                allridges_dup[i_ridge_dup].simplex.center = nanvector(dim);
                allridges_dup[i_ridge_dup].simplex.radius = NAN;
              }else{
                allridges_dup[i_ridge_dup].simplex.center =
                  malloc(dim * sizeof(double));
                double scal = 0;
                for(unsigned i=0; i < dim; i++){
                  scal += (points[0][i]-allfacets[i_facet].simplex.center[i]) *
                            normal[i];
                }
                for(unsigned i=0; i < dim; i++){
                  allridges_dup[i_ridge_dup].simplex.center[i] =
                    allfacets[i_facet].simplex.center[i] + scal*normal[i];
                }
                allridges_dup[i_ridge_dup].simplex.radius =
                  sqrt(squaredDistance(
                        allridges_dup[i_ridge_dup].simplex.center,
                        points[0], dim));
              }
            }
            /* orient the normal (used for unbounded Voronoi cells) */
            if(allridges_dup[i_ridge_dup].ridgeOf2 == -1 &&
               (!facet->degenerate || dim==2))
            {
              pointT* otherpoint = /* the vertex of the facet not in the ridge */
                getpoint(sites, allfacets[facet->id].simplex.sitesids[m], dim);
              double thepoint[dim]; /* the point center+normal */
              for(unsigned i=0; i < dim; i++){
                thepoint[i] = allridges_dup[i_ridge_dup].simplex.center[i] +
                              allridges_dup[i_ridge_dup].simplex.normal[i];
              }
              /* we check that these two points are on the same side of the ridge */
              double h1 = dotproduct(otherpoint,
                                     allridges_dup[i_ridge_dup].simplex.normal,
                                     dim) +
                          allridges_dup[i_ridge_dup].simplex.offset;
              double h2 = dotproduct(thepoint,
                                     allridges_dup[i_ridge_dup].simplex.normal,
                                     dim) +
                          allridges_dup[i_ridge_dup].simplex.offset;
              if(h1*h2 > 0){
                for(unsigned i=0; i < dim; i++){
                  allridges_dup[i_ridge_dup].simplex.normal[i] *= -1;
                }
              }
            }
            for(unsigned i=0; i < dim; i++){
              free(points[i]);
            }
          }
          i_ridge_dup++;
        } // end loop combinations (m)
        qsortu(allfacets[i_facet].ridgesids, dim+1);
//        qsort(allfacets[i_facet].ridgesids, dim+1, sizeof(unsigned), cmpfunc);
        i_facet++;
      } // end FORALLfacets
    }

    /* make unique ridges */
    SubTileT* allridges = malloc(n_ridges * sizeof(SubTileT));
    unsigned inc_ridge = 0;
		for(unsigned l=0; l < n_ridges_dup; l++){
      if(allridges_dup[l].flag){
        allridges[inc_ridge] = allridges_dup[l];
        inc_ridge++;
      }
		}

    /* make neighbor ridges per vertex */
    unsigned* i_ridges_per_vertex = uzeros(n);
		for(unsigned v=0; v < n; v++){
      allsites[v].neighridgesids =
        malloc(allsites[v].nneighridges * sizeof(unsigned));
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

    /* order vertices neighbor sites and make neighbor tiles per vertex */
		for(unsigned v=0; v < n; v++){
      qsortu(allsites[v].neighsites, allsites[v].nneighsites);
      allsites[v].neightiles =
        malloc(allsites[v].nneightiles * sizeof(unsigned));
			unsigned inc_facet = 0; unsigned inc_vfn = 0;
			while(inc_vfn < allsites[v].nneightiles){
				if(verticesFacetsNeighbours[v][inc_facet] == 1){
          allsites[v].neightiles[inc_vfn] = inc_facet;
					inc_vfn++;
				}
				inc_facet++;
			}
		}

// 		for(unsigned v=0; v<n; v++){
//       qsortu(allsites[v].neighsites, allsites[v].nneighsites);
// //      qsort(allsites[v].neighsites, allsites[v].nneighsites, sizeof(unsigned), cmpfunc);
// 		}

	  out->sites      = allsites;
    out->tiles      = allfacets;
	  out->ntiles     = nfacets;
	  out->subtiles   = allridges;
    out->nsubtiles  = n_ridges;

    free(allridges_dup);
    free(i_ridges_per_vertex);
    free(verticesFacetsNeighbours);

	}

	/* Do cleanup regardless of whether there is an error */
  int curlong, totlong;
	qh_freeqhull(qh, !qh_ALL);                  /* free long memory */
	qh_memfreeshort(qh, &curlong, &totlong);   /* free short memory and memory allocator */

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
