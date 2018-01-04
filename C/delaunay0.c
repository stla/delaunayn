// C:\Users\sdl96354\AppData\Local\Programs\HaskellPlatform\8.2.1\mingw\bin\gcc -I"." -L"." -o MYFILE libqhull_r.c geom_r.c geom2_r.c global_r.c io_r.c mem_r.c poly_r.c poly2_r.c qset_r.c random_r.c usermem_r.c merge_r.c userprintf_r.c user_r.c stat_r.c MYFILE2.c -std=c99
#define qh_QHimport
#include "qhull_a.h"
#include "result.h"
//#include <unistd.h>              /* For unlink() */

int* delaunay(double* vertices, unsigned dim, unsigned n, int* nf, char* tmpfile)
{
	//int i;
//	boolT ismalloc;
	char flags[250];             /* option flags for qhull, see qh_opt.htm */
  sprintf(flags,"qhull d Qbb T0 Fn %s", ""); // options here
  int* tri;
  FILE *errfile = NULL;

  /* Check input matrix */
// 	dim = ncols(p);
// 	n   = nrows(p);
// 	if(dim <= 0 || n <= 0){
// 		error("Invalid input matrix.");
// 	}
//   if (n <= dim) {
//     error("Number of points is not greater than the number of dimensions.");
//   }

	if (n > dim + 1) { // quid si n = dim+1 ? c'est dans le else (vide)
		int i, j;
		int exitcode;
		int curlong, totlong;
		boolT ismalloc = False;   /* True if qhull should free points in qh_freeqhull() or reallocation */
    FILE* tmpstdout = fopen(tmpfile, "w");
		exitcode = qh_new_qhull(dim, n, vertices, ismalloc, flags, tmpstdout, errfile);
    fclose(tmpstdout);
    // unlink(name);
    // free((char *) name);
		if (!exitcode) {                    /* 0 if no error from qhull */
			facetT *facet;                  /* set by FORALLfacets */
			vertexT *vertex, **vertexp;
      facetT *neighbor, **neighborp;
      /* Count the number of facets so we know how much space to allocate */
			nf[0]=0;                 /* Number of facets */
			FORALLfacets {
				if (!facet->upperdelaunay) {
          nf[0]++;
        }
        /* Double check. Non-simplicial facets will cause segfault below */
        if (! facet->simplicial) {
//          error ("Qhull returned non-simplicial facets -- try delaunayn with different options");
          exitcode = 1;
          break;
        }
			}
      /* Alocate the space */
      tri = (int*) malloc(nf[0] * (dim+1) * sizeof(int));
    //   PROTECT(neighbours = allocVector(VECSXP, nf));
    //   PROTECT(areas = allocVector(REALSXP, nf));
      /* Iterate through facets to extract information */
      i=0;
	    FORALLfacets {
			  if (!facet->upperdelaunay) {
          // if (i >= nf[0]) {
          //   error("Trying to access non-existent facet %i", i);
          // }
          /* Triangulation */
		      j=0;
		      FOREACHvertex_ (facet->vertices) {
          //  if ((i + nf*j) >= nf*(dim+1))
          //    error("Trying to write to non-existent area of memory i=%i, j=%i, nf=%i, dim=%i", i, j, nf, dim);
            tri[i*(dim+1)+j] = 1 + qh_pointid(vertex->point); // virer le 1
            j++;
					}
          /* Neighbours */
        //   PROTECT(neighbour = allocVector(INTSXP, qh_setsize(facet->neighbors)));
        //   j=0;
        //   FOREACHneighbor_(facet) {
        //     INTEGER(neighbour)[j] = neighbor->visitid ? neighbor->visitid: 0 - neighbor->id;
        //     j++;
        //   }
        //   SET_VECTOR_ELT(neighbours, i, neighbour);
        //   UNPROTECT(1);

          /* Area. Code modified from qh_getarea() in libquhull/geom2.c */
        //   if ((facet->normal) && !(facet->upperdelaunay && qh ATinfinity)) {
        //     if (!facet->isarea) {
        //       facet->f.area= qh_facetarea(facet);
        //       facet->isarea= True;
        //     }
        //     REAL(areas)[i] = facet->f.area;
        //   }

					i++;
				}
			}
		}
    /* Do cleanup regardless of whether there is an error */
		qh_freeqhull(!qh_ALL);                  /* free long memory */
		qh_memfreeshort (&curlong, &totlong);   /* free short memory and memory allocator */
		// if (curlong || totlong) {
		// 	warning("delaunay: did not free %d bytes of long memory (%d pieces)", totlong, curlong);
		// }
    // if (exitcode) {
		// 	error("Received error code %d from qhull.", exitcode);
		// }
	} else if (n == dim + 1) {
    /* Number of points is one more than the number of dimensions. */
		/* FIXME. Need to check if nx points span a simplex. */
    // PROTECT(tri = allocMatrix(INTSXP, 1, dim+1));
	// 	for (int i=0; i<n; i++) {
	// 		INTEGER(tri)[i] = i + 1;
	// 	}
	// 	UNPROTECT(1);
	}
//   PROTECT(retlist = allocVector(VECSXP, retlen));
//   PROTECT(retnames = allocVector(VECSXP, retlen));
//   SET_VECTOR_ELT(retlist, 0, tri);
//   SET_VECTOR_ELT(retnames, 0, mkChar("tri"));
//   SET_VECTOR_ELT(retlist, 1, neighbours);
//   SET_VECTOR_ELT(retnames, 1, mkChar("neighbours"));
//   SET_VECTOR_ELT(retlist, 2, areas);
//   SET_VECTOR_ELT(retnames, 2, mkChar("areas"));
//   setAttrib(retlist, R_NamesSymbol, retnames);
//   UNPROTECT(2);
// 	return retlist;
    return tri;
}

struct Result* test(double* vertices, unsigned dim, unsigned n, int* nf, char* tmpfile)
{
  int* tri = delaunay(vertices, dim, n, nf, tmpfile);
  struct Result* out = malloc (sizeof(ResultT));
  out->dim = (int)(dim);
  out->length = nf[0];
//  out->indices = malloc (nf[0]*(dim+1)*sizeof(int));
  out->indices = tri;
//  free(tri);
  // struct Result out;
  // out.dim = (int)(dim);
  // out.length = nf[0];
  // out.indices = tri;
  return out;
}
