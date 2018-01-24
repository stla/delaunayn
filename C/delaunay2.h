typedef struct Site {
  unsigned   id;
  unsigned*  neighsites;
  unsigned   nneighsites;
  unsigned*  neighridgesids;
  unsigned   nneighridges;
  unsigned*  neightiles;
  unsigned   nneightiles;
} SiteT;

typedef struct Simplex {
  unsigned* sitesids;
  double*   center;
  double*   normal;
  double    offset;
  double    volume;
} SimplexT;

typedef struct SubTile {
  unsigned id;
  SimplexT simplex;
  unsigned ridgeOf1;
  int      ridgeOf2;
  unsigned flag;
} SubTileT;

#define INIT_SUBTILE(X) SubTileT X = {.flag = 0}

typedef struct Tile {
  unsigned  id;
  SimplexT  simplex;
  unsigned* neighbors;
  unsigned  nneighbors;
  unsigned* ridgesids;
  unsigned  nridges; //  = dim+1
  int       family;
  int       orientation;
} TileT;

typedef struct Tesselation {
  SiteT*    sites;
  TileT*    tiles;
  unsigned  ntiles;
  SubTileT* subtiles;
  unsigned  nsubtiles;
} TesselationT;

TesselationT* tesselation(double*, unsigned, unsigned, unsigned, unsigned*);
void testdel2();
