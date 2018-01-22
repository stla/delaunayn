typedef struct Site {
  unsigned  id;
  unsigned* neighsites;
  unsigned* nneighsites;
  unsigned* neighridges;
  unsigned* nneighridges;
  unsigned* neightiles;
  unsigned* nneightiles;
} SiteT;

typedef struct Simplex {
  unsigned* verticesids;
  double*   center;
  double*   normal;
  double    offset;
  double    volume;
  int       orientation;
} SimplexT;

typedef struct SubTile {
  unsigned id;
  SimplexT simplex;
  unsigned ridgeOf1;
  unsigned ridgeOf2;
  unsigned flag;
} SubTileT;

#define INIT_SUBTILE(X) SubTileT X = {.flag = 0}

typedef struct Tile {
  unsigned  id;
  SimplexT  simplex;
  unsigned* neighbors;
  unsigned  nneighbors;
  unsigned* ridgesids;
  unsigned  nridges;
  int       family;
} TileT;

typedef struct Tesselation {
  SiteT*    sites;
  TileT*    tiles;
//  unsigned* tilesids;
  unsigned  ntiles;
  SubTile*  subtiles;
//  unsigned* subtilesids;
  unsigned  nsubtiles;
} TesselationT;
