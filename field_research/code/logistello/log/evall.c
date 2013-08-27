// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// new tables / 2.97

#include "main.h"
#include "board.h"
#include "eval.h"
#include "move.h"
#include "crt.h"
#include "fpatt.h"
#include "tabtype.h"
#include "shm.h"

#if GCC
#define ASSIGN_REGS 0 // was 1
#else
#define ASSIGN_REGS 0
#endif

#define USE_SHM          true
#define SHM_SIZE         4500000

#define ALTERNATIVE_TABS false

#define RARE_WRITE       false        // save positions with rare configurations
#define RARE_FILE        "rare2.sfk"

static char *pattern_files[] = {
#if !ALTERNATIVE_TABS
  "patt_EDGE+2X.bin.A",
  "patt_2x5.bin.A",
  "patt_3x3.bin.A",
  "patt_HV2.bin.A",
  "patt_HV3.bin.A",
  "patt_HV4.bin.A",
  "patt_D8.bin.A",
  "patt_D7.bin.A",
  "patt_D6.bin.A",
  "patt_D5.bin.A",
  "patt_D4.bin.A"
#else
  "patt_EDGE+2X.newbin",
  "patt_2x5.newbin",
  "patt_3x3.newbin",
  "patt_HV2.newbin",
  "patt_HV3.newbin",
  "patt_HV4.newbin",
  "patt_D8.newbin",
  "patt_D7.newbin",
  "patt_D6.newbin",
  "patt_D5.newbin",
  "patt_D4.newbin"
#endif

};

const int PATT_NUM = sizeof(pattern_files)/sizeof(pattern_files[0]);

static TTYPE *pattern_tabs[PATT_NUM][64];

static char *feature_files[] = {
#if !ALTERNATIVE_TABS
  "feat_PARITY.bin.A"
#else
  "feat_PARITY.newbin"
#endif
};

const int FEAT_NUM = sizeof(feature_files)/sizeof(feature_files[0]);

static TTYPE *feature_tabs[FEAT_NUM][64];


#if RARE_WRITE

static char *pattern_freq_files[] = {
  "patt_EDGE+2X.freq",
  "patt_2x5.freq",
  "patt_3x3.freq",
  "patt_HV2.freq",
  "patt_HV3.freq",
  "patt_HV4.freq",
  "patt_D8.freq",
  "patt_D7.freq",
  "patt_D6.freq",
  "patt_D5.freq",
  "patt_D4.freq"
};

static char *feature_freq_files[] = {
  "feat_PARITY.freq"
};

static TTYPE *pattern_freq_tabs[PATT_NUM][64];
static TTYPE *feature_freq_tabs[FEAT_NUM][64];

#endif


#define P10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])

#define P9(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])

#define P8(p1,p2,p3,p4,p5,p6,p7,p8) \
(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])

#define P7(p1,p2,p3,p4,p5,p6,p7) \
(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])

#define P6(p1,p2,p3,p4,p5,p6) \
(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])

#define P5(p1,p2,p3,p4,p5) \
(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])

#define P4(p1,p2,p3,p4) \
(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])

#define P3(p1,p2,p3) \
(3*(3*p[p1]+p[p2])+p[p3])


static int interval_num = 0;
static int interval_len = 0;
static int disc_min     = 0;


enum { SHM_NO=1, SHM_CREATE, SHM_ACCESS };

static void read_tables(int mode = SHM_NO)
{
  int i, j, k, r;

  FOR (i, PATT_NUM) {
    String name = DataPath; name += "/"; name += pattern_files[i];
    FILE *fp = fopen(name.c_str(), "r");
    if (!fp) { cerr << name << flush; Error("file not found"); }

    cout << name << endl;

    int len = getc(fp);
    if (len < 1 || len > 12) Error("length corrupt");

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_num corrupt");
    if (interval_num && interval_num != r) Error("interval_num different");
    interval_num = r;

    r = fgetc(fp);
    if (r < 4 || r >= 60) Error("disc_min corrupt");
    if (disc_min && disc_min != r) Error("disc_min different");
    disc_min = r;

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_len corrupt");
    if (interval_len && interval_len != r) Error("interval_len different");
    interval_len = r;

    int config_num = Pot3[len];

    FOR (k, interval_num) {

      if (mode == SHM_NO) {

	pattern_tabs[i][k] = (TTYPE*) calloc(sizeof(TTYPE), config_num);

      } else {

	if (mode == SHM_CREATE)

	  pattern_tabs[i][k] = (TTYPE*) shm_alloc(sizeof(TTYPE) * config_num);

	else if (mode == SHM_ACCESS) {

	  pattern_tabs[i][k] = (TTYPE*) shm_next_alloc(sizeof(TTYPE) * config_num);

	} else 

	  Error("illegal SHM mode");
      }

      FOR (j, config_num) {

	short val = fgetc(fp);
	val += fgetc(fp) << 8;

	if (mode != SHM_ACCESS)
	  pattern_tabs[i][k][j] = val;
	else
	  if (i != 1 && pattern_tabs[i][k][j] != val) Error("shm val different!");
      }
      
      pattern_tabs[i][k] += (config_num-1)/2;
    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);
  }

  FOR (i, FEAT_NUM) {
    String name = DataPath;
    name += "/"; name += feature_files[i];
    FILE *fp = fopen(name.c_str(), "r");
    if (!fp)  { cerr << name << flush; Error("file not found"); }

    cout << name << endl;

    int len = getc(fp);
    if (len < 1) Error("length corrupt");

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_num corrupt");
    if (interval_num && interval_num != r) Error("interval_num different");
    interval_num = r;

    r = fgetc(fp);
    if (r < 4 || r >= 60) Error("disc_min corrupt");
    if (disc_min && disc_min != r) Error("disc_min different");
    disc_min = r;

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_len corrupt");
    if (interval_len && interval_len != r) Error("interval_len different");
    interval_len = r;

    int config_num = len;

    FOR (k, interval_num) {

      if (mode == SHM_NO)

	feature_tabs[i][k] = (TTYPE*) calloc(sizeof(TTYPE), config_num);

      else {

	if (mode == SHM_CREATE)

	  feature_tabs[i][k] = (TTYPE*) shm_alloc(sizeof(TTYPE) * config_num);

	else if (mode == SHM_ACCESS) {

	  feature_tabs[i][k] = (TTYPE*) shm_next_alloc(sizeof(TTYPE) * config_num);

	} else 

	  Error("illegal SHM mode");
      }

      FOR (j, config_num) {

	short val = fgetc(fp);
	val += fgetc(fp) << 8;

	if (mode != SHM_ACCESS)
	  feature_tabs[i][k][j] = val;
	else
	  if (feature_tabs[i][k][j] != val) Error("shm val different!");
      }
    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);
  }
}

#if RARE_WRITE

static void read_freq_tables()
{
  int i, j, k, r;

  FOR (i, PATT_NUM) {
    char name[2*DATEI_MAX];
    sprintf(name, "%s/%s", DataPath, pattern_freq_files[i]);
    FILE *fp = fopen(name, "r");
    if (!fp) { fprintf(stderr, name); Error("file not found"); }

    cout << name << endl;

    int len = getc(fp);
    if (len < 1 || len > 12) Error("length corrupt");

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_num corrupt");
    if (interval_num && interval_num != r) Error("interval_num different");
    interval_num = r;

    r = fgetc(fp);
    if (r < 4 || r >= 60) Error("disc_min corrupt");
    if (disc_min && disc_min != r) Error("disc_min different");
    disc_min = r;

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_len corrupt");
    if (interval_len && interval_len != r) Error("interval_len different");
    interval_len = r;

    int config_num = Pot3[len];

    FOR (k, interval_num) {

      pattern_freq_tabs[i][k] = (TTYPE*) calloc(sizeof(TTYPE), config_num);
      
      FOR (j, config_num) {

	short val = fgetc(fp);
	val += fgetc(fp) << 8;

	pattern_freq_tabs[i][k][j] = val;

      }
      
      pattern_freq_tabs[i][k] += (config_num-1)/2;

    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);
  }


  FOR (i, FEAT_NUM) {
    char name[2*DATEI_MAX];
    sprintf(name, "%s/%s", DataPath, feature_freq_files[i]);
    FILE *fp = fopen(name, "r");
    if (!fp)  { fprintf(stderr, name); Error("file not found"); }

    cout << name << endl;

    int len = getc(fp);
    if (len < 1) Error("length corrupt");

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_num corrupt");
    if (interval_num && interval_num != r) Error("interval_num different");
    interval_num = r;

    r = fgetc(fp);
    if (r < 4 || r >= 60) Error("disc_min corrupt");
    if (disc_min && disc_min != r) Error("disc_min different");
    disc_min = r;

    r = fgetc(fp);
    if (r < 1 || r > 60) Error("interval_len corrupt");
    if (interval_len && interval_len != r) Error("interval_len different");
    interval_len = r;

    int config_num = len;

    FOR (k, interval_num) {

      feature_freq_tabs[i][k] = (TTYPE*) calloc(sizeof(TTYPE), config_num);

      FOR (j, config_num) {

	short val = fgetc(fp);
	val += fgetc(fp) << 8;

	feature_freq_tabs[i][k][j] = val;
      }
    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);
  }
}

#endif

const int PERM_INDEX = 1;
const int PERM_LEN   = 10;
const int PERM_LEN_POT_3 = 59049;
const int perm[PERM_LEN] = { 0, 1, 2, 3, 8, 4, 5, 6, 7, 9 };

#if 1

// new code: save memory by using temp array

static void permute_2x5()
{
  int i, j, k;
  const int config_num = PERM_LEN_POT_3; //Pot3[PERM_LEN];
  TTYPE temp[config_num];

  cout << "permute 2x5 ..." << flush;

  FOR (k, interval_num) {

    // create permuation in temp[]

    TTYPE *p = pattern_tabs[PERM_INDEX][k] - (config_num-1)/2;

    FOR (i, config_num) {

      int n = i, cont[PERM_LEN];

      FOR (j, PERM_LEN) {
	cont[PERM_LEN - 1 - j] = n % 3;
	n /= 3;
      }

      n = 0;
      FOR (j, PERM_LEN) n = 3*n + cont[perm[j]];

      temp[i] = p[n];
    }

    // copy temp[] to original array

    FOR (i, config_num) p[i] = temp[i];
  }

  cout << "OK" << endl;
}


#else 

// old code: create new array

static void permute_2x5()
{
  int i, j, k;

  int config_num = Pot3[PERM_LEN];
  
  cout << "permute 2x5 ..." << flush;
  FOR (k, interval_num) {

    TTYPE *new_tab = (TTYPE*) calloc(sizeof(TTYPE), config_num);
    pattern_tabs[PERM_INDEX][k] -= (config_num-1)/2;
    FOR (i, config_num) {

      int n = i, cont[PERM_LEN];

      FOR (j, PERM_LEN) {
	cont[PERM_LEN - 1 - j] = n % 3;
	n /= 3;
      }

      n = 0;
      FOR (j, PERM_LEN) n = 3*n + cont[perm[j]];
      
      new_tab[i] = pattern_tabs[PERM_INDEX][k][n];
    }

    free(pattern_tabs[PERM_INDEX][k]);
    pattern_tabs[PERM_INDEX][k] = new_tab + (config_num-1)/2;;
  }

  cout << "OK" << endl;
}

#endif

#if RARE_WRITE

void permute_freq_2x5()
{
  int i, j, k;
  
  int config_num = Pot3[PERM_LEN];

  cout << "permute freq 2x5 ..." << flush;
  FOR (k, interval_num) {

    TTYPE *new_tab = (TTYPE*) calloc(sizeof(TTYPE), config_num);
    pattern_freq_tabs[PERM_INDEX][k] -= (config_num-1)/2;
    FOR (i, config_num) {

      int n = i, cont[PERM_LEN];

      FOR (j, PERM_LEN) {
	cont[PERM_LEN - 1 - j] = n % 3;
	n /= 3;
      }

      n = 0;
      FOR (j, PERM_LEN) n = 3*n + cont[perm[j]];
      
      new_tab[i] = pattern_freq_tabs[PERM_INDEX][k][n];
    }

    free(pattern_freq_tabs[PERM_INDEX][k]);
    pattern_freq_tabs[PERM_INDEX][k] = new_tab + (config_num-1)/2;;
  }

  cout << "OK" << endl;
}

#endif


/**************************************************************/



static inline int patt_val(TTYPE *pt, int player, int index)
{
  if (player > 0) return pt[index]; else return pt[-index];
}

static inline int patt_val_black(TTYPE *pt, int index) { return pt[ index]; }
static inline int patt_val_white(TTYPE *pt, int index) { return pt[-index]; }


static void show_config_values(BRETT *pb, sint4 inter)
{
  register TTYPE *pt;
  register Square *p=pb->p;
  register STRAHLTYP *pat = pb->NewPatt.st;

  SPFELD sf;
  BrettSf(pb, &sf);

  SfAus(&sf, 0, 0);
    
  pt = pattern_tabs[2][inter];
  cout << P9(A1,B1,C1,A2,B2,C2,A3,B3,C3) << " "
       << patt_val_black(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3)) << " "
       << patt_val_white(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3)) << endl;
  cout << P9(H1,G1,F1,H2,G2,F2,H3,G3,F3) << " "
       << patt_val_black(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3)) << " "
       << patt_val_white(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3)) << endl;
  cout << P9(A8,B8,C8,A7,B7,C7,A6,B6,C6) << " "
       << patt_val_black(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6)) << " "
       << patt_val_white(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6)) << endl;
  cout << P9(H8,G8,F8,H7,G7,F7,H6,G6,F6) << " "
       << patt_val_black(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6)) << " "
       << patt_val_white(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6)) << endl;

  cout << endl;
  pt = pattern_tabs[1][inter];

  cout << P10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2) << " "
       << patt_val(pt, BLACK, P10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)) << " "
       << patt_val(pt, WHITE, P10(A1,B1,C1,D1,A2,B2,C2,D2,E1,E2)) << endl;
  cout << P10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2) << " "
       << patt_val(pt, BLACK, P10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)) << " "
       << patt_val(pt, WHITE, P10(H1,G1,F1,E1,H2,G2,F2,E2,D1,D2)) << endl;
  cout << P10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7) << " "
       << patt_val(pt, BLACK, P10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)) << " "
       << patt_val(pt, WHITE, P10(A8,B8,C8,D8,A7,B7,C7,D7,E8,E7)) << endl;
  cout << P10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7) << " "
       << patt_val(pt, BLACK, P10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)) << " "
       << patt_val(pt, WHITE, P10(H8,G8,F8,E8,H7,G7,F7,E7,D8,D7)) << endl;
  cout << P10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5) << " "
       << patt_val(pt, BLACK, P10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)) << " "
       << patt_val(pt, WHITE, P10(A1,A2,A3,A4,B1,B2,B3,B4,A5,B5)) << endl;
  cout << P10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5) << " "
       << patt_val(pt, BLACK, P10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)) << " "
       << patt_val(pt, WHITE, P10(H1,H2,H3,H4,G1,G2,G3,G4,H5,G5)) << endl;
  cout << P10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4) << " "
       << patt_val(pt, BLACK, P10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)) << " "
       << patt_val(pt, WHITE, P10(A8,A7,A6,A5,B8,B7,B6,B5,A4,B4)) << endl;
  cout << P10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4) << " "
       << patt_val(pt, BLACK, P10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)) << " "
       << patt_val(pt, WHITE, P10(H8,H7,H6,H5,G8,G7,G6,G5,H4,G4)) << endl;

  cout << endl;
  
  pt = pattern_tabs[3][inter];
  cout << pat[PHV2A]/2 << " "
       << patt_val(pt, BLACK, pat[PHV2A]/2) << " "
       << patt_val(pt, WHITE, pat[PHV2A]/2) << endl;
  cout << pat[PHV2B]/2 << " "
       << patt_val(pt, BLACK, pat[PHV2B]/2) << " "
       << patt_val(pt, WHITE, pat[PHV2B]/2) << endl;
  cout << pat[PHV2C]/2 << " "
       << patt_val(pt, BLACK, pat[PHV2C]/2) << " "
       << patt_val(pt, WHITE, pat[PHV2C]/2) << endl;
  cout << pat[PHV2D]/2 << " "
       << patt_val(pt, BLACK, pat[PHV2D]/2) << " "
       << patt_val(pt, WHITE, pat[PHV2D]/2) << endl;
  cout << endl;

  pt = pattern_tabs[4][inter];
  cout << pat[PHV3A]/2 << " "
       << patt_val(pt, BLACK, pat[PHV3A]/2) << " "
       << patt_val(pt, WHITE, pat[PHV3A]/2) << endl;
  cout << pat[PHV3B]/2 << " "
       << patt_val(pt, BLACK, pat[PHV3B]/2) << " "
       << patt_val(pt, WHITE, pat[PHV3B]/2) << endl;
  cout << pat[PHV3C]/2 << " "
       << patt_val(pt, BLACK, pat[PHV3C]/2) << " "
       << patt_val(pt, WHITE, pat[PHV3C]/2) << endl;
  cout << pat[PHV3D]/2 << " "
       << patt_val(pt, BLACK, pat[PHV3D]/2) << " "
       << patt_val(pt, WHITE, pat[PHV3D]/2) << endl;
  cout << endl;

  pt = pattern_tabs[5][inter];
  cout << pat[PHV4A]/2 << " "
       << patt_val(pt, BLACK, pat[PHV4A]/2) << " "
       << patt_val(pt, WHITE, pat[PHV4A]/2) << endl;
  cout << pat[PHV4B]/2 << " "
       << patt_val(pt, BLACK, pat[PHV4B]/2) << " "
       << patt_val(pt, WHITE, pat[PHV4B]/2) << endl;
  cout << pat[PHV4C]/2 << " "
       << patt_val(pt, BLACK, pat[PHV4C]/2) << " "
       << patt_val(pt, WHITE, pat[PHV4C]/2) << endl;
  cout << pat[PHV4D]/2 << " "
       << patt_val(pt, BLACK, pat[PHV4D]/2) << " "
       << patt_val(pt, WHITE, pat[PHV4D]/2) << endl;
  cout << endl;
  
  cout << "-----------------" << endl;
}

WERT EvalLSlow(BRETT *pb, PARTEI player)
{
  static bool tables_read = false;
  int i;


Error("slow not supported");

  // WIPE-OUT?
  
  if ((i=pb->SteinAnz) != 0) 

    if (player == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));

    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }

  if (!tables_read) {
    read_tables();
    tables_read = true;
  }

  register TTYPE *pt;
  register Square *p=pb->p;
  int isu = 0, inter;

  inter = (pb->SteinAnz - disc_min)/interval_len;
  if (inter < 0) inter = 0;
  else if (inter >= interval_num) inter = interval_num-1;

  // Pattern EDGE+2X

  pt = pattern_tabs[0][inter];

  isu +=
    patt_val(pt, player, P10(A1,B1,C1,D1,E1,F1,G1,H1,B2,G2)) +
    patt_val(pt, player, P10(A8,B8,C8,D8,E8,F8,G8,H8,B7,G7)) +
    patt_val(pt, player, P10(A1,A2,A3,A4,A5,A6,A7,A8,B2,B7)) +
    patt_val(pt, player, P10(H1,H2,H3,H4,H5,H6,H7,H8,G2,G7));

  // Pattern 2x5

  pt = pattern_tabs[1][inter];

  // wrong square order!!!
  
  isu +=
    patt_val(pt, player, P10(A1,B1,C1,D1,E1,A2,B2,C2,D2,E2)) +
    patt_val(pt, player, P10(H1,G1,F1,E1,D1,H2,G2,F2,E2,D2)) +
    patt_val(pt, player, P10(A8,B8,C8,D8,E8,A7,B7,C7,D7,E7)) +
    patt_val(pt, player, P10(H8,G8,F8,E8,D8,H7,G7,F7,E7,D7)) +
    patt_val(pt, player, P10(A1,A2,A3,A4,A5,B1,B2,B3,B4,B5)) +
    patt_val(pt, player, P10(H1,H2,H3,H4,H5,G1,G2,G3,G4,G5)) +
    patt_val(pt, player, P10(A8,A7,A6,A5,A4,B8,B7,B6,B5,B4)) +
    patt_val(pt, player, P10(H8,H7,H6,H5,H4,G8,G7,G6,G5,G4));

#if 1
  cout << "val1=" << patt_val(pt, player, P10(A1,B1,C1,D1,E1,A2,B2,C2,D2,E2)) << endl;
  cout << "val2=" << patt_val(pt, player, P10(H1,G1,F1,E1,D1,H2,G2,F2,E2,D2)) << endl;
  cout << "val3=" << patt_val(pt, player, P10(A8,B8,C8,D8,E8,A7,B7,C7,D7,E7)) << endl;
  cout << "val4=" << patt_val(pt, player, P10(H8,G8,F8,E8,D8,H7,G7,F7,E7,D7)) << endl;
  cout << "val5=" << patt_val(pt, player, P10(A1,A2,A3,A4,A5,B1,B2,B3,B4,B5)) << endl;
  cout << "val6=" << patt_val(pt, player, P10(H1,H2,H3,H4,H5,G1,G2,G3,G4,G5)) << endl;
  cout << "val7=" << patt_val(pt, player, P10(A8,A7,A6,A5,A4,B8,B7,B6,B5,B4)) << endl;
  cout << "val8=" << patt_val(pt, player, P10(H8,H7,H6,H5,H4,G8,G7,G6,G5,G4)) << endl;
#endif

  // Pattern 3x3

  pt = pattern_tabs[2][inter];

  isu +=
    patt_val(pt, player, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3)) +
    patt_val(pt, player, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3)) +
    patt_val(pt, player, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6)) +
    patt_val(pt, player, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6));

  // Pattern HV2

  pt = pattern_tabs[3][inter];

  isu +=
    patt_val(pt, player, P8(A2,B2,C2,D2,E2,F2,G2,H2)) +
    patt_val(pt, player, P8(A7,B7,C7,D7,E7,F7,G7,H7)) +
    patt_val(pt, player, P8(B1,B2,B3,B4,B5,B6,B7,B8)) +
    patt_val(pt, player, P8(G1,G2,G3,G4,G5,G6,G7,G8));

  // Pattern HV3				       

  pt = pattern_tabs[4][inter];

  isu += 
    patt_val(pt, player, P8(A3,B3,C3,D3,E3,F3,G3,H3)) +
    patt_val(pt, player, P8(A6,B6,C6,D6,E6,F6,G6,H6)) +
    patt_val(pt, player, P8(C1,C2,C3,C4,C5,C6,C7,C8)) +
    patt_val(pt, player, P8(F1,F2,F3,F4,F5,F6,F7,F8));

  // Pattern HV4				       

  pt = pattern_tabs[5][inter];

  isu += 
    patt_val(pt, player, P8(A4,B4,C4,D4,E4,F4,G4,H4)) +
    patt_val(pt, player, P8(A5,B5,C5,D5,E5,F5,G5,H5)) +
    patt_val(pt, player, P8(D1,D2,D3,D4,D5,D6,D7,D8)) +
    patt_val(pt, player, P8(E1,E2,E3,E4,E5,E6,E7,E8));

  // Pattern D8				       

  pt = pattern_tabs[6][inter];

  isu += 
    patt_val(pt, player, P8(A1,B2,C3,D4,E5,F6,G7,H8)) +
    patt_val(pt, player, P8(H1,G2,F3,E4,D5,C6,B7,A8));

  // Pattern D7

  pt = pattern_tabs[7][inter];

  isu += 
    patt_val(pt, player, P7(A2,B3,C4,D5,E6,F7,G8)) +
    patt_val(pt, player, P7(H2,G3,F4,E5,D6,C7,B8)) +
    patt_val(pt, player, P7(A7,B6,C5,D4,E3,F2,G1)) +
    patt_val(pt, player, P7(H7,G6,F5,E4,D3,C2,B1));

  // Pattern D6

  pt = pattern_tabs[8][inter];

  isu += 
    patt_val(pt, player, P6(A3,B4,C5,D6,E7,F8)) +
    patt_val(pt, player, P6(H3,G4,F5,E6,D7,C8)) +
    patt_val(pt, player, P6(A6,B5,C4,D3,E2,F1)) +
    patt_val(pt, player, P6(H6,G5,F4,E3,D2,C1));

  // Pattern D5

  pt = pattern_tabs[9][inter];

  isu +=
    patt_val(pt, player, P5(A4,B5,C6,D7,E8)) +
    patt_val(pt, player, P5(H4,G5,F6,E7,D8)) +
    patt_val(pt, player, P5(A5,B4,C3,D2,E1)) +
    patt_val(pt, player, P5(H5,G4,F3,E2,D1));

  // Pattern D4

  pt = pattern_tabs[10][inter];

  isu += 
    patt_val(pt, player, P4(A5,B6,C7,D8)) +
    patt_val(pt, player, P4(H5,G6,F7,E8)) +
    patt_val(pt, player, P4(A4,B3,C2,D1)) +
    patt_val(pt, player, P4(H4,G3,F2,E1));

  // PARITY

  isu += feature_tabs[0][inter][pb->SteinAnz & 1]; 
  //  cout << isu << endl;

  isu = int(isu * (0.1 / 512) * WERTFAKTOR);

  if (isu >=  WERTGEWINN) return(+(WERTGEWINN-1)); 
  if (isu <= -WERTGEWINN) return(-(WERTGEWINN-1));  

  return isu;
}


#if DOUBLE_INDEX

#define PTB(i) (*(TTYPE*)((char*)pt + ((sizeof(TTYPE)/2)*(i))))
#define PTW(i) (*(TTYPE*)((char*)pt - ((sizeof(TTYPE)/2)*(i))))

// pt + (2*index)

#else

#define PTB(i) pt[i]
#define PTW(i) pt[-(i)]

#endif


#define PAT1B(n,x) PTB(3*pat[n]+(DOUBLE_INDEX ? 2 : 1)*p[x])
#define PAT1W(n,x) PTW(3*pat[n]+(DOUBLE_INDEX ? 2 : 1)*p[x])

#define PAT2B(n,x1,x2) PTB(9*pat[n]+(DOUBLE_INDEX ? 2 : 1)*(3*p[x1]+p[x2]))
#define PAT2W(n,x1,x2) PTW(9*pat[n]+(DOUBLE_INDEX ? 2 : 1)*(3*p[x1]+p[x2]))

static int stage[65];

// fast

WERT EvalL(BRETT *pb, PARTEI player)
{
  const float FACTOR = (0.1 / 512) * WERTFAKTOR;
  register STRAHLTYP *pat

#ifdef NDEBUG
#if ASSIGN_REGS && __i386__
asm("%edi")
#endif
#endif

    = pb->NewPatt.st;

  register TTYPE *pt 
  
#ifdef NDEBUG
#if ASSIGN_REGS && __i386__
asm("%ebx")
#endif
#endif

  ;

  static bool tables_read = false;

#if RARE_WRITE
  static FILE *fp_rare = 0;
  static int num=0, write_num=0;
#endif

  int i;


  // WIPE-OUT?

  if ((i=pb->SteinAnz) != 0) 

    if (player == BLACK) {

      if (i == -pb->StDiffBW) return(-(WERTGEWINN+64));
 
    } else {

      if (i == +pb->StDiffBW) return(-(WERTGEWINN+64));

    }

  if (!tables_read) {

    tables_read = true;

    if (USE_SHM) {
      
      if (shm_access(SHM_SIZE) >= 0) {

	printf("access shm\n");
	read_tables(SHM_ACCESS);

      } else if (shm_create(SHM_SIZE) >= 0) {

	printf("create shm\n");
	read_tables(SHM_CREATE);
	permute_2x5();
	shm_mark_ready();
	  
      } else goto normal;

    } else {

    normal:;

      read_tables();
      permute_2x5();

    }

#if RARE_WRITE

    read_freq_tables();
    permute_freq_2x5();

    fp_rare = fopen(RARE_FILE, "w");
    if (!fp_rare) Error("can't write to RARE_FILE");

#endif

    // compute stage indices

    int j;

    FOR (j, 65) {
      int s = (j - disc_min)/interval_len;
      if (s < 0) s = 0;
      else if (s >= interval_num) s = interval_num-1;
      stage[j] = s;
    }
  }

  register Square *p
#ifdef NDEBUG
#if ASSIGN_REGS && __i386__
asm("%esi")
#endif
#endif
    =pb->p;

  register int isu 

#ifdef NDEBUG
#if ASSIGN_REGS && __i386__
asm("%edx")
#endif
#endif

    = 0;
    

  int inter = stage[pb->SteinAnz];


#if RARE_WRITE
  int write1=0,write2=0,write3=0,write4=0,write5=0,write6=0,write7=0,write8=0, 
      write9=0,write10=0,write11=0;

#define RARE(x,n) (((x) > 0) << (n))

#endif

  if (player == BLACK) {

    // Pattern EDGE+2X

    pt = pattern_tabs[0][inter];

    isu += PAT2B(PHV1A, B2, G2);
    isu += PAT2B(PHV1B, G2, G7);
    isu += PAT2B(PHV1C, G7, B7);
    isu += PAT2B(PHV1D, B7, B2);

    // Pattern 2x5

    pt = pattern_tabs[1][inter];

    isu += PAT2B(PR1A, E1, E2);
    isu += PAT2B(PR1B, H5, G5);
    isu += PAT2B(PR1C, D8, D7);
    isu += PAT2B(PR1D, A4, B4);
    isu += PAT2B(PR1E, D1, D2);
    isu += PAT2B(PR1F, E8, E7);
    isu += PAT2B(PR1G, A5, B5);
    isu += PAT2B(PR1H, H4, G4);

    // Pattern 3x3

    pt = pattern_tabs[2][inter];

    isu += patt_val_black(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3));
    isu += patt_val_black(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3));
    isu += patt_val_black(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6));
    isu += patt_val_black(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6));

    // Pattern HV2

    pt = pattern_tabs[3][inter];
    isu += PTB(pat[PHV2A]);
    isu += PTB(pat[PHV2B]);
    isu += PTB(pat[PHV2C]);
    isu += PTB(pat[PHV2D]);
    
    // Pattern HV3				       

    pt = pattern_tabs[4][inter];
    isu += PTB(pat[PHV3A]);
    isu += PTB(pat[PHV3B]);
    isu += PTB(pat[PHV3C]);
    isu += PTB(pat[PHV3D]);

    // Pattern HV4				       

    pt = pattern_tabs[5][inter];
    isu += PTB(pat[PHV4A]);
    isu += PTB(pat[PHV4B]);
    isu += PTB(pat[PHV4C]);
    isu += PTB(pat[PHV4D]);

    // Pattern D8				       

    pt = pattern_tabs[6][inter];
    isu += PTB(pat[PD1A]);
    isu += PTB(pat[PD1B]);

    // Pattern D7

    pt = pattern_tabs[7][inter];
    isu += PTB(pat[PD2A]);
    isu += PTB(pat[PD2B]);
    isu += PTB(pat[PD2C]);
    isu += PTB(pat[PD2D]);
  
    // Pattern D6

    pt = pattern_tabs[8][inter];
    isu += PTB(pat[PD3A]);
    isu += PTB(pat[PD3B]);
    isu += PTB(pat[PD3C]);
    isu += PTB(pat[PD3D]);
  
    // Pattern D5

    pt = pattern_tabs[9][inter];
    isu += PTB(pat[PD4A]);
    isu += PTB(pat[PD4B]);
    isu += PTB(pat[PD4C]);
    isu += PTB(pat[PD4D]);

    // Pattern D4

    pt = pattern_tabs[10][inter];

    isu += PTB(pat[PD5A]);
    isu += PTB(pat[PD5B]);
    isu += PTB(pat[PD5C]);
    isu += PTB(pat[PD5D]);


#if RARE_WRITE

    // Pattern EDGE+2X

    pt = pattern_freq_tabs[0][inter];

    write1 += RARE(PAT2B(PHV1A, B2, G2),0);
    write1 += RARE(PAT2B(PHV1B, G2, G7),1);
    write1 += RARE(PAT2B(PHV1C, G7, B7),2);
    write1 += RARE(PAT2B(PHV1D, B7, B2),3);

    // Pattern 2x5

    pt = pattern_freq_tabs[1][inter];

    write2 += RARE(PAT2B(PR1A, E1, E2),0);
    write2 += RARE(PAT2B(PR1B, H5, G5),1);
    write2 += RARE(PAT2B(PR1C, D8, D7),2);
    write2 += RARE(PAT2B(PR1D, A4, B4),3);
    write2 += RARE(PAT2B(PR1E, D1, D2),4);
    write2 += RARE(PAT2B(PR1F, E8, E7),5);
    write2 += RARE(PAT2B(PR1G, A5, B5),6);
    write2 += RARE(PAT2B(PR1H, H4, G4),7);

    // Pattern 3x3

    pt = pattern_freq_tabs[2][inter];

    write3 += RARE(patt_val_black(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3)),0);
    write3 += RARE(patt_val_black(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3)),1);
    write3 += RARE(patt_val_black(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6)),2);
    write3 += RARE(patt_val_black(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6)),3);

    // Pattern HV2

    pt = pattern_freq_tabs[3][inter];

    write4 += RARE(PTB(pat[PHV2A]),0);
    write4 += RARE(PTB(pat[PHV2B]),1);
    write4 += RARE(PTB(pat[PHV2C]),2);
    write4 += RARE(PTB(pat[PHV2D]),3);

    // Pattern HV3				       

    pt = pattern_freq_tabs[4][inter];

    write5 += RARE(PTB(pat[PHV3A]),0);
    write5 += RARE(PTB(pat[PHV3B]),1);
    write5 += RARE(PTB(pat[PHV3C]),2);
    write5 += RARE(PTB(pat[PHV3D]),3);

    // Pattern HV4				       

    pt = pattern_freq_tabs[5][inter];

    write6 += RARE(PTB(pat[PHV4A]),0);
    write6 += RARE(PTB(pat[PHV4B]),1);
    write6 += RARE(PTB(pat[PHV4C]),2);
    write6 += RARE(PTB(pat[PHV4D]),3);

    // Pattern D8				       

    pt = pattern_freq_tabs[6][inter];

    write7 += RARE(PTB(pat[PD1A]),0);
    write7 += RARE(PTB(pat[PD1B]),1);

    // Pattern D7

    pt = pattern_freq_tabs[7][inter];

    write8 += RARE(PTB(pat[PD2A]),0);
    write8 += RARE(PTB(pat[PD2B]),1);
    write8 += RARE(PTB(pat[PD2C]),2);
    write8 += RARE(PTB(pat[PD2D]),3);

    // Pattern D6

    pt = pattern_freq_tabs[8][inter];

    write9 += RARE(PTB(pat[PD3A]),0);
    write9 += RARE(PTB(pat[PD3B]),1);
    write9 += RARE(PTB(pat[PD3C]),2);
    write9 += RARE(PTB(pat[PD3D]),3);

    // Pattern D5

    pt = pattern_freq_tabs[9][inter];

    write10 += RARE(PTB(pat[PD4A]),0);
    write10 += RARE(PTB(pat[PD4B]),1);
    write10 += RARE(PTB(pat[PD4C]),2);
    write10 += RARE(PTB(pat[PD4D]),3);

    // Pattern D4

    pt = pattern_freq_tabs[10][inter];

    write11 += RARE(PTB(pat[PD5A]),0);
    write11 += RARE(PTB(pat[PD5B]),1);
    write11 += RARE(PTB(pat[PD5C]),2);
    write11 += RARE(PTB(pat[PD5D]),3);

#endif

  } else {  // player == WHITE

    // Pattern EDGE+2X

    pt = pattern_tabs[0][inter];

    isu += PAT2W(PHV1A, B2, G2);
    isu += PAT2W(PHV1B, G2, G7);
    isu += PAT2W(PHV1C, G7, B7);
    isu += PAT2W(PHV1D, B7, B2);

    // Pattern 2x5

    pt = pattern_tabs[1][inter];

    isu += PAT2W(PR1A, E1, E2);
    isu += PAT2W(PR1B, H5, G5);
    isu += PAT2W(PR1C, D8, D7);
    isu += PAT2W(PR1D, A4, B4);
    isu += PAT2W(PR1E, D1, D2);
    isu += PAT2W(PR1F, E8, E7);
    isu += PAT2W(PR1G, A5, B5);
    isu += PAT2W(PR1H, H4, G4);

    // Pattern 3x3

    pt = pattern_tabs[2][inter];

    isu += patt_val_white(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3));
    isu += patt_val_white(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3));
    isu += patt_val_white(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6));
    isu += patt_val_white(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6));

    // Pattern HV2

    pt = pattern_tabs[3][inter];
    isu += PTW(pat[PHV2A]);
    isu += PTW(pat[PHV2B]);
    isu += PTW(pat[PHV2C]);
    isu += PTW(pat[PHV2D]);
    
    // Pattern HV3				       

    pt = pattern_tabs[4][inter];
    isu += PTW(pat[PHV3A]);
    isu += PTW(pat[PHV3B]);
    isu += PTW(pat[PHV3C]);
    isu += PTW(pat[PHV3D]);

    // Pattern HV4				       

    pt = pattern_tabs[5][inter];
    isu += PTW(pat[PHV4A]);
    isu += PTW(pat[PHV4B]);
    isu += PTW(pat[PHV4C]);
    isu += PTW(pat[PHV4D]);

    // Pattern D8				       

    pt = pattern_tabs[6][inter];
    isu += PTW(pat[PD1A]);
    isu += PTW(pat[PD1B]);

    // Pattern D7

    pt = pattern_tabs[7][inter];
    isu += PTW(pat[PD2A]);
    isu += PTW(pat[PD2B]);
    isu += PTW(pat[PD2C]);
    isu += PTW(pat[PD2D]);
  
    // Pattern D6

    pt = pattern_tabs[8][inter];
    isu += PTW(pat[PD3A]);
    isu += PTW(pat[PD3B]);
    isu += PTW(pat[PD3C]);
    isu += PTW(pat[PD3D]);
  
    // Pattern D5

    pt = pattern_tabs[9][inter];
    isu += PTW(pat[PD4A]);
    isu += PTW(pat[PD4B]);
    isu += PTW(pat[PD4C]);
    isu += PTW(pat[PD4D]);

    // Pattern D4

    pt = pattern_tabs[10][inter];
    isu += PTW(pat[PD5A]);
    isu += PTW(pat[PD5B]);
    isu += PTW(pat[PD5C]);
    isu += PTW(pat[PD5D]);

#if RARE_WRITE

    // Pattern EDGE+2X

    pt = pattern_freq_tabs[0][inter];

    write1 += RARE(PAT2W(PHV1A, B2, G2),0);
    write1 += RARE(PAT2W(PHV1B, G2, G7),1);
    write1 += RARE(PAT2W(PHV1C, G7, B7),2);
    write1 += RARE(PAT2W(PHV1D, B7, B2),3);

    // Pattern 2x5

    pt = pattern_freq_tabs[1][inter];

    write2 += RARE(PAT2W(PR1A, E1, E2),0);
    write2 += RARE(PAT2W(PR1B, H5, G5),1);
    write2 += RARE(PAT2W(PR1C, D8, D7),2);
    write2 += RARE(PAT2W(PR1D, A4, B4),3);
    write2 += RARE(PAT2W(PR1E, D1, D2),4);
    write2 += RARE(PAT2W(PR1F, E8, E7),5);
    write2 += RARE(PAT2W(PR1G, A5, B5),6);
    write2 += RARE(PAT2W(PR1H, H4, G4),7);

    // Pattern 3x3

    pt = pattern_freq_tabs[2][inter];

    write3 += RARE(patt_val_white(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3)),0);
    write3 += RARE(patt_val_white(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3)),1);
    write3 += RARE(patt_val_white(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6)),2);
    write3 += RARE(patt_val_white(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6)),3);

    // Pattern HV2

    pt = pattern_freq_tabs[3][inter];

    write4 += RARE(PTW(pat[PHV2A]),0);
    write4 += RARE(PTW(pat[PHV2B]),1);
    write4 += RARE(PTW(pat[PHV2C]),2);
    write4 += RARE(PTW(pat[PHV2D]),3);

    // Pattern HV3				       

    pt = pattern_freq_tabs[4][inter];

    write5 += RARE(PTW(pat[PHV3A]),0);
    write5 += RARE(PTW(pat[PHV3B]),1);
    write5 += RARE(PTW(pat[PHV3C]),2);
    write5 += RARE(PTW(pat[PHV3D]),3);

    // Pattern HV4				       

    pt = pattern_freq_tabs[5][inter];

    write6 += RARE(PTW(pat[PHV4A]),0);
    write6 += RARE(PTW(pat[PHV4B]),1);
    write6 += RARE(PTW(pat[PHV4C]),2);
    write6 += RARE(PTW(pat[PHV4D]),3);

    // Pattern D8				       

    pt = pattern_freq_tabs[6][inter];

    write7 += RARE(PTW(pat[PD1A]),0);
    write7 += RARE(PTW(pat[PD1B]),1);

    // Pattern D7

    pt = pattern_freq_tabs[7][inter];

    write8 += RARE(PTW(pat[PD2A]),0);
    write8 += RARE(PTW(pat[PD2B]),1);
    write8 += RARE(PTW(pat[PD2C]),2);
    write8 += RARE(PTW(pat[PD2D]),3);

    // Pattern D6

    pt = pattern_freq_tabs[8][inter];

    write9 += RARE(PTW(pat[PD3A]),0);
    write9 += RARE(PTW(pat[PD3B]),1);
    write9 += RARE(PTW(pat[PD3C]),2);
    write9 += RARE(PTW(pat[PD3D]),3);

    // Pattern D5

    pt = pattern_freq_tabs[9][inter];

    write10 += RARE(PTW(pat[PD4A]),0);
    write10 += RARE(PTW(pat[PD4B]),1);
    write10 += RARE(PTW(pat[PD4C]),2);
    write10 += RARE(PTW(pat[PD4D]),3);

    // Pattern D4

    pt = pattern_freq_tabs[10][inter];

    write11 += RARE(PTW(pat[PD5A]),0);
    write11 += RARE(PTW(pat[PD5B]),1);
    write11 += RARE(PTW(pat[PD5C]),2);
    write11 += RARE(PTW(pat[PD5D]),3);

#endif

  }

#if RARE_WRITE

  num++;

  if (write1 || write2 || write3 || write4 || write5 || write6 ||
      write7 || write8 || write9 || write10 || write11) {

    SPFELD sf;
    BrettSf(pb, &sf);

    write_num++;
    if (player == WHITE) SfInvert(&sf);
    sf.Marke = MA_WEISS_NICHT;
    fSfWrite(fp_rare, &sf, 1);
    fflush(fp_rare);

#if 0
    SfAus(&sf, 0, 0);
cout << setw(2) << hex << 
            write1 << " " <<  write2 << " " <<  write3 << " " <<  write4 << " " << 
            write5 << " " <<  write6 << " " <<  write7 << " " <<  write8 << " " << 
            write9 << " " <<  write10 << " " <<  write11 << endl;
#endif

  } 

  if ((num & 65535) == 0) cout << " ::: " << num << " " << write_num << " " << float(write_num)/num << endl;

#endif


  // PARITY 
    
  isu += feature_tabs[0][inter][pb->SteinAnz & 1]; 
  isu = int(isu * FACTOR);
    

#if 0
  cout << "eval=" << isu << endl;
  show_config_values(pb, inter);
#endif
    
  if (isu >=  WERTGEWINN) return(+(WERTGEWINN-1)); 
  if (isu <= -WERTGEWINN) return(-(WERTGEWINN-1));  
  
  return isu;
}
