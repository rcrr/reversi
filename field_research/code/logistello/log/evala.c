// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// dynamic patterns / 9.98

#include "main.h"
#include "board.h"
#include "eval.h"
#include "move.h"
#include "crt.h"
#include "fpatt.h"
#include "tabtype.h"
#include "shm.h"
#include "sparse.h"
#include "nndpatt.h"

#define USE_SHM          false
#define SHM_SIZE         4500000

#define HIT_COUNT        false  // sparse pattern hit count

#define RARE_WRITE       false         // save positions with rare configurations
#define RARE_FILE        "rare.sfk"

#define HASH_SPEED_UP      true
#define SPARSE_FAST_TABS   false     // is not faster :( but needs much more memory

static int foo_b;

static void foo(int b)
{
  foo_b = b;
}


// if 2x5 => change PERM_INDEX!!!

static char *pattern_files[] = {
  "patt_EDGE+2X.front.bin",
  "patt_3x3.front.bin",
  "patt_HV2.front.bin",
  "patt_HV3.front.bin",
  "patt_HV4.front.bin",
  "patt_D8.front.bin",
  "patt_D7.front.bin",
  "patt_D6.front.bin",
  "patt_D5.front.bin",
  "patt_D4.front.bin"
};

const int PATT_NUM = sizeof(pattern_files)/sizeof(pattern_files[0]);

static TTYPE *pattern_tabs[PATT_NUM][64];


// features

static char *feature_files[] = {
  "feat_PARITY.front.bin"
};

const int FEAT_NUM = sizeof(feature_files)/sizeof(feature_files[0]);

static TTYPE *feature_tabs[FEAT_NUM][64];


// sparse patterns

static struct { char *conf_file, *par_file; } spattern_files[] =
{ 
  // { "conf2x8.75.hash", "spatt_2x8.4x4.bin" },
  // { "conf4x4.75.hash", "spatt_4x4.4x4.bin" }
  // { "confadj.75.hash", "spatt_ADJ.4x4.bin" },
  // { "confopp.75.hash", "spatt_OPP.4x4.bin" }
};

//const int IND2x8 = 0;
const int IND4x4 = 0;
//const int INDADJ = 0;
//const int INDOPP = 1;

const int SPATT_NUM = sizeof(spattern_files)/sizeof(spattern_files[0]);
static SparsePattern sp[SPATT_NUM];
static TTYPE *spattern_tabs[PATT_NUM][64];


// dynamic pattern

const bool USE_DPATT = true;
const char *dp_name = "dpatt_FRONT.front.bin";

static DPatternInfo dpi = {

 {
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,0,0,0,0,0, 
    0,5,6,7,8,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "FRONT"

};


static DPattern dp;
static TTYPE *dpattern_tabs[100][65];


#undef P10
#define P10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])

#undef P9
#define P9(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])

#undef P8
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

  // patterns

  FOR (i, PATT_NUM) {
    string name = DataPath + "/" + pattern_files[i];
    FILE *fp = fopen(name.c_str(), "r");
    if (!fp) { cerr << name << endl; Error("file not found"); }

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

  // dynamic pattern

  if (USE_DPATT) {

    string name;

    dp.init(dpi, false);

    name = DataPath + "/" + dp_name;
    FILE *fp = fopen(name.c_str(), "r");
    if (!fp) { cerr << name << endl; Error("file not found"); }

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
    int ti;

    FOR (ti, dp.tab_num) {

      cout << "read tab " << ti << endl;

      FOR (k, interval_num) {

	dpattern_tabs[ti][k] = (TTYPE*) calloc(sizeof(TTYPE), config_num);
	
	FOR (j, config_num) {
	  short val = fgetc(fp);
	  val += fgetc(fp) << 8;
	  dpattern_tabs[ti][k][j] = val;
	}
	
	// dpattern_tabs[ti][k] += (config_num-1)/2;
      }
    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);
  }



  // features

  FOR (i, FEAT_NUM) {
    string name = DataPath + "/" + feature_files[i];
    FILE *fp = fopen(name.c_str(), "r");
    if (!fp)  { cerr << name << endl; Error("file not found"); }

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


  // sparse patterns

  FOR (i, SPATT_NUM) {

    string name = DataPath + "/" + spattern_files[i].conf_file;
    sp[i].hash_read(name.c_str());

#if 1
    name = DataPath + "/" + spattern_files[i].par_file;

    FILE *fp = fopen(name.c_str(), "r");
    if (!fp) { 
      cerr << name << endl;
      Error("file not found"); 
    }

    cout << name << endl;

    if (fgetc(fp) != 0x55) Error("magic");
    if (fgetc(fp) != 0xaa) Error("magic");
    if (fgetc(fp) != 0x00) Error("magic");

    int a = fgetc(fp);
    int b = ((sp[i].var_num+sp[i].check_sum) % 253) & 0xff;

    if (a != b) {
      printf("!!! checksums different: %d %d \a\n", a, b);
      // Error("check-sum");
    }

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

    int config_num = sp[i].var_num;

#if 0

    FOR (k, interval_num) {

      cout << k+1 << ": ";
      
      int sz = (k+1) * config_num * sizeof(TTYPE);

      int power = 1;

      while (power < sz) power += power;

      cout << "sz=" << sz << " power=" << power << " q=" << double(power)/sz << endl;
    }

#endif

    TTYPE *all_tabs = (TTYPE*) calloc(sizeof(TTYPE), config_num*interval_num);

    cout << sizeof(TTYPE) * config_num*interval_num << " bytes allocated" << endl;

    FOR (k, interval_num) {

      spattern_tabs[i][k] = &all_tabs[k*config_num];

      FOR (j, config_num) {

	short val = fgetc(fp);
	val += fgetc(fp) << 8;

	if (mode != SHM_ACCESS)
	  spattern_tabs[i][k][j] = val;
	else
	  if (spattern_tabs[i][k][j] != val) Error("shm val different!");
      }
    }

    if (feof(fp)) Error("file too short");
    fgetc(fp);
    if (!feof(fp)) Error("file too long");

    fclose(fp);

#if SPARSE_FAST_TABS

    sp[i].hash_tab2 = new sint2[(interval_num+1)*sp[i].hash_n];

    // copy adjusted hash-locks

    FOR (k, sp[i].hash_n) {
      sp[i].hash_tab2[k] = (sp[i].hash_tab[k].id1-(sp[i].id1n-1)/2)*2;
    }

    // copy values

    FOR (k, interval_num) {

      cout << k << endl;

      sint2 *ha = &sp[i].hash_tab2[(k+1)*sp[i].hash_n];

      FOR (j, sp[i].hash_n) {

	if (sp[i].hash_tab[j].id1 != SparseInfo2::EMPTY) {
	  
	  int index = sp[i].hash_tab[j].index;

	  if (index < 0 || index >= config_num) Error("index out of range");

	  ha[j] = spattern_tabs[i][k][index];

	} else ha[j] = 0;
      }
    }

    // center hash_tab2

    sp[i].hash_tab2 += (sp[i].id2n-1)/2;

    free(all_tabs);
    FOR (k, interval_num) spattern_tabs[i][k] = 0;

#endif


#if HASH_SPEED_UP
    sp[i].hash_speed_adjust();
#endif

#endif

  }

}




const int PERM_INDEX = -1;  // no 2x5!!!

const int PERM_LEN   = 10;
const int perm[PERM_LEN] = { 0, 1, 2, 3, 8, 4, 5, 6, 7, 9 };

#if 1

// new code: save memory by using temp array

static void permute_2x5()
{
  int i, j, k;
  int config_num = Pot3[PERM_LEN];
  TTYPE temp[config_num];

  if (PERM_INDEX < 0) return;

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
  
  if (PERM_INDEX < 0) rerturn;

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


/**************************************************************/


static inline int patt_val(TTYPE *pt, int player, int index)
{
  if (player > 0) return pt[index]; else return pt[-index];
}

static inline int patt_val_black(TTYPE *pt, int index) { return pt[ index]; }
static inline int patt_val_white(TTYPE *pt, int index) { return pt[-index]; }


WERT EvalASlow(BRETT *, PARTEI)
{
  Error("slow not supported");
  return 0;
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

WERT EvalA(BRETT *pb, PARTEI player)
{
  const float FACTOR = (0.1 / 512) * WERTFAKTOR;
  register STRAHLTYP *pat

#ifdef NDEBUG
#if 1 && __i386__
asm("%edi")
#endif
#endif

    = pb->NewPatt.st;

  register TTYPE *pt 
  
#ifdef NDEBUG
#if 0 &&  __i386__
asm("%ebx")
#endif
#endif

  ;

  register int hash_incr; 


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

    // compute stage indices

    int j;

    FOR (j, 65) {
      int s = (j - disc_min)/interval_len;
      if (s < 0) s = 0;
      else if (s >= interval_num) s = interval_num-1;
      stage[j] = s;
    }
  }

  Square *p

#ifdef NDEBUG
#if 0 && __i386__
asm("%esi")
#endif
#endif
    =pb->p;

  register int isu  

#ifdef NDEBUG
#if 0 && __i386__
asm("%ebp")
#endif
#endif

  = 0;
    
  int inter = stage[pb->SteinAnz];
  int i1, index;


#if SPARSE_FAST_TABS

  register sint2 *s_hash_tab 
#ifdef NDEBUG
#if 1 && __i386__
asm("%esi")
#endif
#endif
    ;

#else

  register SparseInfo2 *s_hash_tab 
#ifdef NDEBUG
#if 1 && __i386__
asm("%esi")
#endif
#endif
    ;

#endif



  register int *seeds

#ifdef NDEBUG
#if 1 && __i386__
asm("%ecx")
#endif
#endif

    ;


/* code depends on: 

    - double indexes
    - sizeof(*seeds) = 4
    - sizeof(SparseInfo2) = 4
*/


#if !DOUBLE_INDEX
#error "!DOUBLE_INDEX"
#endif

  
  if (player == BLACK) {
  
    // Pattern EDGE+2X

    pt = pattern_tabs[0][inter];
    isu += PAT2B(PHV1A, B2, G2);
    isu += PAT2B(PHV1B, G2, G7);
    isu += PAT2B(PHV1C, G7, B7);
    isu += PAT2B(PHV1D, B7, B2);

#if 1

    // Pattern 3x3

    pt = pattern_tabs[1][inter];
    isu += patt_val_black(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3));
    isu += patt_val_black(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3));
    isu += patt_val_black(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6));
    isu += patt_val_black(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6));

    // Pattern HV2

    pt = pattern_tabs[2][inter];
    isu += PTB(pat[PHV2A]);
    isu += PTB(pat[PHV2B]);
    isu += PTB(pat[PHV2C]);
    isu += PTB(pat[PHV2D]);
    
    // Pattern HV3				       

    pt = pattern_tabs[3][inter];
    isu += PTB(pat[PHV3A]);
    isu += PTB(pat[PHV3B]);
    isu += PTB(pat[PHV3C]);
    isu += PTB(pat[PHV3D]);

    // Pattern HV4				       

    pt = pattern_tabs[4][inter];
    isu += PTB(pat[PHV4A]);
    isu += PTB(pat[PHV4B]);
    isu += PTB(pat[PHV4C]);
    isu += PTB(pat[PHV4D]);

    // Pattern D8				       

    pt = pattern_tabs[5][inter];
    isu += PTB(pat[PD1A]);
    isu += PTB(pat[PD1B]);

    // Pattern D7

    pt = pattern_tabs[6][inter];
    isu += PTB(pat[PD2A]);
    isu += PTB(pat[PD2B]);
    isu += PTB(pat[PD2C]);
    isu += PTB(pat[PD2D]);

    // Pattern D6

    pt = pattern_tabs[7][inter];
    isu += PTB(pat[PD3A]);
    isu += PTB(pat[PD3B]);
    isu += PTB(pat[PD3C]);
    isu += PTB(pat[PD3D]);
  
    // Pattern D5

    pt = pattern_tabs[8][inter];
    isu += PTB(pat[PD4A]);
    isu += PTB(pat[PD4B]);
    isu += PTB(pat[PD4C]);
    isu += PTB(pat[PD4D]);

    // Pattern D4

    pt = pattern_tabs[9][inter];
    isu += PTB(pat[PD5A]);
    isu += PTB(pat[PD5B]);
    isu += PTB(pat[PD5C]);
    isu += PTB(pat[PD5D]);


    // dynamic pattern

    int tr;

    if (0) {
      SPFELD sf;
      BrettSf(pb, &sf);
      SfAus(&sf, 0, 0);
    }

    FOR (tr, 8) {
      int tab_index;
      int conf_index = dp.indices(p, tr, tab_index);
      pt = dpattern_tabs[tab_index][inter];

      // cout << "tab=" << tab_index << " conf=" << conf_index << endl;

      assert(conf_index >= 0);
      assert(conf_index < 6561);

      // isu += pt[conf_index];
    }

#endif

  } else {  // player == WHITE

    // Pattern EDGE+2X

    pt = pattern_tabs[0][inter];
    isu += PAT2W(PHV1A, B2, G2);
    isu += PAT2W(PHV1B, G2, G7);
    isu += PAT2W(PHV1C, G7, B7);
    isu += PAT2W(PHV1D, B7, B2);

#if 1

    // Pattern 3x3

    pt = pattern_tabs[1][inter];
    isu += patt_val_white(pt, P9(A1,B1,C1,A2,B2,C2,A3,B3,C3));
    isu += patt_val_white(pt, P9(H1,G1,F1,H2,G2,F2,H3,G3,F3));
    isu += patt_val_white(pt, P9(A8,B8,C8,A7,B7,C7,A6,B6,C6));
    isu += patt_val_white(pt, P9(H8,G8,F8,H7,G7,F7,H6,G6,F6));

    // Pattern HV2

    pt = pattern_tabs[2][inter];
    isu += PTW(pat[PHV2A]);
    isu += PTW(pat[PHV2B]);
    isu += PTW(pat[PHV2C]);
    isu += PTW(pat[PHV2D]);
    
    // Pattern HV3				       

    pt = pattern_tabs[3][inter];
    isu += PTW(pat[PHV3A]);
    isu += PTW(pat[PHV3B]);
    isu += PTW(pat[PHV3C]);
    isu += PTW(pat[PHV3D]);

    // Pattern HV4				       

    pt = pattern_tabs[4][inter];
    isu += PTW(pat[PHV4A]);
    isu += PTW(pat[PHV4B]);
    isu += PTW(pat[PHV4C]);
    isu += PTW(pat[PHV4D]);

    // Pattern D8				       

    pt = pattern_tabs[5][inter];
    isu += PTW(pat[PD1A]);
    isu += PTW(pat[PD1B]);

    // Pattern D7

    pt = pattern_tabs[6][inter];
    isu += PTW(pat[PD2A]);
    isu += PTW(pat[PD2B]);
    isu += PTW(pat[PD2C]);
    isu += PTW(pat[PD2D]);
  
    // Pattern D6

    pt = pattern_tabs[7][inter];
    isu += PTW(pat[PD3A]);
    isu += PTW(pat[PD3B]);
    isu += PTW(pat[PD3C]);
    isu += PTW(pat[PD3D]);
  
    // Pattern D5

    pt = pattern_tabs[8][inter];
    isu += PTW(pat[PD4A]);
    isu += PTW(pat[PD4B]);
    isu += PTW(pat[PD4C]);
    isu += PTW(pat[PD4D]);

    // Pattern D4

    pt = pattern_tabs[9][inter];
    isu += PTW(pat[PD5A]);
    isu += PTW(pat[PD5B]);
    isu += PTW(pat[PD5C]);
    isu += PTW(pat[PD5D]);


    // dynamic pattern

    int tr;

    FOR (tr, 8) {
      int tab_index;
      int conf_index = dp.indices(p, tr, tab_index);
      pt = dpattern_tabs[tab_index][inter];

      assert(conf_index >= 0);
      assert(conf_index < 6561);

      // isu += pt[6560-conf_index];
    }

#endif

  }

  // PARITY 
    
  isu += feature_tabs[0][inter][pb->SteinAnz & 1]; 

  //cout << "isu= " << isu << endl;

  isu = int(isu * FACTOR);

  if (isu >=  WERTGEWINN) return(+(WERTGEWINN-1)); 
  if (isu <= -WERTGEWINN) return(-(WERTGEWINN-1));  
  
  return isu;

}
