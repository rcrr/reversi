// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// sparse tables / 2.98

#include "main.h"
#include "board.h"
#include "eval.h"
#include "move.h"
#include "crt.h"
#include "fpatt.h"
#include "tabtype.h"
#include "shm.h"
#include "sparse.h"

#define USE_SHM          false
#define SHM_SIZE         4500000

#define ALTERNATIVE_TABS true   // true => *.newbin

#define HIT_COUNT        false  // sparse pattern hit count

#define RARE_WRITE       false         // save positions with rare configurations
#define RARE_FILE        "rare.sfk"

#define HASH_SPEED_UP      true
#define SPARSE_FAST_TABS   false     // is not faster :( but needs much more memory

int foo_b;

static void foo(int b)
{
  foo_b = b;
}


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
  "patt_EDGE+2X.wall.bin",
  "patt_2x5.wall.bin",
  "patt_3x3.wall.bin",
  "patt_HV2.wall.bin",
  "patt_HV3.wall.bin",
  "patt_HV4.wall.bin",
  "patt_D8.wall.bin",
  "patt_D7.wall.bin",
  "patt_D6.wall.bin",
  "patt_D5.wall.bin",
  "patt_D4.wall.bin",
  "patt_WALL.wall.bin"
#endif

};

const int PATT_NUM = sizeof(pattern_files)/sizeof(pattern_files[0]);

static TTYPE *pattern_tabs[PATT_NUM][64];


// features

static char *feature_files[] = {
#if !ALTERNATIVE_TABS
  "feat_PARITY.bin.A"
#else
  "feat_PARITY.wall.bin"
#endif
};

const int FEAT_NUM = sizeof(feature_files)/sizeof(feature_files[0]);

static TTYPE *feature_tabs[FEAT_NUM][64];


// sparse patterns

static struct { char *conf_file, *par_file; } spattern_files[] =
{ 
  // { "conf2x8.75.hash", "spatt_2x8.newbin" },
  // { "conf4x4.326.0", "spatt_4x4.newbin" }, // takes too long
  // { "confadj.75.hash", "spatt_ADJ.newbin" },
  // { "confopp.75.hash", "spatt_OPP.newbin" }
};

const int IND2x8 = 0;
// const int IND4x4 = 1;
const int INDADJ = 1;
const int INDOPP = 2;

const int SPATT_NUM = sizeof(spattern_files)/sizeof(spattern_files[0]);
  
static SparsePattern sp[SPATT_NUM];

static TTYPE *spattern_tabs[PATT_NUM][64];


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

#define P12(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) \
(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*p[p1]+p[p2])+p[p3])+p[p4])+p[p5])+p[p6])+p[p7])+p[p8])+p[p9])+p[p10])+p[p11])+p[p12])

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

  // patterns

  FOR (i, PATT_NUM) {
    char name[2*DATEI_MAX];
    sprintf(name, "%s/%s", DataPath, pattern_files[i]);
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


  // features

  FOR (i, FEAT_NUM) {
    char name[2*DATEI_MAX];
    sprintf(name, "%s/%s", DataPath, feature_files[i]);
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

    char name[2*DATEI_MAX];

    sprintf(name, "%s/%s", DataPath, spattern_files[i].conf_file);

    //   sp[i].read(name);
    sp[i].hash_read(name);

    // sp[i].free_lists();

#if 1
    sprintf(name, "%s/%s", DataPath, spattern_files[i].par_file);

    FILE *fp = fopen(name, "r");
    if (!fp) { 
      fprintf(stderr, name); 
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




#if RARE_WRITE

static void read_freq_tables()
{
  int i, j, k, r;

  FOR (i, PATT_NUM) {
    FILE *fp = fopen(pattern_freq_files[i], "r");
    if (!fp) { fprintf(stderr, pattern_freq_files[i]); Error("file not found"); }

    cout << pattern_freq_files[i] << endl;

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
    FILE *fp = fopen(feature_freq_files[i], "r");
    if (!fp)  { fprintf(stderr, feature_freq_files[i]); Error("file not found"); }

    cout << feature_freq_files[i] << endl;

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
const int perm[PERM_LEN] = { 0, 1, 2, 3, 8, 4, 5, 6, 7, 9 };

#if 1

// new code: save memory by using temp array

static void permute_2x5()
{
  int i, j, k;
  int config_num = Pot3[PERM_LEN];
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


WERT EvalKSlow(BRETT *, PARTEI)
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

WERT EvalK(BRETT *pb, PARTEI player)
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

#if 1

/* code depends on: 

    - double indexes
    - sizeof(*seeds) = 4
    - sizeof(SparseInfo2) = 4
*/


#if !DOUBLE_INDEX
#error "!DOUBLE_INDEX"
#endif

#if 1

#if 1

#if !HASH_SPEED_UP
#error "!HASH_SPEED_UP"
#endif

#if SPARSE_FAST_TABS

#define E2X8(v,p1,p2,I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((sint2*)seeds+i1);\
    register sint2 *psi = (sint2*)((sint1*)s_hash_tab+(sd v pat[p2]));\
    if (*psi == i1) isu += *(sint2*)((sint1*)psi+hash_incr);\
  }

#define EADJ(v,p1,p2,x,I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((sint2*)seeds+i1);\
    register sint2 *psi = (sint2*)((sint1*)s_hash_tab+(sd v (3*pat[p2]+p[x]+p[x])));\
    if (*psi == i1) isu += *(sint2*)((sint1*)psi+hash_incr);\
  }

#define EOPP(v,p1,p2, I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((sint2*)seeds+i1);\
    register sint2 *psi = (sint2*)((sint1*)s_hash_tab+(sd v pat[p2]));\
    if (*psi == i1) isu += *(sint2*)((sint1*)psi+hash_incr);\
  }

#else


#define E2X8(v,p1,p2,I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((short*)seeds+i1);\
    register SparseInfo2 &si = *(SparseInfo2*)((short*)s_hash_tab+(sd v pat[p2]));\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EADJ(v,p1,p2,x,I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((short*)seeds+i1);\
    register SparseInfo2 &si = *(SparseInfo2*)((short*)s_hash_tab+(sd v (3*pat[p2]+p[x]+p[x])));\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EOPP(v,p1,p2, I) \
  {\
    register int i1 = v (pat[p1]);\
    register int sd = *(int*)((short*)seeds+i1);\
    register SparseInfo2 &si = *(SparseInfo2*)((short*)s_hash_tab+(sd v pat[p2]));\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#endif


#else


#define E2X8(v,p1,p2,I) \
  {\
    int i1 = v (pat[p1]);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1 >> 1] >> 1) v (pat[p2] >> 1)];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EADJ(v, p1,p2,x, I) \
  {\
    int i1 = v (pat[p1]);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1 >> 1] >> 1) v (3*(pat[p2] >> 1) + p[x])];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EOPP(v, p1,p2, I) \
  {\
    int i1 = v (pat[p1]);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1 >> 1] >> 1) v (pat[p2] >> 1)];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#endif

#endif
#endif



#if 0

#if HASH_SPEED_UP
#error "HASH_SPEED_UP"
#endif

#define E2X8(v,p1,p2,I) \
  {\
    int i1 = 3280 v (pat[p1] >> 1);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1]) + (3280 v (pat[p2] >> 1))];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EADJ(v,p1,p2,x,I) \
  {\
    int i1 = 3280 v (pat[p1] >> 1);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1]) + (9841 v (3*(pat[p2] >> 1) + p[x]))];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#define EOPP(v,p1,p2,I) \
  {\
    int i1 = 3280 v (pat[p1] >> 1);\
    register SparseInfo2 &si = s_hash_tab[(seeds[i1]) + 3280 v (pat[p2] >> 1)];\
    if (si.id1 == i1) isu += pt[si.index];\
  }

#endif


#if 0

// OK

#if HIT_COUNT
#define INCR  hit_cnt++
static try_cnt = 0;
static hit_cnt = 0;
#else
#define INCR
#endif

// old : g++ code not very good
// hash-tab  : fast!

#define E2X8(v,p1,p2,I) \
   i1 = 3280 v (pat[p1] >> 1);\
   if (sp[IND2x8].hash_exists(i1)) {\
      index = sp[IND2x8].hash_fast_index(i1, 3280 v (pat[p2] >> 1)); \
      if (index >= 0) { \
        isu += pt[index];   /* THIS CODE IS NOT GENERATED?! */\
   /*      if (abs(pt[index]) >= 4000) cout << "!!!" << endl;\
    cout << "2x8 " << I << " val=" << pt[index] << endl;\
      for (int k=0; k < interval_num; k++) cout << spattern_tabs[0][k][index] << " ";\
      cout << endl;*/\
     }\
   }


#define E4X4(v,p1,p21,pa22,p23,pa24,p25,pa26,p27,pa28) \
    i1 = 3280 v (pat[p1] >> 1);\
    if (sp[IND4x4].hash_exists(i1)) {\
    /*    cout << "4x4: i1=" << ( 3280 v pat[p1]/2) << " i2=" << (3280 v P8(p21,pa22,p23,pa24,p25,pa26,p27,pa28)) << endl;*/\
      index = sp[IND4x4].hash_fast_index(i1, 3280 v P8(p21,pa22,p23,pa24,p25,pa26,p27,pa28)); \
      if (index >= 0) { \
        isu += pt[index];\
    /*cout << "4x4 " << i_tab4x4[index]  << " ind=" << index << " " << sp_sum << endl;*/\
      }\
    }


#define EADJ(v,p1,p2,x, I) \
  i1 = 3280 v (pat[p1] >> 1);\
  if (sp[INDADJ].hash_exists(i1)) {\
   /*    cout << "adj: i1=" << ( 3280 v (pat[p1]>>1)) << " i2=" << (9841 v (3*(pat[p2] >> 1) + p[x])) << endl;*/\
  \
    index = sp[INDADJ].hash_fast_index(i1, 9841 v (3*(pat[p2] >> 1) + p[x])); \
    if (index >= 0) { \
      isu += pt[index];\
    /*        cout << "adj " << I << ": ";\
    int k; FOR (k, 13) cout << spattern_tabs[2][k][index] << " " << endl;*/\
    }\
  }

#define EOPP(v,p1,p2, I) \
  i1 = 3280 v (pat[p1] >> 1);\
  if (sp[INDOPP].hash_exists(i1)) {\
   /*    cout << "opp: i1=" << ( 3280 v pat[p1]/2) << " i2=" << (3280 v P8(p21,pa22,p23,pa24,p25,pa26,p27,pa28))\
 << endl;*/\
   index = sp[INDOPP].hash_fast_index(i1, 3280 v (pat[p2] >> 1)); \
   if (index >= 0) { \
     isu += pt[index];\
    /*cout << "opp " << i_tab4x4[index]  << " ind=" << index << " " << sp_sum << endl;*/\
   }\
  }
#endif

#if 0

// no sparse patterns

#define E2X8(v,p1,p2,I)
#define E4X4(v,p1,p21,pa22,p23,pa24,p25,pa26,p27,pa28)
#define EADJ(v,p1,p2,x, I) 
#define EOPP(v,p1,p2, I) 

#endif

#if 0

// binary search

#define E2X8(v,p1,p2,I) \
    i1 = 3280 v (pat[p1] >> 1);\
    if (sp2x8.exists(i1)) {\
      index = sp2x8.fast_index(i1, 3280 v (pat[p2] >> 1)); \
      if (index >= 0) { \
	isu += i_tab2x8[index];\
      /*    cout << "evalk: 2x8 " << i_tab2x8[index] << " ind=" << index << " val=" << i_tab2x8[index] << endl;*/\
      }\
    }

#define E4X4(v,p1,p21,pa22,p23,pa24,p25,pa26,p27,pa28,I) \
    i1 = 3280 v (pat[p1] >> 1);\
    if (sp4x4.exists(i1)) {\
    /*    cout << "4x4: i1=" << ( 3280 v pat[p1]/2) << " i2=" << (3280 v P8(p21,pa22,p23,pa24,p25,pa26,p27,pa28)) << endl;*/\
      index = sp4x4.fast_index(i1, 3280 v P8(p21,pa22,p23,pa24,p25,pa26,p27,pa28)); \
      if (index >= 0) { \
        isu += i_tab4x4[index];\
      /*    cout << "evalk: 4x4 " << i_tab4x4[index]  << " ind=" << index << " val=" << i_tab4x4[index] << endl;*/\
      }\
    }

#endif


#if RARE_WRITE
  int write1=0,write2=0,write3=0,write4=0,write5=0,write6=0,write7=0,write8=0, 
      write9=0,write10=0,write11=0;

#define RARE(x,n) (((x) > 0) << (n))

#endif


#if HIT_COUNT

  try_cnt++;

  if ((try_cnt & 65535) == 0) printf("%d %f\n", try_cnt, double(hit_cnt)/try_cnt);

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

#if 1
    // Pattern WALL

    pt = pattern_tabs[11][inter];

#if 0

    int index = P12(D2,E3,F4,G5,C3,D4,E5,F6,B4,C5,D6,E7);
    cout << "index=" << index << " t=" << patt_val_black(pt, index) << endl;
    index = P12(E2,D3,C4,B5,F3,E4,D5,C6,G4,F5,E6,D7);
    cout << "index=" << index << " t=" << patt_val_black(pt, index) << endl;

    isu = 0; // !!!

    int perm[4][12] = {
      { 1,2,3,4,5,6,7,8,9,10,11,12 },
      { 12,11,10,9,8,7,6,5,4,3,2,1 },
      { 9,10,11,12,5,6,7,8,1,2,3,4 },
      { 4,3,2,1,8,7,6,5,12,11,10,9 }
    };

    int di[12], k=0, l;
    di[k++] = p[D2];
    di[k++] = p[E3];
    di[k++] = p[F4];
    di[k++] = p[G5];
    di[k++] = p[C3];
    di[k++] = p[D4];
    di[k++] = p[E5];
    di[k++] = p[F6];
    di[k++] = p[B4];
    di[k++] = p[C5];
    di[k++] = p[D6];
    di[k++] = p[E7];
    int min_s = MAXINT;

    FOR (k, 4) {
      int s=0;
      FOR (l, 12) s = s+s+s+di[perm[k][l]-1];
      printf("%d -> %d\n", k, s);
      if (s < min_s) min_s = s;
    }

    printf("min_s=%d\n", min_s);

    k=0;

    di[k++] = p[E2];
    di[k++] = p[D3];
    di[k++] = p[C4];
    di[k++] = p[B5];
    di[k++] = p[F3];
    di[k++] = p[E4];
    di[k++] = p[D5];
    di[k++] = p[C6];
    di[k++] = p[G4];
    di[k++] = p[F5];
    di[k++] = p[E6];
    di[k++] = p[D7];
    min_s = MAXINT;

    FOR (k, 4) {
      int s=0;
      FOR (l, 12) s = s+s+s+di[perm[k][l]-1];
      printf("%d -> %d\n", k, s);
      if (s < min_s) min_s = s;
    }

    printf("min_s=%d\n", min_s);
#endif

    isu += patt_val_black(pt, P12(B5,G5,B6,C6,D6,E6,F6,G6,C7,D7,E7,F7));
    isu += patt_val_black(pt, P12(B4,G4,B3,C3,D3,E3,F3,G3,C2,D2,E2,F2));
    isu += patt_val_black(pt, P12(E2,E7,F2,F3,F4,F5,F6,F7,G3,G4,G5,G6));
    isu += patt_val_black(pt, P12(D2,D7,C2,C3,C4,C5,C6,C7,B3,B4,B5,B6));

#endif

#if 0 

    // sparse patterns

    if (0) { SPFELD sf;
      BrettSf(pb, &sf);
      SfAus(&sf, 0, 0);
      printf("BTM\n");
    }
 
    // 2x8

    //fork(); foo(isu == 0);

    assert(sizeof(seeds[0]) == 4);
    assert(sizeof(SparseInfo2) == 4);

    pt = spattern_tabs[IND2x8][inter];
    seeds = sp[IND2x8].hash_id1_seeds;
#if SPARSE_FAST_TABS
    s_hash_tab = sp[IND2x8].hash_tab2;
    hash_incr = sp[IND2x8].hash_incr[inter];
#else
    s_hash_tab = sp[IND2x8].hash_tab;
#endif

    //printf("%d %d %d\n", hash_incr, inter, sp[IND2x8].hash_n);

    E2X8(+,PHV1A,PHV2A,1);
    E2X8(+,PHV1B,PHV2B,2);
    E2X8(+,PHV1C,PHV2C,3);
    E2X8(+,PHV1D,PHV2D,4);

    //cout << "isu13= " << isu << endl;
    //cout << "debug1= " << debug1 << endl;
    //cout << "debug2= " << debug2 << endl;

#if 0

    // 4x4 

    pt = spattern_tabs[IND4x4][inter];

    E4X4(+,PR1A, A3,B3,C3,D3,A4,B4,C4,D4);
    E4X4(+,PR1E, H3,G3,F3,E3,H4,G4,F4,E4);
    E4X4(+,PR1C, H6,G6,F6,E6,H5,G5,F5,E5);
    E4X4(+,PR1F, A6,B6,C6,D6,A5,B5,C5,D5);

#endif

    // adj

    pt = spattern_tabs[INDADJ][inter];
    seeds = sp[INDADJ].hash_id1_seeds;
#if SPARSE_FAST_TABS
    s_hash_tab = sp[INDADJ].hash_tab2;
    hash_incr = sp[INDADJ].hash_incr[inter];
#else
    s_hash_tab = sp[INDADJ].hash_tab;
#endif

    EADJ(+,PHV1B,PHV1A, G2, 1);
    EADJ(+,PHV1C,PHV1B, G7, 2);
    EADJ(+,PHV1D,PHV1C, B7, 3);
    EADJ(+,PHV1A,PHV1D, B2, 4);

#if 1
    // opp

    pt = spattern_tabs[INDOPP][inter];
    seeds = sp[INDOPP].hash_id1_seeds;
#if SPARSE_FAST_TABS
    s_hash_tab = sp[INDOPP].hash_tab2;
    hash_incr = sp[INDOPP].hash_incr[inter];
#else
    s_hash_tab = sp[INDOPP].hash_tab;
#endif

    EOPP(+,PHV1A,PHV1C,1);
    EOPP(+,PHV1B,PHV1D,2);
#endif

    // sparse patterns

#endif   




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

#if 1
    // Pattern WALL

    pt = pattern_tabs[11][inter];

#if 0
    int index = P12(D2,E3,F4,G5,C3,D4,E5,F6,B4,C5,D6,E7);
    cout << "index=" << index << " t=" << patt_val_white(pt, index) << endl;
    index = P12(E2,D3,C4,B5,F3,E4,D5,C6,G4,F5,E6,D7);
    cout << "index=" << index << " t=" << patt_val_white(pt, index) << endl;

    isu = 0; // !!!
#endif

    isu += patt_val_white(pt, P12(B5,G5,B6,C6,D6,E6,F6,G6,C7,D7,E7,F7));
    isu += patt_val_white(pt, P12(B4,G4,B3,C3,D3,E3,F3,G3,C2,D2,E2,F2));
    isu += patt_val_white(pt, P12(E2,E7,F2,F3,F4,F5,F6,F7,G3,G4,G5,G6));
    isu += patt_val_white(pt, P12(D2,D7,C2,C3,C4,C5,C6,C7,B3,B4,B5,B6));

#endif

#if 0

    // sparse patterns

    if (0) { SPFELD sf;
      BrettSf(pb, &sf);
      SfAus(&sf, 0, 0);
      printf("WTM\n");
    }


    // 2x8

    //fork(); foo(isu == 0);

    pt = spattern_tabs[IND2x8][inter];
    seeds = sp[IND2x8].hash_id1_seeds;

#if SPARSE_FAST_TABS
    s_hash_tab = sp[IND2x8].hash_tab2;
    hash_incr = sp[IND2x8].hash_incr[inter];
#else
    s_hash_tab = sp[IND2x8].hash_tab;
#endif

    //printf("WTM %d %d\n", hash_incr, inter);

    E2X8(-,PHV1A,PHV2A,2);
    E2X8(-,PHV1B,PHV2B,2);
    E2X8(-,PHV1C,PHV2C,3);
    E2X8(-,PHV1D,PHV2D,4);

#if 0

    // 4x4 

    pt = spattern_tabs[IND4x4][inter];

    E4X4(-,PR1A, A3,B3,C3,D3,A4,B4,C4,D4);
    E4X4(-,PR1E, H3,G3,F3,E3,H4,G4,F4,E4);
    E4X4(-,PR1C, H6,G6,F6,E6,H5,G5,F5,E5);
    E4X4(-,PR1F, A6,B6,C6,D6,A5,B5,C5,D5);

#endif

    // adj

    pt = spattern_tabs[INDADJ][inter];
    seeds = sp[INDADJ].hash_id1_seeds;
#if SPARSE_FAST_TABS
    s_hash_tab = sp[INDADJ].hash_tab2;
    hash_incr = sp[INDADJ].hash_incr[inter];
#else
    s_hash_tab = sp[INDADJ].hash_tab;
#endif

    EADJ(-,PHV1B,PHV1A, G2, 1);
    EADJ(-,PHV1C,PHV1B, G7, 2);
    EADJ(-,PHV1D,PHV1C, B7, 3);
    EADJ(-,PHV1A,PHV1D, B2, 4);

#if 1

    // opp

    pt = spattern_tabs[INDOPP][inter];
    seeds = sp[INDOPP].hash_id1_seeds;
#if SPARSE_FAST_TABS
    s_hash_tab = sp[INDOPP].hash_tab2;
    hash_incr = sp[INDOPP].hash_incr[inter];
#else
    s_hash_tab = sp[INDOPP].hash_tab;
#endif

    EOPP(-,PHV1A,PHV1C,1);
    EOPP(-,PHV1B,PHV1D,2);

#endif

    // sparse patterns

#endif



#if RARE_WRITE

    // Pattern EDGE+2X

    pt = pattern_freq_tabs[0][inter];

    write1 += RARE(PAT2W(PHV1A, B2, G2),0);
    write1 += RARE(PAT2W(PHV1B, B7, G7),1);
    write1 += RARE(PAT2W(PHV1C, B2, B7),2);
    write1 += RARE(PAT2W(PHV1D, G2, G7),3);

    // Pattern 2x5

    pt = pattern_freq_tabs[1][inter];

    write2 += RARE(PAT2W(PR1A, E1, E2),0);
    write2 += RARE(PAT2W(PR1B, D1, D2),1);
    write2 += RARE(PAT2W(PR1C, E8, E7),2);
    write2 += RARE(PAT2W(PR1D, D8, D7),3);
    write2 += RARE(PAT2W(PR1E, A5, B5),4);
    write2 += RARE(PAT2W(PR1F, H5, G5),5);
    write2 += RARE(PAT2W(PR1G, A4, B4),6);
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

  //cout << "isu= " << isu << endl;

  isu = int(isu * FACTOR);

  if (isu >=  WERTGEWINN) return(+(WERTGEWINN-1)); 
  if (isu <= -WERTGEWINN) return(-(WERTGEWINN-1));  
  
  return isu;

}
