// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// table learning by hill climbing / 2.97
//   go through all examples -> no learning rate needed


#include "main.h"
#include "crt.h"
#include "patt.h"
#include "eval.h"
#include "trans.h"
#include "fpatt.h"
#include "disco.h"

#undef DISC_MIN
#undef DISC_MAX

#define OLD false

#if OLD
const int WIDTH = 0;
#else
const int WIDTH     = 2;  // was 3
const int RIGHT_ADJ = -1; // interval = [i-WIDTH ... i+WIDTH+RIGHT_ADJ]
#endif

// const int N_MIN    = 20; // look for at least that many examples in vicinity
// const int W_MAX    = 3;  // extrapol-interpos size

const int N_SMOOTH = 50; // use delta * min(1,n/N_SMOOTH) if rare_factor is not used

const float LEARN_RATE = 1.0;  // 3.0 -> divergence


#define USE_RARE_FACTOR true

const float RARE_FACTOR_MAX = 1.0;  // was 1
const int   RARE_N_MAX      = 15;   // was 20


const char FILE_NAME[]      = "test.sfk";  // "1163.sfk"

const bool USE_TEST_BOARDS = false;
const char TEST_FILE_NAME[] = "test3960.sfk";

const bool READ_BOARDS_INTO_MEMORY = false;
const int INFO_MAXNUM = 25000;


const int N_SUB = 12;  // total frequ. < N_SUB => use sub-pattern // doesn't work

const bool WRITE_DATA = true;
const bool READ_DATA  = true;
const bool ASC_WRITE  = false;

// for discordance computation

const int DISCOR_BUCKET_SIZE = 250;
const int DISCO_STEP = 4;

const int RES_MAX = 10;
const int RES_MIN = -RES_MAX;
const int RES_NUM = (RES_MAX-RES_MIN)/2 + 1;


#define P8(p1,p2,p3,p4,p5,p6,p7,p8) \
(3280 + (3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8))

#define P10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
(29524 + (3*(3*(3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8)+p9)+p10))

#define P12(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) \
(265720 + (3*(3*(3*(3*(3*(3*(3*(3*(3*(3*(3*p1+p2)+p3)+p4)+p5)+p6)+p7)+p8)+p9)+p10)+p11)+p12))




// example weight

int learn_weight(int /* result */)
{
#if 1

  return 1;

#else

  // is worse :(

  result = abs(result);
  if (result >= 30) return 1;
  if (result >= 20) return 2;
  if (result >= 10) return 3;
  if (result >= 6)  return 4;
  return 10;

#endif

}


float smooth(int n)
{
  if (n >= N_SMOOTH) return 1.0;

  return (float)n/N_SMOOTH;
}





// function to approximate

#if 1

// disc difference

inline float F(int diff) { return diff; }

#else

// sigmoid 

inline float F(int diff)
{
  return 100*(2.0/(1+exp(-0.073*diff)) - 1.0);
}

#endif


static int patt_index(SPFELD &bo, int *sq_list)
{
  int i=0, n = 0;

  while (sq_list[i]) {

    n = 3*n + bo.p[sq_list[i]] + 1;
    i++;
  }

  return n;
}


static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}


static inline bool sgn_equal(SPFELD &bo1, SPFELD &bo2)
{
  int i;

  FOR (i, 100) {
    if ((bo1.p[i] != 0) != (bo2.p[i] != 0)) return false;
  }

  return true;
}


class Entry {

public:

  const int DISC_MIN     = 13;
  const int DISC_MAX     = 64;
  const int INTERVAL_LEN = 4;
  const int INTERVAL_NUM = (DISC_MAX+1-DISC_MIN)/INTERVAL_LEN;

  const int WINDOW_LENHALF = 1;


  class Estimate {
  public:
    float  y;
    double delta_sum;
    int    n;

#if USE_RARE_FACTOR
    float  rare_factor;
#endif

    Estimate() { 
      y = 0; delta_sum = 0; n = 0; 
#if USE_RARE_FACTOR
      rare_factor = 1.0; 
#endif
    }
  };

  Entry  *p_map;
  Estimate *esti;
  bool   one_phase;

  Entry(bool o_p) { 
    int i_n;

    one_phase = o_p;
    if (one_phase) i_n = 1; else i_n = INTERVAL_NUM;

    esti = new Estimate[i_n];

    p_map = 0;
  }

  inline static int inter(int disc_num)
  {
    if      (disc_num < DISC_MIN) disc_num = DISC_MIN;
    else if (disc_num > DISC_MAX) disc_num = DISC_MAX;

    return (disc_num - DISC_MIN) / INTERVAL_LEN;
  }


  inline static int min_num(int inter)
  {
    return DISC_MIN + inter*INTERVAL_LEN;
  }

  inline static int max_num(int inter)
  {
    return DISC_MIN + (inter+1)*INTERVAL_LEN - 1;
  }

  inline float get_y(int inter) 
  { 
    return esti[inter].y;
  }

  inline int get_n(int inter) 
  { 
    return esti[inter].n; 
  }

  inline void set_y(int inter, float value) 
  { 
    esti[inter].y = value; 
  }

  inline void set_rare_factor(int inter, float value) 
  { 
    esti[inter].rare_factor = value; 
  }

  inline float get_rare_factor(int inter) const 
  { 
    return esti[inter].rare_factor;
  }

  inline void new_delta(int inter, double delta, int n)
  {
    esti[inter].delta_sum += delta;
    esti[inter].n += n;
  }


  // new: update all entries separately

  float update()
  {
    int i;
    double max_abs_d = 0;

    if (one_phase) Error("one-phase not implemented");

    // update all y's for each stage

#if 0
cout << endl;
FOR (i, INTERVAL_NUM) cout << setw(5) << esti[i].n;
cout << endl;
#endif

    FOR (i, INTERVAL_NUM) {

      if (esti[i].n) {

#if USE_RARE_FACTOR

	// divide??? that's probably wrong (use rare_factor only for prediction)

	double d = (esti[i].delta_sum/esti[i].n)/esti[i].rare_factor;

#else
	double d = smooth(esti[i].n) * esti[i].delta_sum/esti[i].n;
#endif

	esti[i].y += d;
	d = abs(d);
	if (d > max_abs_d) max_abs_d = d;
      }
    }


    // new: linear interpolation for stages without examples
    
    FOR (i, INTERVAL_NUM) {

      if (esti[i].n == 0) {

	// left neighbor
	  
	int l, r;
	
	float yl = 0;

	for (l=i-1; l >= 0; l--)
	  if (esti[l].n) { yl = esti[l].y; break; }

	if (l < 0) l = 0;

	// right neighbor

	float yr = 0;

	for (r=i+1; r < INTERVAL_NUM; r++)
	  if (esti[r].n) { yr = esti[r].y; break; }
	
	if (r >= INTERVAL_NUM) r = INTERVAL_NUM-1;

	esti[i].y = (i-l) * (yr-yl) / (r-l) + yl;
      }
    }

#if USE_RARE_FACTOR

    // set rare_factor

    float b = (RARE_FACTOR_MAX-1)/(1-1/sqrt(RARE_N_MAX));
    float a = RARE_FACTOR_MAX - b;

    FOR (i, INTERVAL_NUM) {
      if (esti[i].n >= RARE_N_MAX) 
	esti[i].rare_factor = 1.0;
      else if (esti[i].n == 0)         
	esti[i].rare_factor = RARE_FACTOR_MAX;
      else
	esti[i].rare_factor = a + b/sqrt(esti[i].n);

#if 0
      cout << esti[i].n << " " << esti[i].rare_factor << endl;
#endif

    }
    
#endif

    // clear delta info

    FOR (i, INTERVAL_NUM) {
      esti[i].delta_sum = 0;
      esti[i].n = 0;
    }
    
    return max_abs_d;
  }

};



struct PatternInfo {
  int sq[100];
  char *name;
  bool one_phase;
  int sub_list[64];
};


class Pattern {

public:

  const int MAX_LEN = 16;

  char  *name;
  int   sq[100];
  int   perm[MAX_LEN+1];
  int   len;
  bool  one_phase;
  Entry *tab;
  int   trans_num;
  int   sq_lists[8][MAX_LEN+1];
  int   sub_list[MAX_LEN+1];
  Pattern *p_sub;

  Pattern() { tab = 0; len = 0; trans_num = 0; p_sub = 0; one_phase = false; }

  void init(PatternInfo &pinf)
  {
    int i, j, num=0;
    SPFELD bo, bos[8];
    bool used[MAX_LEN+1];

    name = pinf.name;
    one_phase = pinf.one_phase;

    cout << "Pattern " << name << endl;

    // compute square lists

    FOR (i, MAX_LEN) used[i] = false;

    FOR (i, 100) {
      sq[i] = pinf.sq[i];
      if (sq[i] != 0) {
	if (sq[i] <= 0 || sq[i] >= MAX_LEN) 
	  Error("Pattern::init: value out of range in pattern-info");
	used[sq[i]-1] = true;
	num++;
      }
      bo.p[i] = sq[i];
    }

    if (num == 0)       Error("Pattern::init: no squares!");
    if (num >= MAX_LEN) Error("Pattern::init: too many squares!");

    FOR (i, num) if (!used[i]) Error("Pattern::init: hole in sequence");

    FOR (i, num) perm[i] = i+1;

    Transform(&bo, bos);

    trans_num = 0;

    FOR (i, 8) {

      for (j=0; j < i; j++)
	if (sgn_equal(bos[i], bos[j])) break;

      if (j >= i) {

	// new transformation => compute sq-list

	FOR (j, 100) {

	  if (bos[i].p[j]) { 
	    if (!ZUG(j)) Error("Pattern::init: illegal square in pattern!");

	    //  cout << int(bos[i].p[j]) << " ";
	    sq_lists[trans_num][bos[i].p[j]-1] = j;
	  }
	}

	sq_lists[trans_num][num] = 0;

	FOR (j, num) { GKoorAus(sq_lists[trans_num][j]); cout << "," << flush; }
	cout << endl;
	trans_num++;

      } else {

	if (j == 0) {

	  // pattern is symmetrical => determine perm-list

	  int n=0;

	  FOR (j, 100) {
	    if (bo.p[j]) {
	      perm[n++] = bos[i].p[j];
	    }
	  }
	}
      }
    }

    FOR (j, num) cout << perm[j] << " ";
    cout << endl << endl;

    // allocate table

    tab = new Entry[Pot3[num]](one_phase);
    if (!tab) Error("Pattern::init: no memory");

    // compute permutations

    FOR (i, Pot3[num]) {
      int list[MAX_LEN+1], j;
      int s = i;

      FOR (j, num) {
	list[num-1-j] = s % 3;
	s /= 3;
      }

      s = 0;

      FOR (j, num) s = s+s+s+list[perm[j]-1];

      // pointer to unique representation (if symmetrical)

      if (i > s) 
        tab[i].p_map = &tab[s];
      else
	tab[i].p_map = &tab[i];

      //    printf("%d->%d\n", i, s);
    }

    len = num;




    if (pinf.sub_list[0]) {

      // init sub-pattern

      PatternInfo sub_info;

      sub_info.one_phase = 0;
      sub_info.name = "sub";
      sub_info.sub_list[0] = 0;

      FOR (i, 100) sub_info.sq[i] = 0;

      for (i=0; pinf.sub_list[i]; i++) {
	sub_list[i] = pinf.sub_list[i];
	FOR (j, 100) 
	  if (sq[j] == pinf.sub_list[i]) {
	    sub_info.sq[j] = i+1; break;
	  }
      }
      sub_list[i] = 0; // end-marker
      
      p_sub = new Pattern;
      p_sub->init(sub_info);

    }
  }

  static void conf_write(FILE *fp, int l, int n)
  {
    int i, r, cont[MAX_LEN];
    
    FOR (i, l) {
      r = n % 3;
      if      (r == 0) cont[i] = WHITE;
      else if (r == 1) cont[i] = LEER;
      else             cont[i] = BLACK;
      n /= 3;
    }
    
    FOR (i, l) WriteDisc(fp, cont[l-1-i]);
  }


  void asc_write(FILE *fp)
  {
    int i, k;

    FOR (k, Pot3[len]) {
      
      Pattern::conf_write(fp, len, k);

      if (one_phase) {

	fprintf(fp, " %+6.3f", tab[k].p_map->get_y(0));

      } else {

	FOR (i, Entry::INTERVAL_NUM) 
	  fprintf(fp, " %+6.3f", tab[k].p_map->get_y(i));

      }

      fprintf(fp, "\n");
    }
  }


  void bin_write(FILE *fp)
  {
    int k, l;

    fputc(len, fp);

    if (one_phase) {

      fputc(1,  fp);
      fputc(4,  fp);
      fputc(61, fp);

      FOR (k, Pot3[len]) {
	int val = int(round(tab[k].p_map->get_y(0) * 512));
	
	if (val > +32767) { 
	  val = +32767;
	  fprintf(stderr, ">"); fflush(stderr); 
	} else if (val < -32767) {
	  val = -32767;
	  fprintf(stderr, "<"); fflush(stderr); 
	}
	
	fputc(val & 255, fp);
	fputc(val >> 8,  fp);
      }
      
    } else {

      fputc(Entry::INTERVAL_NUM, fp);
      fputc(Entry::DISC_MIN, fp);
      fputc(Entry::INTERVAL_LEN, fp);

      FOR (l, Entry::INTERVAL_NUM)
	FOR (k, Pot3[len]) {
	int val = int(round(tab[k].p_map->get_y(l) * 512));
	
	if (val > +32767) { 
	  val = +32767;
	  fprintf(stderr, ">"); fflush(stderr); 
	} else if (val < -32767) {
	  val = -32767;
	  fprintf(stderr, "<"); fflush(stderr); 
	}
	
	fputc(val & 255, fp);
	fputc(val >> 8,  fp);
      }
    }
  }


  bool bin_read(FILE *fp)
  {
    int k, l;

    if (fgetc(fp) != len) return true;

    if (one_phase) {

      if (fgetc(fp) != 1)  return true;
      if (fgetc(fp) != 4)  return true;
      if (fgetc(fp) != 61) return true;

      FOR (k, Pot3[len]) {
	short val = fgetc(fp);
	val += fgetc(fp) << 8;
	tab[k].p_map->set_y(0, val/512.0);
      }
      
    } else {

      if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
      if (fgetc(fp) != Entry::DISC_MIN)     return true;
      if (fgetc(fp) != Entry::INTERVAL_LEN) return true;

      FOR (l, Entry::INTERVAL_NUM)
	FOR (k, Pot3[len]) {
	  short val = fgetc(fp);
	  val += fgetc(fp) << 8;
	  tab[k].p_map->set_y(l, val / 512.0);
        }
    }

    return false;
  }

  bool bin_write_freq(FILE *fp, int n)
  {
    int k, l;

    if (n <= 0) Error("n <= 0");
    if (one_phase) Error("one phase not implemented");

    fputc(len, fp);
    fputc(Entry::INTERVAL_NUM, fp);
    fputc(Entry::DISC_MIN, fp);
    fputc(Entry::INTERVAL_LEN, fp);

    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, Pot3[len]) {
        int val = (tab[k].p_map->get_n(l) < n) * 512;
	fputc(val & 255, fp);
	fputc(val >> 8,  fp);
      }

    return false;
  }


  int sub_index(int ind)
  {
    int i, list[MAX_LEN+1];
    int s = ind;

    FOR (i, len) {
      list[len-1-i] = s % 3;
      s /= 3;
    }

    s = 0;

    for (i=0; sub_list[i]; i++) 
      s = s+s+s+list[sub_list[i]-1];
    
    // cout << name << " " << ind << " -> " << s << endl;

    return s;
  }


};



// ******************** patterns ********************** XXX


static PatternInfo pattern_infos[] = {

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,6,7,8,0, 
    0,0,9,0,0,0,0,10,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "EDGE+2X", 0
   //, { 1,2,3,4,5,6,7,8,0 }
  }

  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,0,0,0,0, 
    0,6,7,8,9,10,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "2x5", 0
   //, { 1,2,3,4,6,7,8,9,0 } 
  }

  ,

  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,0,0,0,0,0,0,
    0,4,5,6,0,0,0,0,0,0,
    0,7,8,9,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "3x3", 0
   //, { 1,2,3,4,5,6,0 }
  }

#if 0

  ,

  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,0,0,0,0,0,
    0,0,5,6,7,0,0,0,0,0,
    0,0,0,8,9,0,0,0,0,0,
    0,0,0,0,10,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "TRI" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,1,1,1,0,0,0,0,0,
    0,0,1,1,1,0,0,0,0,0,
    0,0,0,1,1,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "TRI" }

  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,1,1,1,0,0,0,0,0, 
    0,1,1,1,1,0,0,0,0,0, 
    0,1,1,1,1,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "3x4", true }

  ,
  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,1,0,0,0,0,1,1,0, 
    0,1,1,0,0,0,0,1,1,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "TEST" }
#endif

#if 1
  ,
  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV2" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV3" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "HV4" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0, 
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0, 
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,8,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D8" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0, 
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,7,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D7" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0,
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0, 
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,6,0,0,0,
    0,0,0,0,0,0,0,0,0,0 
  }, "D6" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0, 
    0,0,0,3,0,0,0,0,0,0, 
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,5,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D5" }
  ,

  {{
    0,0,0,0,0,0,0,0,0,0, 
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,1,0,0,0,0,0,0,0,0,
    0,0,2,0,0,0,0,0,0,0, 
    0,0,0,3,0,0,0,0,0,0,
    0,0,0,0,4,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "D4" }

#endif

};

const int PATT_NUM = sizeof(pattern_infos)/sizeof(pattern_infos[0]);

Pattern patterns[PATT_NUM];



// *********************** features ************************** YYY


struct FeatureInfo {

  char *name;
  int (*f)(SPFELD &, int disc_num);
  int bucket_num;
  bool one_phase;
    
}; 


class Feature {

public:

  char *name;
  int (*f)(SPFELD &, int disc_num);
  int bucket_num;
  bool one_phase;
  Entry *entries;

  Feature() { f = 0; bucket_num = 0; entries = 0; }

  void init(FeatureInfo &finf)
  {
    name = finf.name;
    f = finf.f;
    bucket_num = finf.bucket_num;
    one_phase = finf.one_phase;

    cout << "Feature(" << bucket_num << ") " << name << endl;

    if (bucket_num <= 0) Error("Feature::init: bucket_num <= 0");
    entries = new Entry[bucket_num](one_phase);
    if (!entries) Error("Feature::init: no mem");
  }


  void asc_write(FILE *fp)
  {
    int i, k;
    
    FOR (k, bucket_num) {
      
      fprintf(fp, "%2d ", k);
      FOR (i, Entry::INTERVAL_NUM) fprintf(fp, " %+6.3f", entries[k].get_y(i));
      fprintf(fp, "\n");
    }
  }


  void bin_write(FILE *fp)
  {
    int i, l;

    if (one_phase) Error("one phased features not supported yet");

    fputc(bucket_num, fp);
    fputc(Entry::INTERVAL_NUM, fp);
    fputc(Entry::DISC_MIN, fp);
    fputc(Entry::INTERVAL_LEN, fp);

    FOR (l, Entry::INTERVAL_NUM) {
      FOR (i, bucket_num) {

	int val = int(round(entries[i].get_y(l) * 512));
      
	if (val > +32767) { 
	  val = +32767;
	  fprintf(stderr, ">"); fflush(stderr); 
	} else if (val < -32767) {
	  val = -32767;
	  fprintf(stderr, "<"); fflush(stderr); 
	}

	fputc(val & 255, fp);
	fputc(val >> 8,  fp);
      } 
    }
  }

  bool bin_read(FILE *fp)
  {
    int k, l;

    if (fgetc(fp) != bucket_num) return true;
    if (fgetc(fp) != Entry::INTERVAL_NUM) return true;
    if (fgetc(fp) != Entry::DISC_MIN)     return true;
    if (fgetc(fp) != Entry::INTERVAL_LEN) return true;

    FOR (l, Entry::INTERVAL_NUM)
      FOR (k, bucket_num) {
        short val = fgetc(fp);
	val += fgetc(fp) << 8;
	entries[k].set_y(l, val / 512.0);
    }

    return false;
  }

  bool bin_write_freq(FILE *fp, int n)
  {
    int k, l;

    if (n <= 0) Error("n <= 0");
    if (one_phase) Error("one phased features not supported yet");

    fputc(bucket_num, fp);
    fputc(Entry::INTERVAL_NUM, fp);
    fputc(Entry::DISC_MIN, fp);
    fputc(Entry::INTERVAL_LEN, fp);

    FOR (l, Entry::INTERVAL_NUM) {
      FOR (k, bucket_num) {
        int val = (entries[k].get_y(l) < n) * 512;
	fputc(val & 255, fp);
	fputc(val >> 8,  fp);
      }
    }

    return false;
  }

};


static int feature_par(SPFELD &, int disc_num)
{
  return disc_num & 1;
}

static int feature_center_diff(SPFELD &bo, int)
{
  sint1 *p=bo.p;

  return p[C3]+p[C4]+p[C5]+p[C6]+
         p[D3]+p[D4]+p[D5]+p[D6]+
         p[E3]+p[E4]+p[E5]+p[E6]+
         p[F3]+p[F4]+p[F5]+p[F6];
}

const int MOB_MAX = 15;

static int feature_mob(SPFELD &bo, int)
{
  BRETT brett;

  SfBrett(&bo, &brett);
  
  int md = int(STRMOBALLB(&brett));

  if      (md > +MOB_MAX) md = +MOB_MAX;
  else if (md < -MOB_MAX) md = -MOB_MAX;
  
  return md + MOB_MAX;
}


static FeatureInfo feature_infos[] = {

    { "PARITY", feature_par, 2 }
    //  , { "CENTER", feature_center_diff, 1 }
  //  , { "MOB",    feature_mob, 2*MOB_MAX+1 }

};


const int FEAT_NUM = sizeof(feature_infos)/sizeof(feature_infos[0]);

static Feature features[FEAT_NUM];






class BoardInfo {

public:

  typedef int IndType;
  const int MAX_IND = 531441;

  const int IND_MAX  = 61;
  const int FEAT_MAX = 2;
 
  short disc_num, result;
  float val, val2;

  static int feat_num, ind_num;
  static Pattern *p_patt[IND_MAX];

  IndType indices[IND_MAX]; 
  short  feat[FEAT_MAX];

  // compute board data

  void from_board(SPFELD &bo, int dn)
  {
    int m = bo.Marke;

    if (m < MA_DIFF || m > MA_DIFF+128) Error("no diff-label");

    m -= (MA_DIFF+64);

    // make m even 

    if (m & 1) {
      if (m > 0) m++;
      if (m < 0) m--;
    }

    result = m;
    val = F(result);
    disc_num = dn; 

    //    if (SfAnz(&bo) != dn) Error("corrupt dn"); // !!!

    int i;

    FOR (i, FEAT_NUM) {
      feat[i] = features[i].f(bo, disc_num);

      if (features[i].bucket_num != 1 && 
	  (feat[i] < 0 || feat[i] >= features[i].bucket_num)) {
	cerr << features[i].name << ": " << flush;
	Error("feature value out of range");
      }
    }

    int ind = 0;

    FOR (i, PATT_NUM) {

      int j;
      Pattern &patt = patterns[i];

      FOR (j, patt.trans_num) {

	if (ind >= BoardInfo::IND_MAX) Error("too many updates");

	int r = patt_index(bo, patt.sq_lists[j]);
        if (r < 0 || r > MAX_IND) Error("illegal index");

	p_patt[ind] = &patterns[i];
	indices[ind] = r;
	ind++;

	// cout << patt_index(bo, patt.sq_lists[j]) << " ";
      }
      // cout << endl;
    }

    if (ind_num > 0) {
      if (ind_num != ind) Error("different ind_num");
    } else
      ind_num = ind;
  }


  float evaluate()
  {
    int i, inter = Entry::inter(disc_num);
    float value = 0;

    FOR (i, FEAT_NUM) {
      Feature &f = features[i];

      if (f.bucket_num == 1) 
	value += feat[i] * f.entries[0].get_y(inter);
      else 
	value += f.entries[feat[i]].get_y(inter);
    }

    FOR (i, BoardInfo::ind_num) 
      value += p_patt[i]->tab[indices[i]].p_map->get_y(inter);

    return value;
  }

};

int BoardInfo::ind_num = 0;
Pattern *BoardInfo::p_patt[IND_MAX];


static bool finished = false;

void _abort(void) { exit(20); }

static void ctrlc(int) { finished = true; }



int main(int argc, char **argv)
{
  FILE *fp=0;
  
  BoardInfo *infos=0;
  int  num=0, i, j;

  signal(SIGINT, ctrlc);
  InitSetzen();


  if (argc <= 1) {
    Error("usage: onn2 (-iter | -show table-file | -freq n)");
    exit(20);
  }

  int argi = 1;

  if (argi < argc && !strcmp(argv[argi], "-show")) {
   
    PatternInfo pi;
    Pattern pat;
    int k;

    fp=fopen(argv[argi+1], "r");
    if (!fp) Error("can't open file");
    int len = fgetc(fp);

    cout << len << " discs" << endl;

    if (len < 1 || len > 12) Error("len?");

    if (fgetc(fp) != Entry::INTERVAL_NUM) Error("interval num?");
    if (fgetc(fp) != Entry::DISC_MIN)     Error("disc min?");
    if (fgetc(fp) != Entry::INTERVAL_LEN) Error("interval len?");
    fclose(fp);

    pi.one_phase = 0;
    pi.sub_list[0] = 0;
    pi.name = "show";

    FOR (i, 100) pi.sq[i] = 0;
    k = len;

    FOR_SFPOS10(i) {
      pi.sq[i] = k;
      k--;
      if (k <= 0) break;
    }

    fp=fopen(argv[argi+1], "r");
    if (!fp) Error("can't open file");
    pat.init(pi);
    pat.bin_read(fp);
    pat.asc_write(stdout);
    fclose(fp);
    exit(0);
  }


  if (READ_BOARDS_INTO_MEMORY) {
    infos = new BoardInfo[INFO_MAXNUM];
    if (!infos) Error("no mem.");
  }

  FOR (i, PATT_NUM) patterns[i].init(pattern_infos[i]);
  FOR (i, FEAT_NUM) features[i].init(feature_infos[i]);


  vector<BoardInfo> dinfo[Entry::INTERVAL_NUM][RES_NUM];
  bool ok[Entry::INTERVAL_NUM][RES_NUM];

  if (USE_TEST_BOARDS) {

    // read test boards into buckets

    fp = fopen(TEST_FILE_NAME, "r");
    if (!fp) Error("can't open test-file");

    printf("reading test-boards ...\n");
    
    FOR (i, Entry::INTERVAL_NUM) 
      FOR (j, RES_NUM)
        ok[i][j] = false;

    int ok_num;

    for (ok_num = 0; ok_num < Entry::INTERVAL_NUM*RES_NUM; ) {

      SPFELD bo;
      int num_b, num_w;

      if (!fSfReadNum(fp, &bo, num_b, num_w)) break;

      int res = bo.Marke - (MA_DIFF+64);
      if (res < -64 || res > 64) Error("no diff-label");

      if (res & 1) {
	if (res > 0) res++;
	if (res < 0) res--;
      }

      int disc_num = num_b + num_w;

      if (disc_num < Entry::DISC_MIN || disc_num > Entry::DISC_MAX || 
	  res < RES_MIN || res > RES_MAX)  
	continue;

      int res_index = res -RES_MIN;

      res_index /= 2;

      if (ok[Entry::inter(disc_num)][res_index]) continue;

      BoardInfo inf;
    
      inf.from_board(bo, disc_num);
      dinfo[Entry::inter(disc_num)][res_index].push_back(inf);
      
      //SfAus(&bo, 0, 0);
      //cout << disc_num << " " << res << " " << res_index << endl;
      
      if (dinfo[Entry::inter(disc_num)][res_index].size() >= DISCOR_BUCKET_SIZE) {

	//      cout << "ok " << disc_num << " " << res_index << endl;
	
	ok[Entry::inter(disc_num)][res_index] = true;
	ok_num++;
      }
    }

    fclose(fp);

    if (ok_num < Entry::INTERVAL_NUM*RES_NUM) Error("not enough test-boards");
 
  }


  if (READ_BOARDS_INTO_MEMORY) {

    fp = fopen(FILE_NAME, "r");
    if (!fp) Error("can't open file");

    printf("reading boards ...\n");

    for (num=0; ; num++) {

      SPFELD bo;
      int num_b, num_w;

      if (!fSfReadNum(fp, &bo, num_b, num_w)) break;

      //      if ((num & 4095) == 0) cout << num << "\r" << flush;
    
      int disc_num = num_b+num_w;

      if (disc_num < Entry::DISC_MIN || disc_num > Entry::DISC_MAX) {
	num--; continue;
      }

      if (num >= INFO_MAXNUM) break;
      if (FEAT_NUM > BoardInfo::FEAT_MAX) Error("too many features");
    
      infos[num].from_board(bo, disc_num);
    }

    printf("%d\n", num);
    
    fclose(fp);
  }
 
  int training_num[Entry::INTERVAL_NUM], training_correct[Entry::INTERVAL_NUM];
  double delta_sums[Entry::INTERVAL_NUM];
  int    delta_ns[Entry::INTERVAL_NUM];


  if (READ_DATA) {

    cout << "\nread config. values" << endl;
  
    FOR (i, PATT_NUM) {

      char name[1000];
      
      sprintf(name, "patt_%s.bin", patterns[i].name);
      cout << name << endl;
      fp=fopen(name,"r");
      if (fp) {
	if (patterns[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
      }
    }
    
    cout << "OK" << endl;
    cout << "\nread feature weights" << endl;
    
    FOR (i, FEAT_NUM) {
      
      char name[1000];
      
      sprintf(name, "feat_%s.bin", features[i].name);
      
      cout << name << endl;
      
      fp=fopen(name,"r");
      if (fp) {
	if (features[i].bin_read(fp)) cout << "read error" << endl;
	fclose(fp);
      } else {
	cout << " not found" << endl;
      }
    }
    
    cout << "OK" << endl << endl;
  }



  for (int iter=1; ; iter++) {

    // go through all examples

    int k;
    double delta_sum = 0;

    FOR (i, Entry::INTERVAL_NUM) {
      training_correct[i] = training_num[i] = 0;
      delta_sums[i] = 0;
      delta_ns[i] = 0;
    }

    if (!READ_BOARDS_INTO_MEMORY) {
      num = 0;
      fp = fopen(FILE_NAME, "r");
      if (!fp) Error("can't open board-file");
    }

    for (k=0; ; k++) {

      int i;
      SPFELD bo;
      BoardInfo inf;

      if (READ_BOARDS_INTO_MEMORY) {

	if (k >= num) break;
        inf = infos[k];

      } else {

	int num_b, num_w;

	if (!fSfReadNum(fp, &bo, num_b, num_w)) break;
	int disc_num = num_b+num_w;

	if (disc_num < Entry::DISC_MIN || disc_num > Entry::DISC_MAX) {
	  continue;
	}

	num++;
	inf.from_board(bo, disc_num);
      }

      // if (!(num & 1023)) cout << num << "\r" << flush;

      // compute values

      int inter0 = Entry::inter(inf.disc_num);

      for (int inter=inter0-WIDTH; inter <= inter0+WIDTH+RIGHT_ADJ; inter++) {
	if (inter >= 0 && inter < Entry::INTERVAL_NUM) {

	  int num_terms = FEAT_NUM + BoardInfo::ind_num;
	  float value = 0;

	  FOR (i, FEAT_NUM) {
	    Feature &feat = features[i];

	    if (feat.bucket_num == 1) {
	      value += inf.feat[i] * feat.entries[0].get_y(inter) 
#if USE_RARE_FACTOR
		* feat.entries[0].get_rare_factor(inter)
#endif
		;
	      if (inf.feat[i] == 0) num_terms--;
	    } else {
	      value += feat.entries[inf.feat[i]].get_y(inter)
#if USE_RARE_FACTOR
		* feat.entries[inf.feat[i]].get_rare_factor(inter)
#endif
		;
	    }
	  }

	  i = BoardInfo::ind_num-1;
	  register Pattern **pp_patt = &BoardInfo::p_patt[i];
	  register int *p_ind        = &inf.indices[i];

	  for (i=BoardInfo::ind_num-1; i >= 0; i--) {
	    //value += patterns[BoardInfo::patt_num[i]].tab[inf.indices[i]].p_map->get_y(inter);
	    Entry &entry = *(*pp_patt--)->tab[*p_ind--].p_map;
	    // old : value += (*pp_patt--)->tab[*p_ind--].p_map->get_y(inter)

	    value += entry.get_y(inter) 

#if USE_RARE_FACTOR
		* entry.get_rare_factor(inter)
#endif
	      ;
	  }

	  // cout << k << " " << inf.val << " " << value << " " << num_terms << " " << BoardInfo::ind_num << endl;
	  
#if 0	  
	  if (inter == inter0 && value != inf.evaluate()) {
	    cout << ">>>" << inf.val << " " << inf.evaluate() << endl;
	  }
#endif

	  double delta = inf.val - value;
	  int learn_n = learn_weight(inf.result);
	  
	  if (inter == inter0) {
	    delta_sum += abs(delta);
	    delta_sums[inter] += abs(delta);
	    delta_ns[inter]++;

	    if (inf.result) {
	      if ((inf.result > 0) ^ (value < 0)) 
		training_correct[Entry::inter(inf.disc_num)]++;
	      training_num[Entry::inter(inf.disc_num)]++;
	    }
	  }
	  
	  double learn_delta = LEARN_RATE*learn_n*(delta/num_terms);
	  
	  FOR (i, FEAT_NUM) {
	    
	    Feature &feat = features[i];
	    
	    if (feat.bucket_num == 1) {
	      if (inf.feat[i] != 0) 
		feat.entries[0].new_delta(inter, learn_delta/inf.feat[i], learn_n);
	    } else {
	      feat.entries[inf.feat[i]].new_delta(inter, learn_delta, learn_n);
	    }
	  }
	  
	  for (i=BoardInfo::ind_num-1; i >= 0; i--) {

	    BoardInfo::p_patt[i]->tab[inf.indices[i]].p_map->
	      new_delta(inter, learn_delta, learn_n);
	  }
	}
      }
    }

    if (!READ_BOARDS_INTO_MEMORY) fclose(fp);

    if (iter == 1) {

      // establish pointers to sub-patterns if configuration count is low

      FOR (i, PATT_NUM) {

	Pattern &patt = patterns[i];

	if (patt.p_sub) {

	  cout << "sub-pattern: " << patt.name << flush;

	  int n=0, sub_n=0;

	  for (k=Pot3[patt.len]-1; k >= 0; k--) {

	    if (patt.tab[k].p_map == &patt.tab[k]) {
	      int l, sn = 0;
	      
	      FOR (l, Entry::INTERVAL_NUM) 
		sn += patt.tab[k].esti[l].n;

	      n++;

	      if (sn < N_SUB) {
		int si = patt.sub_index(k);

		// copy values to sub_config

		FOR (l, Entry::INTERVAL_NUM) {

		  if (patt.p_sub->tab[si].p_map->esti[l].y != 0.0 &&
		      patt.p_sub->tab[si].p_map->esti[l].y != patt.tab[k].esti[l].y)
		    Error("different?");

		  patt.p_sub->tab[si].p_map->esti[l].y = 
		    patt.tab[k].esti[l].y;
		}

		// pointer to sub_config

    	        patt.tab[k].p_map = patt.p_sub->tab[si].p_map;
		sub_n++;
	      }

#if 0
	      Pattern::conf_write(stdout, patt.len, k);
	      cout << " " << sn << endl;
#endif

	    }
	  }

	  // pointer shortcut: max. distance is 1

	  for (k=Pot3[patt.len]-1; k >= 0; k--) {

	    Entry *p = patt.tab[k].p_map;

	    while (p != p->p_map) { p = p->p_map; }

	    patt.tab[k].p_map = p;
	  }

	  cout << " " << n-sub_n << "/" << n << endl;

	}
      }


      if (argi < argc && !strcmp(argv[argi], "-freq")) {

	// generate freq files

	argi++;
	if (argi >= argc) Error("n?");

	int n = atoi(argv[argi]);

	if (n <= 0) Error("n <= 0");

	cout << "writing freq-files (n=" << n << ")..."  << flush;

	FOR (i, PATT_NUM) {

	  Pattern &patt = patterns[i];
	  char name[1000];

	  sprintf(name, "patt_%s.freq", patt.name);
	  fp=fopen(name,"w");
	  if (!fp) Error("can't write pattern-freq file");
          patt.bin_write_freq(fp, n);
	  fclose(fp);

	}
	
	FOR (i, FEAT_NUM) {
	  Feature &feat = features[i];
	  char name[1000];

	  sprintf(name, "feat_%s.freq", feat.name);
	  fp=fopen(name,"w");
	  if (!fp) Error("can't write feature-freq file");
          feat.bin_write_freq(fp, n);
	  fclose(fp);
	}
	
	cout << "OK" << endl;
	exit(0);
      }
 
    }


    // update all values

    double delta_abs_sum=0, max_abs_delta = 0;
    int    delta_n=0;

    FOR (i, PATT_NUM) {

      Pattern &patt = patterns[i];

      for (k=Pot3[patt.len]-1; k >= 0; k--) {
	if (patt.tab[k].p_map == &patt.tab[k]) {
	  double d = abs(patt.tab[k].update());
	  if (d != 0) {
	    max_abs_delta = max(d, max_abs_delta);
	    delta_abs_sum += d;
	    delta_n++;
	  }
	}
      }
    }

    FOR (i, FEAT_NUM) {
      Feature &feat = features[i];

      for (k=feat.bucket_num-1; k >= 0; k--) {

	double d = abs(feat.entries[k].update());
	if (d != 0) {
	  max_abs_delta = max(d, max_abs_delta);
	  delta_abs_sum += d;
	  delta_n++;
	}
      }
    }


    // print statistics

    int i;
    
    printf("%-8d d=%.3f ld=%.3f mld=%.3f \n",
	   iter, 
	   delta_sum / num,
	   delta_abs_sum / (delta_n+0.0001),
	   max_abs_delta);

    FOR (i, FEAT_NUM) {
      cout << "Feature(" << features[i].bucket_num << ") " << features[i].name << endl;
      FOR (k, features[i].bucket_num) {
	int l;
	cout << setw(2) << k << ": " << flush;
	FOR (l, Entry::INTERVAL_NUM) 
	  printf("%+5.2f ", features[i].entries[k].get_y(l));
	puts("");
      }
    }

#if 0
    patterns[2].asc_write(stdout);


    printf("%f %f %f %f\n", 
	   patterns[2].tab[28795].get_y(0), 
	   patterns[2].tab[0].get_y(0),
	   patterns[2].tab[P12(0,0,0,0,0,0,0,0,0,0,0,0)].get_y(0),
	   patterns[2].tab[P12(0,1,1,1,1,0,1,1,1,1,1,1)].get_y(0));
#endif


    if (USE_TEST_BOARDS) {

      // evaluate test boards

      int test_correct[Entry::INTERVAL_NUM], test_num[Entry::INTERVAL_NUM];
      float disco[Entry::INTERVAL_NUM];
      double d_abs_sum=0;
      int d_n=0;

      FOR (i, Entry::INTERVAL_NUM) {
	test_correct[i] = test_num[i] = 0;
	Values all0, all1;
	
	FOR (j, RES_NUM)
	  FOR (k, DISCOR_BUCKET_SIZE) {
	    BoardInfo &inf = dinfo[i][j][k];
	  
	    inf.val2 = inf.evaluate();

	    //	  cout << inf.val2 << " " << inf.val << endl;

	    d_abs_sum += abs(inf.val2 - inf.val);
	    d_n++;

	    if (inf.result) {
	      if (inf.result < 0) 
		all0.push_back(inf.val2);
	      else
		all1.push_back(inf.val2);
	      
	      if ((inf.val2 > 0) ^ (inf.result < 0)) 
		test_correct[i]++;
	      test_num[i]++;
	    }
        }
	
	disco[i] = discordance(all0, all1);
	
      }


      // compute discordance

      cout << "s" << DISCO_STEP << " test disco train " << flush;

      FOR (j, RES_NUM-DISCO_STEP/2) printf("%+3d   ", RES_MIN+2*j);
      puts("");

      for (i=Entry::DISC_MIN; i <= Entry::DISC_MAX; i++) {

	cout << i << "  " << flush;
	printf("%4.1f ", 100-100*test_correct[i]/(test_num[i]+0.001));
	printf("%4.1f ", 100*disco[i]);      
	printf("%4.1f ", 100-100*training_correct[i]/(training_num[i]+0.001));
	fflush(stdout);
	
	FOR (j, RES_NUM-DISCO_STEP/2) {

	  Values val0, val1;

	  FOR (k, DISCOR_BUCKET_SIZE) {
	    val0.push_back(dinfo[i][j][k].val2);
	    val1.push_back(dinfo[i][j+DISCO_STEP/2][k].val2);
	  }
	  
	  printf("%5.1f ", 100.0*discordance(val0, val1));
	  
	  //FOR (k, DISCOR_BUCKET_SIZE) cout << val0[k] << " " << val1[k] << endl;
	}
	
	puts("");
      }

      printf("\n");

    } else {

      cout << "  #   err   diff" << endl;
      FOR (i, Entry::INTERVAL_NUM) {
	cout << setw(2) << Entry::min_num(i) << "-" 
	     << setw(2) << Entry::max_num(i) << " " << flush;
	printf("%4.1f  ", 100-100*training_correct[i]/(training_num[i]+0.001));
	printf("%5.3f\n", delta_sums[i]/(delta_ns[i]+0.001));
      }
    }

    cout << endl;

    if (WRITE_DATA) {

      cout << "\nwriting ..." << flush;
  
      FOR (i, PATT_NUM) {

	char name[1000];
      
	sprintf(name, "patt_%s.bin", patterns[i].name);

	fp=fopen(name,"w");
	if (!fp) Error("can't write pattern file");
      
	if (ASC_WRITE)
	  patterns[i].asc_write(fp);
	else
	  patterns[i].bin_write(fp);
	
	fclose(fp);
      }
      
      FOR (i, FEAT_NUM) {
	
	char name[1000];
	
	sprintf(name, "feat_%s.bin", features[i].name);
	
	fp=fopen(name,"w");
	if (!fp) Error("can't write feature file");
	
	if (ASC_WRITE)
	  features[i].asc_write(fp);
	else
	  features[i].bin_write(fp);
	
	fclose(fp);
      }
      
      cout << "OK" << endl << endl;
    }

    if (finished) break;
  }


  return 0;
}

