// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// compute all patterns that occur at least N times 2.98 / under construction

#include "main.h"
#include <vector.h>
#include <algo.h>
#include "sboard.h"
#include "crt.h"
#include "trans.h"
#include "psearchm.h"
#include "pssub.h"
#include "goodies.h"
#include "Rnd.H"

const int  N = 100;


const int MAX_NUM = 3000;  // for testing

const char FILE_NAME[] = "end.sfk";
const char BIN_FILE[]  = "tab4.bin.copy";
const int  INFO_MAXNUM = 30000;
const int  MIN_NUM     = 3000*8;

#undef DISC_MIN
#undef DISC_MAX

const int  DISC_MIN = 56;
const int  DISC_MAX = 61;
const int  DISC_NUM = (DISC_MAX-DISC_MIN+1);

const int  BLACK_IND = 2;
const int  EMPTY_IND = 1;
const int  WHITE_IND = 0;

#define STAT fast_stat


const int TEST_NUM = 20000;   // number of boards in each class (discordance);



typedef vector<int> Values;

class Boards {
public:
  SPFELD b[8];
};


class Stat {

  static int curr_id;

public:

  int n;
  int res_sum;
  int last_id;


  Stat() { n = res_sum = last_id = 0; }

  inline void update(int res)
  {
    if (last_id != curr_id) {
      n++;
      res_sum += res;
      last_id = curr_id;
    }
  }

  static void next_id()
  {
    curr_id++;
    if (curr_id > 2000000000) Error("next_id: id overflow");
  }

};

int Stat::curr_id = 1;


class Patt3 {

public:

  int i[3];
  Stat stat[27];
  float discor;

  inline void update(int offset, int res)
  {
    assert(offset >= 0 && offset <= 26);
    stat[offset].update(res);
  }

  void write(int j)
  {
    int n = j, k;
    int c[4];

    for (k=2; k >= 0; k--) { c[k] = n % 3 -1; n /= 3; }

    FOR (k, 3) {
      char s[10];

      sKoorAus(s, i[k]); 
      cout << s << "=";
      if      (c[k] <  0) cout << "o";
      else if (c[k] == 0) cout << "-";
      else                cout << "x";
      cout << " ";
    }
  }
};


int Config::glob_res_sum=0;
int Config::glob_n=0;


static int MaxTrans(SPFELD *psf)
{
  int   max=0, j, i, a;
  int   *trj, *trmax;
  sint1 *p=psf->p;

  trmax = TransTab[max];

  for (j=1; j < 8; j++) {

    trj = TransTab[j];

    FOR_SFPOS10 (i) {
      a = p[trj[i]] - p[trmax[i]];
      if (a > 0) { max = j; trmax = TransTab[max]; break; }
      if (a < 0) break;
    }
  }

  return TransInv[max];
}



bool Config::maximal()
{
  SPFELD bo;
  sint1  *p=bo.p;
  ULL m = 1;
  int x, y;

  FOR (y, 8) {
    FOR (x, 8) {
      int i = y*10+x+11;
      if (!(mask.bits & m))
	p[i] = 0;
      else {
	if (black.bits & m)
	  p[i] = 3;
	else if (white.bits & m)
	  p[i] = 1;
	else
	  p[i] = 2;
      }
      m <<= 1;
    }
  }
  return MaxTrans(&bo) == 0;
}


void Config::transform(Config cons[8])
{
  int i;
  SPFELD bo, bos[8];

  cons[0] = *this;
  to_board(bo);
  ::Transform(&bo, bos);
  for (i=1; i < 8; i++) cons[i].from_board(bos[i]);
}


void Config::to_board(SPFELD &bo)
{
  int x, y;  
  ULL ma = 1LL;
  sint1 *p = bo.p;

  SfGrund(&bo);

  FOR (y, 8) {
    FOR (x, 8) {
      int ind = y * 10 + x + 11;
      if (mask.bits & ma) {
	if (black.bits & ma) 
	  p[ind] = 3;
	else if (white.bits & ma) 
	  p[ind] = 1;
	else
	  p[ind] = 2;
      } else 
	p[ind] = 0;

      ma <<= 1;
    }
  }
}  


void Config::from_board(SPFELD &bo)
{
  int x, y;  
  ULL ma = 1LL;
  sint1 *p = bo.p;

  mask.bits = black.bits = white.bits = 0;
  size = 0;
  n = 0;
  res_sum = 0;

  FOR (y, 8) {
    FOR (x, 8) {
      int ind = y * 10 + x + 11;

      switch (p[ind]) {

      case 0:  // nothing
	break;

      case 1:  // white
	size++;
	mask.bits  |= ma;
	white.bits |= ma;
	break;

      case 2:  // empty
	size++;
	mask.bits  |= ma;
	break;

      case 3:  // black
	size++;
	mask.bits  |= ma;
	black.bits |= ma;
	break;

      default: KoorAus(ind); cerr << int(p[ind]) << endl; Error("case?");

      }

      ma <<= 1;
    }
  }
}  


ostream &operator << (ostream &os, Config &conf)
{
  int x, y;
  ULL mask = 1LL;

  cout << int(conf.size) << " |";

  FOR (y, 8) {
    FOR (x, 8) {
      if (conf.mask.bits & mask) {
	if (conf.black.bits & mask) 
	  cout << "x";
	else if (conf.white.bits & mask) 
	  cout << "o";
	else
	  cout << "-";
      } else cout << "\267";

      mask <<= 1;
    }
    cout << '|';
  }

  cout << " " << conf.n
       << " " << double(conf.res_sum)/(conf.n+0.0001) - 
                 double(conf.glob_res_sum)/(conf.glob_n+0.0001) 
       << endl;

  return os;
}


bool operator < (const Config &conf1, const Config &conf2)
{
  if (conf2.n < conf1.n) return true;
  if (conf2.n > conf1.n) return false;

  if (conf2.mask.bits  < conf1.mask.bits)  return true;
  if (conf2.mask.bits  > conf1.mask.bits)  return false;
  if (conf2.black.bits < conf1.black.bits) return true;
  if (conf2.black.bits > conf1.black.bits) return false;
  if (conf2.white.bits < conf1.white.bits) return true;
  if (conf2.white.bits > conf1.white.bits) return false;

  return false;
}


bool operator == (const Config &conf1, const Config &conf2)
{
  return conf1.n == conf2.n && conf1.res_sum == conf2.res_sum;
}


void _abort(void) { exit(20); }


static int sq(char *s)
{
  return 11+(s[0] - 'a')+10*(s[1] - '1');
}

static int co(char *s)
{
  if      (s[3] == 'x') return 1;
  else if (s[3] == '-') return 0;
  else if (s[3] == 'o') return -1;
  else Error("illegal character in co()");
  return 0;
}


static float discordance(Values &val0, Values &val1)
{
  int i, j, num0=val0.size(), num1=val1.size();
  int su;

  sort(val0.begin(), val0.end());
  sort(val1.begin(), val1.end());

#if 0
  FOR (i, num0) cout << val0[i] << " ";
  cout << endl;
  
  FOR (i, num1) cout << val1[i] << " ";
  cout << endl;
#endif

  // count pairs with val1 >= val2 but cl1=loss and cl2 = win

  su = 0; i = 0;

  FOR (j, num0) {
    while (i < num1 && val0[j] >= val1[i]) i++;
    su += i;
  }

  //  cout << "s=" << su << " d=" << double(su) / (num0*num1) << endl;

  return double(su) / (num0*num1);
}

 
int value(Patt3 &patt, Boards &b)
{
  int v = 0, t;
  register int i0 = patt.i[0];
  register int i1 = patt.i[1];
  register int i2 = patt.i[2];
  register sint1 *p 
#ifdef NDEBUG
#if 1 && __i386__
asm("%edi")
#endif
#endif
  ;

  t = 7;

  do {

    p = b.b[t].p;
    v += patt.stat[13 + 3*(3*p[i0]+p[i1])+p[i2]].res_sum;

  } while (--t >= 0);

  return v;
}


int main(int argc, char **argv)
{
  FILE *fp;
  vector<Patt3> patt3;
  long long res_sum = 0;
  int num, ind, i, j;

#if 0

  float P = 1/6000.0;
  int n;
  int a[11];
  
  FOR (i, 11) a[i] = 0;

  FOR (n, 10000) {

    int sum = 0;
    
    FOR (i, 24000) {
      if (Rnd::Uni01() < P) sum++;
    }

    if (sum > 10) sum = 10;

    a[sum]++;

    if ((n % 100) == 0) {
      FOR (i, 11) 
	cout << "(" << i << ":" << a[i] << " " << float(a[i])/(n+.0001) << ") " << endl;
      cout << endl;
    }


  }

  exit(0);


#endif


  InitCrt();

  // compute all 4-configs

  patt3.reserve(6000);

  ind = 0;

  FOR (i, 64)
    for (j=i+1; j < 64; j++) 
      for (int k=j+1; k < 64; k++) {
	  SPFELD bo;
	  SfGrund(&bo);
	  bo.p[Tab8to10[i]] = 1;
	  bo.p[Tab8to10[j]] = 1;
	  bo.p[Tab8to10[k]] = 1;
	  if (MaxTrans(&bo) == 0) {
	    Patt3 p3;
	    p3.i[0] = Tab8to10[i];
	    p3.i[1] = Tab8to10[j];
	    p3.i[2] = Tab8to10[k];

#if 0
int t;
FOR (t, 3) cout << p3.i[t] << " ";
cout << endl;
#endif

	    patt3.push_back(p3);
	    ind++;
	  }
	}

  cerr << ind << " 3-masks" << endl;

  // ******************************************************************* //

#if 0

  if (argc == 2 && !strcmp(argv[1], "-asc2bin")) {

    // read big ascii file from stdout and write binary file 'tab4.bin'

    int square[4], square_old[4];
    int patt_ind = -1;  // not def.

    for (i=0;; i++) {

      char s[4][1000], line[1000];
      int n1, j;
      char c;
      float val;

      if ((i & 16383) == 0) cout << i << '\r' << flush;

      if (!fgets(line, 999, stdin)) break;

      int r = sscanf(line, "%s %s %s %s %d %c %f", s[0], s[1], s[2], s[3], &n1, &c, &val);

      //      cout << ">>> " << r << " " << s1 << endl;

      if (r != 7) { 
	if (i != 0) { cerr << s << endl; Error("corrupt line"); }
	else continue;
      }

      FOR (j, 4) square[j] = sq(s[j]);
      FOR (j, 4) if (square[j] != square_old[j]) break;

      if (patt_ind < 0 || j < 4) {

	// search pattern

	for (; patt_ind < ind; patt_ind++) {
	  FOR (j, 4) if (square[j] != patt4[patt_ind].i[j]) break;
	  if (j >= 4) break;
	}
	  
	if (patt_ind >= ind) {

	  FOR (patt_ind, ind) {
	    FOR (j, 4) if (square[j] != patt4[patt_ind].i[j]) break;
	    if (j >= 4) break;
	  }

	  if (patt_ind >= ind) Error("pattern not found");
	}
      }

      int k = 0;

      FOR (j, 4) k = 3*k + co(s[j]);
      k += 40;

      if      (c == '-') val = -val;
      else if (c != '+') Error("no +-");

      if (patt4[patt_ind].stat[k].n) Error("double occurrence");
      patt4[patt_ind].stat[k].n = 1;
      patt4[patt_ind].stat[k].res_sum = int(round(100*val));
      //      cout << patt_ind << " " << k << " " << patt4[patt_ind].stat[k].res_sum << endl;

    }

    cout << "writing to " << BIN_FILE << " ... " << flush;

    fp = fopen(BIN_FILE, "w");
    if (!fp) Error("can't open file");

    FOR (i, patt4.size()) {
      FOR (j, 81) {
	int v = patt4[i].stat[j].res_sum;
	fputc(v & 255, fp);
	fputc(v >> 8, fp);
      }
    }

    fclose(fp);
    exit(0);

  }

#endif


  // ******************************************************************* //

#if 0

  if (argc == 3 && !strcmp(argv[1], "-discor")) {

    SPFELD bo;
    vector<Boards> bo0, bo1;

    // read test examples

    fp = fopen(argv[2], "r");
    if (!fp) Error("can't open  sfk-file");
    
    for (i=0; i < TEST_NUM;) {
      if (!fSfRead(fp, &bo, 1)) break;
      
      int val = bo.Marke;
      if (val < MA_DIFF || val > MA_DIFF+128) Error("no diff-label");
      val -= MA_DIFF+64;
      if (val < 0) { 
	int t;
	SPFELD bos[8];
	Boards b;

	Transform(&bo, bos);
	FOR (t, 8) b.b[t] = bos[t];
	bo0.push_back(b);	
	i++; 
      }
    } 

    if (i < TEST_NUM) Error("too few examples");

    for (i=0; i < TEST_NUM;) {
      if (!fSfRead(fp, &bo, 1)) break;
      
      int val = bo.Marke;
      if (val < MA_DIFF || val > MA_DIFF+128) Error("no diff-label");
      val -= MA_DIFF+64;
      if (val > 0) { 
	int t;
	SPFELD bos[8];
	Boards b;

	Transform(&bo, bos);
	FOR (t, 8) b.b[t] = bos[t];
	bo1.push_back(b);	
	i++; 
      }
    } 

    if (i < TEST_NUM) Error("too few examples");

    fclose(fp);


    // read bin-file

    fp = fopen(BIN_FILE, "r");
    if (!fp) Error("can't open tab-file");

    FOR (i, patt4.size()) {
      FOR (j, 81) {
	short c = fgetc(fp);
	c += fgetc(fp) * 256;
	if (feof(fp)) Error("end of file");
	patt4[i].stat[j].res_sum = c;
      }
    }

    fgetc(fp);
    if (!feof(fp)) Error("file too long");
    fclose(fp);


    // compute discordances

    Values val0(TEST_NUM), val1(TEST_NUM);

    FOR (i, ind) {

      FOR (j, TEST_NUM) {
	val0[j] = value(patt4[i], bo0[j]);
	val1[j] = value(patt4[i], bo1[j]);
      }

      patt4[i].discor = discordance(val0, val1);
      patt4[i].write(0); cout << " " << patt4[i].discor << endl;
    }

    exit(0);
  }

#endif

  // ******************************************************************* //

  if (argc != 1) Error("option?");
 
  fp = fopen(FILE_NAME, "r");
  if (!fp) Error("can't open file");

  Config::glob_n = 0;
  Config::glob_res_sum = 0;

  for (num=0; num < MAX_NUM; num++) {

    SPFELD bo;

    if ((num & 15) == 0) cerr << num << "\r" << flush;

    if (!fSfRead(fp, &bo, 1)) break;

    Stat::next_id();

    int val = bo.Marke;

    if (val < MA_DIFF || val > MA_DIFF+128) {
      Error("no diff-label");
    }

    int result = bo.Marke - (MA_DIFF+64);

    res_sum += result;

    SPFELD bos[8];
    int t;
 
    Transform(&bo, bos);

    FOR (t, 8) {

      // update configs
      
      sint1 *p = bos[t].p;
    
      register Patt3 *pp3 
#ifdef NDEBUG
#if 1 && __i386__
asm("%edi")
#endif
#endif
	= &patt3[0];

      register int k
#ifdef NDEBUG
#if 1 && __i386__
asm("%esi")
#endif
#endif
	= ind;

      //      printf("%d", k);

      do {

	pp3->update(13 + 3*(3*p[pp3->i[0]]+p[pp3->i[1]])+p[pp3->i[2]], result);
	pp3++;
 
      } while (--k);
    }
  }
   
  fclose(fp);

  cerr << num << endl;

  float avg_dd = double(res_sum)/num;

  cout << "avg-dd=" << avg_dd << endl;

  FOR (i, ind) {

    FOR (j, 27) {

      patt3[i].write(j);
	
      cout << " " << patt3[i].stat[j].n << " ";

      float dd = double(patt3[i].stat[j].res_sum)/(patt3[i].stat[j].n+0.0001) - avg_dd;
      
      if (dd >= 0) 
	cout << "+";
      else
	cout << "-";
      
      cout << " " << abs(dd) << endl;
    }
  }

  return 0;
}
