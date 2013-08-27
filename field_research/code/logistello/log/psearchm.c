// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// search patterns / 1.97 / 2.98

#include "main.h"
#include <vector.h>
#include <algo.h>
#include "sboard.h"
#include "crt.h"
#include "trans.h"
#include "psearchm.h"
#include "pssub.h"

const int  INFO_MAXNUM  = 1600000;
const int  MIN_NUM      = 30000;      // 400
const int  MIN_STAT_NUM = 20;
const int  CONFIG_MAX   = 3000000;

const bool LAZY      = true;  // use probabilistic algo. for check
const bool CONNECTED = true;  // only generate connected patterns, buggy with custom patterns

#define START1 true

const bool MAXIMUM_TEST = START1;

const int  BLACK_IND = 2;
const int  EMPTY_IND = 1;
const int  WHITE_IND = 0;

#define STAT fast_stat

#if START1 

SPFELD all_squares = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,0,1,1,0,1,1,0,
  0,1,1,0,1,1,0,1,1,0,
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0
}
};


#else

#error "bla"

#if 1

// 4x4 corner

SPFELD start_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};


SPFELD ext_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,  
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};

SPFELD all_squares = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,1,1,1,1,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};

#endif


#if 0

// 2x8 edge

SPFELD start_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};


SPFELD ext_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,  
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};

SPFELD all_squares = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};

#endif


#if 0

// 4* 4321

SPFELD start_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};


SPFELD ext_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,  
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0
}
};

SPFELD all_squares = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,1,1,0,0,1,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0
}
};

#endif

#if 0

// 4* edge+x

SPFELD start_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0
}
};

SPFELD ext_bo = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,  
  0,1,1,0,0,0,0,1,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0
}
};

SPFELD all_squares = {
{
  0,0,0,0,0,0,0,0,0,0,
  0,1,1,1,1,1,1,1,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,0,0,0,0,0,0,1,0,
  0,1,1,0,0,0,0,1,1,0,
  0,1,1,1,1,1,1,1,1,0,
  0,0,0,0,0,0,0,0,0,0
}
};

#endif

#endif   // START1


long long step_n, step_sum, ok_n, stat_n;


class BitInfo {

public:

  vector<uint4> bits[3*64];
  int index;
  uint4 mask;

  BitInfo() { 
    int i;
    index = 0; mask = 1;
    FOR (i, 192) {
      bits[i].reserve(INFO_MAXNUM/32+2);
      bits[i].push_back(0);
    }
  }

  void set(int i, bool bit) 
  {
    if (bit) bits[i][index] |=  mask;
    else     bits[i][index] &= ~mask;
  }

  void next()
  {
    mask <<= 1;
    if (!mask) {
      mask = 1;
      index++;
      int i;
      FOR (i, 192) bits[i].push_back(0);
    }
  }

} bit_info;


ostream &operator << (ostream &os, BoardInfo &info)
{
  int x, y;
  ULL mask = 1LL;

  FOR (y, 8) {
    FOR (x, 8) {
      if (info.black.bits & mask) 
	os << "x";
      else if (info.white.bits & mask) 
	os << "o";
      else
	os << "-";
      mask <<= 1;
    }
    os << endl;
  }

  os << endl << "res= " << int(info.result) << endl;

  return os;
}

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

  os << int(conf.size) << " : " << conf.pred << " |";

  FOR (y, 8) {
    FOR (x, 8) {
      if (conf.mask.bits & mask) {
	if (conf.black.bits & mask) 
	  os << "x";
	else if (conf.white.bits & mask) 
	  os << "o";
	else
	  os << "-";
      } else os << "\267";

      mask <<= 1;
    }
    os << '|';
  }

  os << " " << conf.n;
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
  return conf1.n == conf2.n;
}


void _abort(void) { exit(20); }



#if 0

static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}

static void WritePatt(FILE *fp, int n, int discnum)
{
  int i, r, Cont[20];

  if (discnum > 20) Error("WritePatt: too many discs");

  FOR (i, discnum) {

    r = n % 3;
    
    if      (r == 0) Cont[i] = WHITE;
    else if (r == 1) Cont[i] = LEER;
    else             Cont[i] = BLACK;

    n /= 3;
  }

  FOR (i, discnum) WriteDisc(fp, Cont[discnum-1-i]);
}
  
#endif



void slow_stat(Config &conf, vector<BoardInfo> &infos, int, int)
{
  int t;
  Config conft[8];

  conf.n = 0;
  conf.transform(conft);

  for (vector<BoardInfo>::iterator i=infos.begin(); i != infos.end(); i++) {

    FOR (t, 8) {
      if (conft[t].match(*i)) conf.n++;
    }
  }
}


#if 0

// max stuff

void fast_stat(Config &conf, vector<BoardInfo> &infos)
{
  conf.n = 0;
  int pati[64];
  int sq_num=0;
  ULL mask = 1LL;
  int i;

  //cout << "config: " << endl << conf << endl;

  FOR (i, 64) {
    if (conf.mask.bits & mask) {
      if (conf.black.bits & mask) 
	pati[sq_num++] = BLACK_IND*64+i;
      else if (conf.white.bits & mask) 
	pati[sq_num++] = WHITE_IND*64+i;
      else 
	pati[sq_num++] = EMPTY_IND*64+i;
    }
    mask <<= 1;
  }


  //FOR (i, sq_num) cout << pati[i] << " ";
  // cout << endl;


  i = 0;

  //step_n = 0;

  stat_n++;

  while (i < infos.size()) {

    step_n++;

    int max = 0, j;
    sint1 *disp = &infos[i].disp[0];
    bool match = true;

    FOR (j, sq_num) {
      int a = disp[pati[j]];
      // if (sq_num > 2) cout << a << " " << flush;
      if (a > 0) match = 0;
      a = abs(a);
      if (a > max) max = a;
    }

    // cout << infos[i] << endl;
    // cout << i << ": " << max << endl;

    //    if (sq_num > 2) cout << max << " " << match << endl;

    if (match) conf.n++;
    ok_n+=match;

    i += max;
    step_sum+=max;
  }

  //  cout << step_n << " of " << infos.size() << " " << 100*step_n/infos.size() << endl;

#if 0

  Config conf1 = conf;

  slow_stat(conf1, infos);
  
  if (conf != conf1) {
    cout << conf1 << conf1 << endl;
  }

#endif

}

#endif




// with bit_info

void fast_stat(Config &conf0, vector<BoardInfo> &infos, int min_num, int total_num)
{
  uint4 *pati[64];
  int i, t;
  Config conft[8];
  static int checked = 0, check_n = 0;
  static int errors = 0;

  conf0.n = 0;

  //cout << "config: " << endl << conf << endl;

  conf0.transform(conft);

#if 1

  // fast probabilistic check (requires randomly permuted boards)

  if (LAZY && min_num > 0 && conf0.size > 3) {
    
    const int step_size = 128;
    int start_index;

    for (start_index=0; start_index < bit_info.index+1; start_index += step_size) {

      FOR (t, 8) {

	Config &conf = conft[t];
	//    cout << "&&& " << conf;
	
	conf.n = 0;
	int sq_num=0;
	ULL mask = 1LL;

	FOR (i, 64) {
	  if (conf.mask.bits & mask) {
	    if (conf.black.bits & mask) 
	      pati[sq_num++] = &bit_info.bits[BLACK_IND*64+i][start_index];
	    else if (conf.white.bits & mask) 
	      pati[sq_num++] = &bit_info.bits[WHITE_IND*64+i][start_index];
	    else 
	      pati[sq_num++] = &bit_info.bits[EMPTY_IND*64+i][start_index];
	  }
	  mask <<= 1;
	}
	
	//  cout << "index=" << bit_info.index << endl;

	int number = step_size;
	if (bit_info.index+1-start_index < step_size) 
	  number = bit_info.index+1-start_index;
	
	if (sq_num == 1)
	  check1(conf, number, pati);
	else if (sq_num == 2) 
	  check2(conf, number, pati);
	else if (sq_num == 3) 
	  check3(conf, number, pati);
	else
	  checkn(sq_num, conf, number, pati);
	
	//    cout << "### " << conf.n;
	
	conf0.n += conf.n;
      }

      if (start_index + step_size <  bit_info.index+1 - 500) {

	const float CT = 0.35 * 8;  // error < 0.1% for 1000/160K
	int n = (start_index + step_size) * 32;
	double P = double(min_num)/Config::glob_n;
	double s = CT * sqrt(n * P * (1-P));

	// X in 0..8 => 64 = 8*8 is maximal variance factor

	if (conf0.n >= n * P + s) {
#if 0
	  cout << "+ " << n << " " << conf0.n << " " << double(conf0.n)/n << " " 
	       <<  n * P << "+" << s << endl;
#endif

#if 0
	  Config conf1 = conf0;
	  fast_stat(conf1, infos, -1, -1);
	  if (conf1.n < min_num) {
	    cout << "+" << flush;
	    errors++;
	    conf0.n = conf1.n;
	    break;
	  }
#endif

	  conf0.n = min_num; break;
	}

	if (conf0.n <= n * P - s) {
#if 0
	  cout << "- " << n << " " << conf0.n << " " << double(conf0.n)/n << " "
	       <<  n * P << "-" << s << endl;
#endif

#if 0
	  Config conf1 = conf0;
	  fast_stat(conf1, infos, -1, -1);
	  if (conf1.n >= min_num) {
	    cout << "-" << flush;
	    errors++;
	    conf0.n = conf1.n;
	    break;
	  }
#endif

	  conf0.n = 1; break; 
	}

      }

    }

#if 0
    if (start_index + step_size >= bit_info.index+1)
      checked += Config::glob_n;
    else
      checked += (start_index + step_size) * 32;

    check_n++;

    if ((check_n & 255) == 0) {
      cout << check_n << " " << float(checked)/check_n << " " 
	   << float(errors*100)/check_n << endl;
    }
#endif

#if 0
    
    Config conf1 = conf0;
    
    slow_stat(conf1, infos);
    
    if (conf0.n != conf1.n) {
      cout << conf0 << conf1 << endl;
    }
    
#endif

    return;
  }

#endif


  FOR (t, 8) {

    Config &conf = conft[t];
    //    cout << "&&& " << conf;

    conf.n = 0;
    int sq_num=0;
    ULL mask = 1LL;

    FOR (i, 64) {
      if (conf.mask.bits & mask) {
	if (conf.black.bits & mask) 
	  pati[sq_num++] = &bit_info.bits[BLACK_IND*64+i][0];
	else if (conf.white.bits & mask) 
	  pati[sq_num++] = &bit_info.bits[WHITE_IND*64+i][0];
	else 
	  pati[sq_num++] = &bit_info.bits[EMPTY_IND*64+i][0];
      }
      mask <<= 1;
    }
  
    //  cout << "index=" << bit_info.index << endl;
    
    if (sq_num == 1)
      check1(conf, bit_info.index+1, pati);
    else if (sq_num == 2) 
      check2(conf, bit_info.index+1, pati);
    else if (sq_num == 3) 
      check3(conf, bit_info.index+1, pati);
    else
      checkn(sq_num, conf, bit_info.index+1, pati);

    //    cout << "### " << conf.n;

    conf0.n += conf.n;

#if 0
    
    Config conf1 = conf;
    
    slow_stat(conf1, infos);
    
    if (conf.n != conf1.n) {
      cout << conf1 << conf1 << endl;
    }
    
#endif
  }

  //cout << conf0;  
}

// with bit_info and n-pattern pre-computation

void new_fast_stat(Config &conf0, vector<BoardInfo> &infos, int min_num, int total_num,
		   Config &orig, int new_square, int contents, bool first_call)
{
  int i, t;
  Config conft[8];
  uint4 *pati[64];
  static int checked = 0, check_n = 0;
  static int errors = 0;

  static uint4 max_len = 0;
  static int curr_len = 0;
  static uint4 *pre[8];
  static uint4 *orig_pati[8][64];

  // cout << "new_fast" << endl;

  if (first_call) {      // first call with new original configuration
    curr_len = 0;

    if (max_len == 0) {  // very first call => allocate lists
      max_len = INFO_MAXNUM/32+1;

      if (max_len * 32 < infos.size()+64) Error("max_len too small");

      FOR (i, 8) pre[i] = new uint4[max_len];
    }

    // pointers to all bit lists for 8 transformations of all original squares

    orig.transform(conft);

    FOR (t, 8) {

      Config &conf = conft[t];
	
      conf.n = 0;
      int sq_num=0;
      ULL mask = 1LL;
      
      FOR (i, 64) {
	if (conf.mask.bits & mask) {
	  if (conf.black.bits & mask) 
	    orig_pati[t][sq_num++] = &bit_info.bits[BLACK_IND*64+i][0];
	  else if (conf.white.bits & mask) 
	    orig_pati[t][sq_num++] = &bit_info.bits[WHITE_IND*64+i][0];
	  else 
	    orig_pati[t][sq_num++] = &bit_info.bits[EMPTY_IND*64+i][0];
	}
	mask <<= 1;
      }
    }
  }

  conf0.n = 0;

  //cout << "config: " << endl << conf << endl;


#if 1

  // fast probabilistic check (requires randomly permuted boards)

  if (LAZY && min_num > 0 && conf0.size > 3) {
    
    const int step_size = 128;
    int start_index;
    static uint4 gen_num = 0, test_num = 0, tests = 0;

    for (start_index=0; start_index < bit_info.index+1; start_index += step_size) {

      int number = step_size;
      if (bit_info.index+1-start_index < step_size) 
	number = bit_info.index+1-start_index;
	
      if (start_index + number > curr_len) {

	// generate further n-intersections

	if (start_index != curr_len) {
	  cerr << start_index << " " << curr_len << endl;
	  Error("start_index != curr_len");
	}

	FOR (t, 8) genn(orig.size, number, orig_pati[t], &pre[t][curr_len]);
	curr_len += number;

	gen_num += number;
      }

      test_num += number;
      tests++;

      if ((tests & 65535) == 0) {
	cout << "tests: " << tests << " " << double(test_num)/(gen_num+0.0001) << endl;
      }

      // compute next intersection bit numbers

      FOR (t, 8) {

	pati[0] = &pre[t][start_index];
	pati[1] = &bit_info.bits[contents*64+Tab10to8[Trans[t](Tab8to10[new_square])]][start_index];

	check2(conf0, number, pati);
	
	//    cout << "### " << conf.n;
      }

      if (start_index + step_size <  bit_info.index+1 - 500) {

	const float CT = 0.35 * 8;  // error < 0.1% for 1000/160K
	int n = (start_index + step_size) * 32;
	double P = double(min_num)/Config::glob_n;
	double s = CT * sqrt(n * P * (1-P));

	// X in 0..8 => 64 = 8*8 is maximal variance factor

	// cout << "p=" << (double)conf0.n/n << endl;

	if (conf0.n >= n * P + s) {
#if 0
	  cout << "+ " << n << " " << conf0.n << " " << double(conf0.n)/n << " " 
	       <<  n * P << "+" << s << endl;
#endif

#if 0
	  Config conf1 = conf0;
	  fast_stat(conf1, infos, -1, -1);
	  if (conf1.n < min_num) {
	    cout << "+" << flush;
	    errors++;
	    conf0.n = conf1.n;
	    break;
	  }
#endif

	  conf0.n = min_num; break;
	}

	if (conf0.n <= n * P - s) {
#if 0
	  cout << "- " << n << " " << conf0.n << " " << double(conf0.n)/n << " "
	       <<  n * P << "-" << s << endl;
#endif

#if 0
	  Config conf1 = conf0;
	  fast_stat(conf1, infos, -1, -1);
	  if (conf1.n >= min_num) {
	    cout << "-" << flush;
	    errors++;
	    conf0.n = conf1.n;
	    break;
	  }
#endif

	  conf0.n = 1; break; 
	}

      }

    }


    // cout << (start_index+step_size)*32 << endl;

#if 0
    if (start_index + step_size >= bit_info.index+1)
      checked += Config::glob_n;
    else
      checked += (start_index + step_size) * 32;

    check_n++;

    if ((check_n & 255) == 0) {
      cout << check_n << " " << float(checked)/check_n << " " 
	   << float(errors*100)/check_n << endl;
    }
#endif

#if 0
    
    Config conf1 = conf0;
    
    slow_stat(conf1, infos);
    
    if (conf0.n != conf1.n) {
      cout << conf0 << conf1 << endl;
    }
    
#endif

    return;
  }

#endif

  FOR (t, 8) {

    Config &conf = conft[t];
    //    cout << "&&& " << conf;

    conf.n = 0;
    int sq_num=0;
    ULL mask = 1LL;

    FOR (i, 64) {
      if (conf.mask.bits & mask) {
	if (conf.black.bits & mask) 
	  pati[sq_num++] = &bit_info.bits[BLACK_IND*64+i][0];
	else if (conf.white.bits & mask) 
	  pati[sq_num++] = &bit_info.bits[WHITE_IND*64+i][0];
	else 
	  pati[sq_num++] = &bit_info.bits[EMPTY_IND*64+i][0];
      }
      mask <<= 1;
    }
  
    //  cout << "index=" << bit_info.index << endl;
    
    if (sq_num == 1)
      check1(conf, bit_info.index+1, pati);
    else if (sq_num == 2) 
      check2(conf, bit_info.index+1, pati);
    else if (sq_num == 3) 
      check3(conf, bit_info.index+1, pati);
    else
      checkn(sq_num, conf, bit_info.index+1, pati);

    //    cout << "### " << conf.n;

    conf0.n += conf.n;

#if 0
    
    Config conf1 = conf;
    
    slow_stat(conf1, infos);
    
    if (conf.n != conf1.n) {
      cout << conf1 << conf1 << endl;
    }
    
#endif
  }

  //cout << conf0;  
}




int main(int argc, char **argv)
{
  FILE *fp;
  vector<BoardInfo> infos;
  vector<Config>    configs;
  Config            extend_configs[64][3];

  int  num, ind, ext_num;

  bool octant[64] = {
    1,1,1,1,0,0,0,0,
    0,1,1,1,0,0,0,0,
    0,0,1,1,0,0,0,0,
    0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0
  };


  if (argc != 2) Error("usage: opsearch sfk-file");

  configs.reserve(CONFIG_MAX);

  fp = fopen(argv[1], "r");
  if (!fp) Error("can't open file");

  InitCrt();

  cout << "reading boards (MIN=" << MIN_NUM << ")..." << flush;


  Config::glob_n = 0;
  Config::glob_res_sum = 0;

  for (num=0, ind=0; ; num++) {

    SPFELD bo;

    if (!fSfRead(fp, &bo, 1)) break;

    //SfAus(&bo, 0, 0);

    int val = bo.Marke;

    if (val < MA_DIFF || val > MA_DIFF+128) {
      Error("no diff-label");
    }

    if (ind >= INFO_MAXNUM) break;

    BoardInfo inf;
    sint1 *p = bo.p;
    int sq, x, y;
    ULL mask = 1LL;

    inf.result = bo.Marke - (MA_DIFF+64);
    inf.black.bits = inf.white.bits = 0LL;

    Config::glob_n++;
    Config::glob_res_sum += inf.result;

    sq = 0;

#if 0
    memset(inf.disp, 1, sizeof(inf.disp));  // 1 <=> 1-square conf. does not occur

    FOR (y, 8) {
      FOR (x, 8) {
	int cont = p[y*10+x+11];
	if (cont == BLACK) {
	  inf.black.bits |= mask;
	  inf.disp[BLACK_IND*64+sq] = 0;
	} else if (cont == WHITE) {
	  inf.white.bits |= mask;
	  inf.disp[WHITE_IND*64+sq] = 0;
	} else {
	  inf.disp[EMPTY_IND*64+sq] = 0;
	}

	mask <<= 1;
	sq++;
      }
    }
#endif

    FOR (y, 8) {
      FOR (x, 8) {
	int cont = p[y*10+x+11];

	bit_info.set(BLACK_IND*64+sq, cont == BLACK);
	if (cont == BLACK) inf.black.bits |= mask;

	bit_info.set(WHITE_IND*64+sq, cont == WHITE);
	if (cont == WHITE) inf.white.bits |= mask;

	bit_info.set(EMPTY_IND*64+sq, cont == LEER);

	mask <<= 1;
	sq++;
      }
    }

    bit_info.next();
    infos.push_back(inf);
    ind++;
  }
  
  fclose(fp);

  cout << num << endl;


#if 0

  // scramble data

  FOR (i, infos.size()-1) {

    BoardInfo t = infos[i];

    int j = i + (IRAN % (infos.size()-i));

    infos[i] = infos[j];
    infos[j] = t;

  }

#endif


#if 0

  // init. disp.

  sint1 disp[3*64], *info_disp;
  int sum_disp[3*64], sumsq_disp[3*64];
  
  FOR (i, 3*64) sum_disp[i] = sumsq_disp[i] = 0;

  memset(disp, 127, sizeof(disp));  // max. displacement
  
  for (i=infos.size()-1; i >= 0; i--) {
    
    info_disp = &infos[i].disp[0];
    int k, l;

    FOR (k, 3) 
      FOR (l, 64) {
        int ind = k * 64 + l;
        if (info_disp[ind]) {
	  info_disp[ind] = disp[ind];
	  if (disp[ind] < 126) disp[ind]++;
	} else {
	  info_disp[ind] = -disp[ind];
	  disp[ind] = 1;
	}

	sum_disp[ind] += abs(info_disp[ind]);
	sumsq_disp[ind] += info_disp[ind] * info_disp[ind];
	
	//	cout << int(infos[i].disp[ind]) << " " << flush;
      }

    //cout << endl << endl;
  }

#endif





#if 0

  FOR (i, 3*64) {

    double E = double(sum_disp[i]) / infos.size();
    double V = double(sumsq_disp[i] / (infos.size()) - E*E);
    
    if (V < 0) V = 0;
     
    cout << "E=" << double(sum_disp[i]) / infos.size() 
         << " S=" << sqrt(V) << " " << V
         << endl;
  }

#endif



  // init. configlist with one-square configurations

  int start_len=0;
  ULL mask = 1LL;
  Config conf_ext;
  conf_ext.from_board(all_squares);

  int i = 0, bit_index = 0;

  while (mask) {

    if ((mask & conf_ext.mask.bits) != 0) {

      Config conf;
      conf.size = 1;
      conf.bit_index = bit_index;
      conf.mask.bits = mask;

      extend_configs[i][EMPTY_IND] = conf;
      if (START1 && octant[bit_index]) configs.push_back(conf);

      conf.black.bits = mask;
      extend_configs[i][BLACK_IND] = conf;
      if (START1 && octant[bit_index]) configs.push_back(conf);        

      conf.black.bits = 0LL;
      conf.white.bits = mask;
      extend_configs[i][WHITE_IND] = conf;
      if (START1 && octant[bit_index]) configs.push_back(conf);
      i++;
    }

    mask <<= 1; bit_index++;
  }

  ext_num = i;

  printf("ext_num=%d\n", ext_num);

#if !START1

    FOREVER {

      Config conf;

      conf.from_board(start_bo);
      start_len = conf.size;

      if (conf.maximal()) {
	STAT(conf, infos, MIN_NUM, Config::glob_n);

	if (conf.n >= MIN_NUM) {
	  configs.push_back(conf);
	  printf("+");
	} else
	  printf("-");
      
	fflush(stdout);
      }

      FOR (i, 64) {
	if (start_bo.p[i] != 0) {
	  start_bo.p[i]++;
	  if (start_bo.p[i] <= 3) break;
	  start_bo.p[i] = 1;
	}
      }

      if (i >= 64) break;
    }

#else 

    // determine values for start patterns

    start_len = 1;
    for (vector<Config>::iterator ci=configs.begin(); ci != configs.end(); ci++) {
      STAT(*ci, infos, -1, -1);
    }

#endif

  int prev_start = 0;

  for (int len=start_len+1;; len++) {

    // if (len == 10) break;

    step_n = step_sum = ok_n = stat_n = 0;

    cout << configs.size() << " configurations with size <= " << len-1 << endl;

#if 0

    sort(configs.begin(), configs.end());

    for (vector<Config>::iterator i = configs.begin(); i != configs.end(); i++) {
      cout << *i << endl;
    }

#endif

    // create new configs

    vector<Config> new_configs;

    int counter = 0;
    int end_index = configs.size();
    int ci;

    FOR (ci, end_index) {

      Config &conf_i = configs[ci];

      if (conf_i.size != len-1) continue;

      bool first_call = true;  // for new_fast_stat

      // cout << "first" << endl;

      static int count = 0;
      count++;

      if (!(count & 31)) {
	cout << count << " "
	     << int(double(ci-prev_start)*1000.0/(end_index-prev_start))/10.0
	     << " "
	     << double(configs.size()-end_index)/(ci-prev_start+0.001)
	     << endl;
      }

#if 0

      // no difference :(

      ULL sour;
      Config &conf = *ci;

      sour = conf.mask.bits |
	((conf.mask.bits << 1) & 0xfefefefefefefefeLL) |
	((conf.mask.bits << 9) & 0xfefefefefefefefeLL) |
	((conf.mask.bits >> 7) & 0xfefefefefefefefeLL) |
	((conf.mask.bits >> 1) & 0x7f7f7f7f7f7f7f7fLL) |
	((conf.mask.bits >> 9) & 0x7f7f7f7f7f7f7f7fLL) |
	((conf.mask.bits << 7) & 0x7f7f7f7f7f7f7f7fLL) |
        (conf.mask.bits << 8) |
        (conf.mask.bits >> 8);

      //      printf("%x %x %x %x\n", int(conf.mask.bits >> 32), int(conf.mask.bits & 0xffffffff),
      //	     int(sour >> 32), int(sour & 0xffffffff));

      ULL mask = 0x8000000000000000;

      for (int j=63; j >= 0; j--, mask >>= 1) {

	if (!(sour & mask)) continue;

	if (conf.mask.bits & mask) break;  // intersection => end


#else

      for (int j=ext_num-1; j >= 0; j-- ) {

        Config &conf1 = extend_configs[j][0];
	if (conf_i.mask.bits & conf1.mask.bits) break;  // intersection => end

	if (CONNECTED) {

	  ULL m;
	  bool connected = true;

	  FOREVER { 
	    m = conf1.mask.bits << 1;
	    if (!(m & 0x0101010101010101LL) && (m & conf_i.mask.bits)) break;
	    m = conf1.mask.bits << 9;
	    if (!(m & 0x0101010101010101LL) && (m & conf_i.mask.bits)) break;
	    m = conf1.mask.bits >> 7;			
	    if (!(m & 0x0101010101010101LL) && (m & conf_i.mask.bits)) break;
	    						
	    m = conf1.mask.bits >> 1;			
	    if (!(m & 0x8080808080808080LL) && (m & conf_i.mask.bits)) break;
	    m = conf1.mask.bits >> 9;			
	    if (!(m & 0x8080808080808080LL) && (m & conf_i.mask.bits)) break;
	    m = conf1.mask.bits << 7;			
	    if (!(m & 0x8080808080808080LL) && (m & conf_i.mask.bits)) break;
	    
	    m = conf1.mask.bits >> 8;
	    if ((m & conf_i.mask.bits)) break;
	    m = conf1.mask.bits << 8;
	    if ((m & conf_i.mask.bits)) break;
	    
	    connected = false;
	    break;
	  }
	  
	  if (!connected) continue;
	}

#endif

	for (int k=0; k < 3; k++) {

	  Config &conf1 = extend_configs[j][k];
	  Config conf = conf_i;

	  conf.size++;

	  // merge configs

	  conf.mask.bits  |= conf1.mask.bits;

	  conf.black.bits |= conf1.black.bits;
	  conf.black.bits &= conf.mask.bits;
	  conf.white.bits |= conf1.white.bits;
	  conf.white.bits &= conf.mask.bits;

#if 0	  
	  // choose shorter pointer list
	  
	  if ((*conf.pinflist).size() > (*conf1.pinflist).size()) {
	    conf.pinflist = conf1.pinflist;
	  }
	  
	  //	  cout << "." << flush;
#endif

	  if (!MAXIMUM_TEST || conf.maximal()) {
	    
#if 0
	    if (conf.size >= 2) {
	      int k, num=0, num1, num2;
	      ULL mask = 1LL;

	      FOR (k, 64) {
		if (conf.mask.bits & mask) {
		  num1 = num2;
		  num2 = k;
		  num++;
		  if (num >= 2) break;
		}
		mask <<= 1;
	      }

	      // printf("%d %d\n", num1, num2);
	      
	      // 0 1 2 3 9 10 11 18 19 27
	      
	      if (num1 != 0 || num2 != 1) break;

	    }

#endif

	    // prove! maximal(n+1) subset Union(e in {0..63} maximal(n)+{e})

	    //	    cout << ">>> maximal" << endl;
	    //cout << conf;

#if 0
	    // BUG: subsets must be maximal, have to check this ...
	    // all n-subsets must be here!

	    if (!CONNECTED) {

	      int subset_n = 0;

	      for (vector<Config>::iterator i = &configs[prev_start]; i != configs.end(); i++) {
		
		if ((*i).size + 1 != len) {
		  cerr << endl << (*i).size + 1 << " " << len << endl;
		  Error("wrong size");
		}
		if ((*i).mask.bits  & ~conf.mask.bits)  continue;
		if ((*i).black.bits & ~conf.black.bits) continue;
		if ((*i).white.bits & ~conf.white.bits) continue;
		subset_n++;
		if (subset_n >= len) break;
	      }	    
	      
	      if (subset_n < len) continue;
	    }

#endif

	    //	    STAT(conf, infos, MIN_NUM, Config::glob_n);

	    if (conf.size <= 3) {

	      fast_stat(conf, infos, MIN_NUM, Config::glob_n);

	    } else {

	      new_fast_stat(conf, infos, MIN_NUM, Config::glob_n, conf_i, conf1.bit_index, k, first_call);
	      first_call = false;

	    }

#if 0
	    static new_num = 0, num = 0;
	    if (conf.n >= MIN_NUM) new_num++;
	    num++;
	    if ((num & 255) == 0) 
	      printf(">> %d %d %f %f\n", num, new_num, double(new_num)/num, 
		     double(num)/count); 
#endif

	    if (conf.n >= MIN_NUM) { 
	      conf.pred = ci;
	      configs.push_back(conf);
	      //	      cout << conf << endl;
	    }

	  }

	  //	  cout << ((conf.n >= MIN_NUM) ? "!" : ".") << flush;
	}
      }
    }

    if (configs.size() == end_index) break;   // nothing new => halt

    prev_start = end_index;
  }

#if 1

  // print config list

  double avg_dd = double(Config::glob_res_sum)/Config::glob_n;
  cout << endl << " avg_dd= " << avg_dd << endl;

  int test_num = (infos.size()/(MIN_NUM/MIN_STAT_NUM)+32) & ~31;

  cout << "test_num = " << test_num << endl;

  for (vector<Config>::iterator ci=configs.begin(); ci != configs.end(); ci++) {

    Config &co = *ci, conft[8];
    int t;
    sint4 res_sum = 0, res_square = 0;

    co.transform(conft);
    co.n = 0;

    FOR (t, 8) {

      Config &conf = conft[t];

#if 0

      uint4 j;
      FOR (j, test_num) {
	if (conf.match(infos[j])) {
	  co.n++;
	  res_sum += infos[j].result;
	  res_square += square(infos[j].result);
	}
      }

#else

      // ~5 times faster

      int sq_num=0;
      ULL mask = 1LL;
      uint4 *pati[64];

      FOR (i, 64) {
	if (conf.mask.bits & mask) {
	  if (conf.black.bits & mask) 
	    pati[sq_num++] = &bit_info.bits[BLACK_IND*64+i][0];
	  else if (conf.white.bits & mask) 
	    pati[sq_num++] = &bit_info.bits[WHITE_IND*64+i][0];
	  else 
	    pati[sq_num++] = &bit_info.bits[EMPTY_IND*64+i][0];
	}
	mask <<= 1;
      }

      if (conf.size != sq_num) Error("sq_num");

      statn(co, test_num/32, pati, infos, res_sum, res_square);
      
#endif

    }

    double mean   = double(res_sum)/(co.n+0.01);
    double mean2  = mean - avg_dd;
    double stddev = sqrt(double(res_square)/(co.n+0.01) - square(mean));

    cout << co;
    cout << flush;

    if (mean2 < 0) printf(" - "); else printf(" + ");

    printf(" %.2f %.2f %.2f\n", abs(mean2), stddev, abs(mean2)/(stddev+0.01));

  }   

#endif


  return 0;
}
