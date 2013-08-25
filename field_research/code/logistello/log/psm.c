// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// search patterns / 1.97 / 2.98

#include "main.h"
#include <vector.h>
#include <algo.h>
#include "sboard.h"
#include "crt.h"
#include "trans.h"
#include "psfast.h"
#include "psbinfo.h"
#include "psconf.h"

const int  INFO_MAXNUM = 200000;
const int  MIN_NUM     = 1300;
const int  CONFIG_MAX  = 2000000;

const bool LAZY      = true;  // use probabilistic algo. for check
const bool CONNECTED = false; // only generate connected patterns, buggy
const bool START1    = false;

const int  BLACK_IND = 2;
const int  EMPTY_IND = 1;
const int  WHITE_IND = 0;

BitInfo bit_info;

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


long long step_n, step_sum, ok_n, stat_n;


void _abort(void) { exit(20); }


static void slow_stat(Config &conf, vector<BoardInfo> &infos, int, int)
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


// with bit_info

static void fast_stat(Config &conf0, vector<BoardInfo> &infos, int min_num, int total_num)
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

static void new_fast_stat(Config &conf0, vector<BoardInfo> &infos, int min_num, int total_num,
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
      max_len = 32768;

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


  // init. configlist with one-square configurations

  int start_len=0;
  ULL mask = 1LL;
  Config conf_ext;
  conf_ext.from_board(ext_bo);

  int i = 0, bit_index = 0;

  while (mask) {

    if (START1 || (mask & conf_ext.mask.bits) != 0) {

      Config conf;
      conf.size = 1;
      conf.bit_index = bit_index;
      conf.mask.bits = mask;

      extend_configs[i][EMPTY_IND] = conf;
      if (START1 && octant[i]) configs.push_back(conf);

      conf.black.bits = mask;
      extend_configs[i][BLACK_IND] = conf;
      if (START1 && octant[i]) configs.push_back(conf);        

      conf.black.bits = 0LL;
      conf.white.bits = mask;
      extend_configs[i][WHITE_IND] = conf;
      if (START1 && octant[i]) configs.push_back(conf);
      i++;
    }

    mask <<= 1; bit_index++;
  }

  ext_num = i;

  printf("ext_num=%d\n", ext_num);

  if (!START1) {

    FOREVER {

      Config conf;

      conf.from_board(start_bo);
      start_len = conf.size;

      if (conf.maximal()) {
	fast_stat(conf, infos, MIN_NUM, Config::glob_n);

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

  } else {

    // determine values for start patterns

    start_len = 1;
    for (vector<Config>::iterator ci=configs.begin(); ci != configs.end(); ci++) {
      fast_stat(*ci, infos, -1, -1);
    }
  }

  int prev_start = 0;

  for (int len=start_len+1;; len++) {

    // if (len == 10) break;

    step_n = step_sum = ok_n = stat_n = 0;

    cout << configs.size() << " configurations with size <= " << len-1 << endl;

    // create new configs

    vector<Config> new_configs;

    int counter = 0;

    for (vector<Config>::iterator ci = configs.begin(); ci != configs.end(); ci++) {

      if ((*ci).size != len-1) continue;

      bool first_call = true;  // for new_fast_stat

      // cout << "first" << endl;

      static int count = 0;
      count++;

      if (!(count & 31)) {
	cout << count << " "
	     << int(double(ci-&configs[prev_start])*1000.0/(configs.end()-&configs[prev_start]))/10.0
	     << " "
	     << double(new_configs.size())/(ci-&configs[prev_start]+0.001)
	     << endl;
      }

      for (int j=ext_num-1; j >= 0; j-- ) {

        Config &conf1 = extend_configs[j][0];
	if ((*ci).mask.bits & conf1.mask.bits) break;  // intersection => end

	if (CONNECTED) {

	  ULL m;
	  bool connected = true;

	  FOREVER { 
	    m = conf1.mask.bits << 1;
	    if (!(m & 0x0101010101010101LL) && (m & (*ci).mask.bits)) break;
	    m = conf1.mask.bits << 9;
	    if (!(m & 0x0101010101010101LL) && (m & (*ci).mask.bits)) break;
	    m = conf1.mask.bits >> 7;
	    if (!(m & 0x0101010101010101LL) && (m & (*ci).mask.bits)) break;
	    
	    m = conf1.mask.bits >> 1;
	    if (!(m & 0x8080808080808080LL) && (m & (*ci).mask.bits)) break;
	    m = conf1.mask.bits >> 9;
	    if (!(m & 0x8080808080808080LL) && (m & (*ci).mask.bits)) break;
	    m = conf1.mask.bits << 7;
	    if (!(m & 0x8080808080808080LL) && (m & (*ci).mask.bits)) break;
	    
	    m = conf1.mask.bits >> 8;
	    if ((m & (*ci).mask.bits)) break;
	    m = conf1.mask.bits << 8;
	    if ((m & (*ci).mask.bits)) break;
	    
	    connected = false;
	    break;
	  }
	  
	  if (!connected) continue;
	}

	for (int k=0; k < 3; k++) {

	  Config &conf1 = extend_configs[j][k];
	  Config conf = *ci;

	  conf.size++;

	  // merge configs

	  conf.mask.bits  |= conf1.mask.bits;

	  conf.black.bits |= conf1.black.bits;
	  conf.black.bits &= conf.mask.bits;
	  conf.white.bits |= conf1.white.bits;
	  conf.white.bits &= conf.mask.bits;

	  if (conf.maximal()) {
	    
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


	    new_fast_stat(conf, infos, MIN_NUM, Config::glob_n, *ci, conf1.bit_index, k, first_call);
	    first_call = false;

#if 0
	    static new_num = 0, num = 0;
	    if (conf.n >= MIN_NUM) new_num++;
	    num++;
	    if ((num & 255) == 0) 
	      printf(">> %d %d %f %f\n", num, new_num, double(new_num)/num, 
		     double(num)/count); 
#endif
	    if (conf.n >= MIN_NUM) { 
	      conf.pred = ci-configs.begin();
	      new_configs.push_back(conf);
	    }

	  }

	  //	  cout << ((conf.n >= MIN_NUM) ? "!" : ".") << flush;
	}
      }
    }

    if (new_configs.empty()) break;   // nothing new => halt

    prev_start = configs.size();

    while (!new_configs.empty()) {
      configs.push_back(new_configs.back());
      new_configs.pop_back();
    }

  }

#if 1

  // print config list

  double avg_dd = double(Config::glob_res_sum)/Config::glob_n;
  cout << endl << " avg_dd= " << avg_dd << endl;

  int test_num = (infos.size()/10+32) & ~31;

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
