// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// generate all patterns of a specific length / 12.99

#include "main.h" 
#include "board.h"
#include "crt.h"
#include "patt.h"
#include "fpatt.h"
#include "eval.h"
#include "trans.h"
#include "pattgenm.h"
#include <set.h>

#define TEST 0

#define MAX_LEN    10  // # of squares in patterns
#define LAST_PLIES 2   // 1 or 2

#define Y_REAL_FACTOR_BITS  6 
#define Y_REAL_FACTOR       (1<<Y_REAL_FACTOR_BITS)

static bool octant[64] = {
  1,1,1,1,0,0,0,0,
  0,1,1,1,0,0,0,0,
  0,0,1,1,0,0,0,0,
  0,0,0,1,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0
};

typedef sint1    VALTYPE; // do not change! (signed access to array +128 somewhere)
typedef VALTYPE *PVALTYPE;

typedef sint4    INDTYPE;
typedef INDTYPE *PINDTYPE;

typedef sint1    RESTYPE;

class IncData {

public:

  sint4 i1, i2, i3, i4;

  IncData() { i1 = i2 = i3 = i4 = 0; }
};

typedef IncData *PINCDATA;

sint4 n;       // #of examples, divisible by 4
sint4 n_bytes; // n/4

PVALTYPE values[64];
PINDTYPE indices[8];   // partial indices, all transformations
Entry **pentries[8];    // pointers to entries
RESTYPE  *results;

PINCDATA incdat[MAX_LEN];

real4 avg_result;
real4 min_avg_error = 1e10;

sint4 patt_num = 0;

static void process(PatternInfo &pinf)
{
  static Pattern prev_patt;
  Pattern patt;
  sint4 i_diff;

  patt_num++;

  //  if ((patt_num % 100) == 0) { cerr.form("%7d\r", patt_num); cerr << flush; }
  
  patt.init(pinf);
  
  // update indices: compute pattern differences,
  // add new square values

  i_diff = 0;
  
  if (prev_patt.len == patt.len) {
    FOR (i_diff, patt.len-LAST_PLIES) {
      if (patt.sq_lists[0][i_diff] != prev_patt.sq_lists[0][i_diff]) break;
    }
  }

  // if (i_diff < patt.len-LAST_PLIES) cout << "i_diff=" << i_diff << endl;
  
  if (prev_patt.len == patt.len) {
    
    // subtract previous square values

    for (sint4 j=i_diff; j < patt.len-LAST_PLIES; j++) {

#if 0
      {
	char s[200];
	cout << "- " << j << " ";

	FORS (t, 8) {
	  sKoorAus(s, Tab8to10[prev_patt.sq_lists[t][j]]);
	  cout << s << " ";
	}
	cout << endl;
      }
#endif
      
      FORS (t, 8) { // for all transformations
      
	register IncData *idat = incdat[j];
	register INDTYPE *pind = indices[t];
	register VALTYPE *pval = values[prev_patt.sq_lists[t][j]];
	
	for (sint4 k = n_bytes; k > 0; k--) {
	  
	  register IncData &d = idat[*pval++];
	  
	  *pind++ -= d.i1;
	  *pind++ -= d.i2;
	  *pind++ -= d.i3;
	  *pind++ -= d.i4;
	}
      }
    }
  }

  //  cout << patt.len-LAST_PLIES - i_diff << flush;
  
  // add new square values

  for (sint4 j=i_diff; j < patt.len-LAST_PLIES; j++) {

#if 0
    {
      char s[200];
      cout << "+ " << j << " ";

      FORS (t, 8) {
	sKoorAus(s, Tab8to10[patt.sq_lists[t][j]]);
	cout << s << " ";
      }
      cout << endl;
    }
#endif
    
    FORS (t, 8) { // for all transformations
      
      register IncData *idat = incdat[j];
      register INDTYPE *pind = indices[t];
      register VALTYPE *pval = values[patt.sq_lists[t][j]];
	
      for (sint4 k = n_bytes; k > 0; k--) {
	  
	register IncData &d = idat[*pval++];
	  
	*pind++ += d.i1;
	*pind++ += d.i2;
	*pind++ += d.i3;
	*pind++ += d.i4;
      }
    }
  }

  // last square(s): update config statistics using index+delta
  
  FORS (t, 8) { // for all transformations
    
    register INDTYPE *pind = indices[t];
    register RESTYPE *pres = results;
    register Entry   *ptab = patt.tab;

#if LAST_PLIES == 1

#error "foo"
    
    register IncData *idat = incdat[patt.len-1];
    register VALTYPE *pval = values[patt.sq_lists[t][patt.len-1]];

    for (sint4 k = n_bytes; k > 0; k--) {

      // puts("");
      register IncData &d = idat[*pval++];
      register Entry *pe;
      
      pe = ptab[*pind++ + d.i1].p_map;
      pe->n++;		      
      pe->y += *pres++;	      
			      
      pe = ptab[*pind++ + d.i2].p_map;
      pe->n++;		      
      pe->y += *pres++;	      
			      
      pe = ptab[*pind++ + d.i3].p_map;
      pe->n++;		      
      pe->y += *pres++;	      
			      
      pe = ptab[*pind++ + d.i4].p_map;
      pe->n++;
      pe->y += *pres++;
    }

#else
    
    IncData *idat1 = incdat[patt.len-1];
    VALTYPE *pval1 = values[patt.sq_lists[t][patt.len-1]];
    
    IncData *idat2 = incdat[patt.len-2];
    VALTYPE *pval2 = values[patt.sq_lists[t][patt.len-2]];

    register Entry **ci = pentries[t];

#if 0
    cout << patt.sq_lists[t][patt.len-1] << " "
	 << patt.sq_lists[t][patt.len-2] << endl;
#endif
    
    for (sint4 k = n_bytes; k > 0; k--) {

      // puts("");
      register IncData &d1 = idat1[*pval1++];
      register IncData &d2 = idat2[*pval2++];      
      register Entry *pe;

#if 0
      cout << *pind << " " << d1.i1 << " " << d2.i1;
      cout.form(" %2x %2x", sint4(*(pval1-1)) & 0xc0, sint4(*(pval2-1)) & 0xc0);
      cout << endl;
#endif
      
      *ci++ = pe = ptab[*pind++ + d1.i1 + d2.i1].p_map;
      pe->n++;
      pe->y += *pres++;

      *ci++ = pe = ptab[*pind++ + d1.i2 + d2.i2].p_map;
      pe->n++;
      pe->y += *pres++;

      *ci++ = pe = ptab[*pind++ + d1.i3 + d2.i3].p_map;
      pe->n++;
      pe->y += *pres++;

      *ci++ = pe = ptab[*pind++ + d1.i4 + d2.i4].p_map;
      pe->n++;
      pe->y += *pres++;
    }

#endif
    
  }

  //

  sint4 n_config = Pot3[patt.len];
  
  FORS (i, n_config) {
    Entry &e = patt.tab[i];
    
    if (&e == e.p_map) {

      if (e.n == 0) e.y = sint4(avg_result * Y_REAL_FACTOR);
      else {
	real4 a = e.n * 0.01;
	if (a > 1.0) {
	
	  e.y = sint4(Y_REAL_FACTOR * real4(e.y)/e.n);
	  
	} else {
	  
	  e.y = sint4(Y_REAL_FACTOR * ((a * e.y)/e.n + (1-a) * avg_result));
	}
      }

      // cout << "n=" << e.n << " y=" << real4(e.y)/Y_REAL_FACTOR << endl;
    }
  }

  register Entry **pc0 = pentries[0];
  register Entry **pc1 = pentries[1];
  register Entry **pc2 = pentries[2];
  register Entry **pc3 = pentries[3];
  register Entry **pc4 = pentries[4];
  register Entry **pc5 = pentries[5];
  register Entry **pc6 = pentries[6];
  register Entry **pc7 = pentries[7];
  RESTYPE *pres = results;

  sint4 abs_diff_sum = 0;
  
  for (sint4 k = n; k > 0; k--) {

    // puts("");
    
    sint4 val =
      (*pc0++)->y + (*pc1++)->y +
      (*pc2++)->y + (*pc3++)->y +
      (*pc4++)->y + (*pc5++)->y +
      (*pc6++)->y + (*pc7++)->y;

#if 0
    cout << sint4(*pres) << " " << val << " "
	 << real4(val >> 3)/Y_REAL_FACTOR << endl;
#endif
    
    abs_diff_sum += abs(((*pres++) << Y_REAL_FACTOR_BITS) - (val >> 3));
  }

  real4 avg_error = real4(abs_diff_sum)/Y_REAL_FACTOR/n;

#if 1


  cout.form("%-7d ", patt_num);
  
  {
    char s[200];

    FORS (j, patt.len) {
      sKoorAus(s, Tab8to10[patt.sq_lists[0][j]]);
      cout << s << " ";
    }
  }
  
  cout.form("%.2f", avg_error);
  
  if (avg_error < min_avg_error) {

    min_avg_error = avg_error;

    cout << " *";
  }

  cout << endl;
#endif
  
  prev_patt = patt;

#if 0
  if (prev_patt.trans_num != patt.trans_num) Error("1");
  if (prev_patt.len != patt.len) Error("2");  

  FORS (t, 8) {
    FORS (i, patt.len) {
      if (prev_patt.sq_lists[t][i] != patt.sq_lists[t][i])
	Error("3");
    }
  }
#endif
  
}



void _abort(void) { exit(1); }

static void WriteDisc(FILE *fp, PARTEI Partei)
{
  if      (Partei == LEER)  fprintf(fp, "-");
  else if (Partei == BLACK) fprintf(fp, "X");
  else			    fprintf(fp, "O");
}

Pattern::Pattern() 
{ 
  tab = 0; len = 0; trans_num = 0;
}

Pattern::~Pattern() 
{
  free();
}


void Pattern::init(const PatternInfo &pinf0)
{
  int i, j, num=0;
  SPFELD bo, bos[8];
  bool used[MAX_LEN+1];

  pinf = pinf0;
  
  //  cout << "Pattern " << name << endl;
  
  // compute square lists
  
  FOR (i, MAX_LEN) used[i] = false;
  
  FOR (i, 100) {
    sq[i] = pinf.sq[i];
    if (sq[i] != 0) {
      if (sq[i] <= 0 || sq[i] > MAX_LEN) 
	Error("Pattern::init: value out of range in pattern-info");
      used[sq[i]-1] = true;
      num++;
    }
    bo.p[i] = sq[i];
  }
  
  if (num == 0)       Error("Pattern::init: no squares!");
  if (num > MAX_LEN) Error("Pattern::init: too many squares!");
  
  FOR (i, num) if (!used[i]) Error("Pattern::init: hole in sequence");
  
  FOR (j, 8) {
    FOR (i, num) perms[j][i] = i+1;
  }

  Transform(&bo, bos);
  
  trans_num = 0;
  perm_num  = 1;

  FOR (i, 8) {
    
    for (j=0; j < i; j++)
      if (sgn_equal(bos[i], bos[j])) break;

    if (j == 0) {

      // pattern is symmetrical => determine perm-list

      FOR (j, 100) {
	if (bo.p[j]) {
	  perms[perm_num][bo.p[j]-1] = bos[i].p[j];
	}
      }

      perm_num++;
    }
    
    // compute sq-list
    
    FOR (j, 100) {
      
      if (bos[i].p[j]) { 
	if (!ZUG(j)) Error("Pattern::init: illegal square in pattern!");
	
	//  cout << int(bos[i].p[j]) << " ";
	sq_lists[trans_num][bos[i].p[j]-1] = j;
      }
    }
    
    sq_lists[trans_num][num] = 0;
    trans_num++;
  }
  
  // allocate table

  free();
  
  tab = new Entry[Pot3[num]];
  if (!tab) Error("Pattern::init: no memory");
  
  // compute permutations
  
  FOR (i, Pot3[num]) {
    int list[MAX_LEN+1], j;
    int s = i;
    
    FOR (j, num) {
      list[num-1-j] = s % 3;
      s /= 3;
    }

    int min_s = MAXINT;
    int k;

    FOR (k, perm_num) {
      s = 0;
      FOR (j, num) s = s+s+s+list[perms[k][j]-1];
      if (s < min_s) min_s = s;
    }

    tab[i].p_map = &tab[min_s];

    /*
      if (!strcmp(name, "CHECKERS"))
      FOR (k, perm_num) printf("i=%d, %d->%d\n", i, ps[k], min_s);
    */
    
  }

#if 1

  // printf("%d entries\n", allocated);

  FOR (i, Pot3[num]) {
    if (!tab[i].p_map) Error("0");
  }

#endif


  len = num;



  // adjust pattern: 64 squares and 8 transformations

  FORS (i, trans_num) {
    FORS (j, len) {
      sq_lists[i][j] = Tab10to8[sq_lists[i][j]];
    }
  }

  assert(trans_num == 8);
}


void Pattern::conf_write(FILE *fp, int l, int n)
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


void Pattern::asc_write(FILE *fp)
{
  int k;
  
  FOR (k, Pot3[len]) {
    
    Pattern::conf_write(fp, len, k);
    fprintf(fp, " %+6.3f", tab[k].p_map->y/(tab[k].p_map->n+0.01));
    fprintf(fp, "\n");
  }
}


void Pattern::bin_write(FILE *fp)
{
  int k;
  
  fputc(len, fp);

  FOR (k, Pot3[len]) tab[k].bin_write(fp);
}


bool Pattern::bin_read(FILE *fp)
{
  int k;
  
  if (fgetc(fp) != len) return true;
  
  FOR (k, Pot3[len]) tab[k].bin_read(fp);
  return false;
}


sint4 PatternInfo::max_trans() const
{
  sint4 max=0, j, i, a;
  sint4 *trj, *trmax;
  const sint4 *p=sq;

  trmax = TransTab[max];

  for (j=1; j < 8; j++) {

    trj = TransTab[j];

    FOR_SFPOS10 (i) {
      a = (p[trj[i]] != 0) - (p[trmax[i]] != 0);
      if (a > 0) { max = j; trmax = TransTab[max]; break; }
      if (a < 0) break;
    }
  }

  return TransInv[max];
}


typedef set<sint4> SquareSet;

bool PatternInfo::connected() const
{
  sint4 i, i0=0;
  sint4 num = 0;
  
  FOR (i, 100) {
    if (sq[i]) { i0 = i; num++; }
  }

  if (num == 0) Error("connected: no square");

  SquareSet sq_set, new_set, newnew_set;

  new_set.insert(Square(i0));

  while (new_set.size() > 0) {

    newnew_set.erase(newnew_set.begin(), newnew_set.end());
    
    SquareSet::iterator i;

    for (i=new_set.begin(); i != new_set.end(); i++) {
      sq_set.insert(*i);
    }
    
    for (i=new_set.begin(); i != new_set.end(); i++) {
      int j;
      Square s = *i;
      sq_set.insert(s);
      FOR (j, 8) {
        int l = s+ds[j];
        if (sq[l] && sq_set.find(Square(l)) == sq_set.end())
          newnew_set.insert(l);
      }
    }

    new_set = newnew_set;
  }

  return (sint4)sq_set.size() == num;
}



int main(int argc, char **argv)
{
  char *file_name;
  FILE  *fp;
  struct stat st;

  InitCrt();

  if (argc != 3) {
    Error("call: opattgen #squares sfk-file");
  }

  // preprocessing: read in all examples and compute values (0,1,2)
  // for each square, 4 values stored in one byte
  
  // #examples

  sint4 n_squares = atoi(argv[1]);

  if (n_squares < 1 || n_squares > MAX_LEN) Error("#squares out of range");
  
  file_name = argv[2];

  if (stat(file_name, &st)) Error("can't stat file\n");

  if ((st.st_size % 18) != 0) Error("file length % 18 != 0");

  n = (st.st_size / 18) & ~3;
  n_bytes = (n+3)/4;

  SPFELD sf;
  
  cout << n << " positions" << endl;

  if (n < 4) Error("n < 4");
  
  // allocate arrays
  
  FORS (i, 64) {
    values[i] = new VALTYPE[n_bytes];
    FORS (j, n_bytes) values[i][j] = 0;
  }

  results = new sint1[n];

  FORS (i, 8) {
    indices[i] = new INDTYPE[n];
    FORS (j, n) indices[i][j] = 0;

    pentries[i] = new (Entry*)[n];
    FORS (j, n) pentries[i][j] = 0;
  }


  // compute inc data:

  // incdat[a][b].ic = 3^a * ((b >> (8-2*c)) & 3) 

  sint4 power = 1;
  
  FORS (i, MAX_LEN) {

    incdat[i] = new IncData[256];
    incdat[i] += 128;

    for (sint4 j = -128; j <= 127; j++) {
      incdat[i][j].i1 = power * ((j >> 6) & 3);
      incdat[i][j].i2 = power * ((j >> 4) & 3);
      incdat[i][j].i3 = power * ((j >> 2) & 3);
      incdat[i][j].i4 = power * ((j     ) & 3);      
    }

    power *= 3;
  }

  fp = fopen(file_name, "r");
  if (!fp) Error("can't open file\n");

  sint4 bit_ind  = 0;
  sint4 byte_ind = 0;
  sint4 result_sum = 0;
  
  FORS (i, n) {
    
    sint4 j = fSfRead(fp, &sf, 1);
    if (j != 1) Error("premature end of file");
 
    sint4 label = sf.Marke;
    sint4 res = 0;

    if (label >= MA_DIFF && label <= MA_DIFF+128) {

      res = (label - MA_DIFF - 64);

    } else Error("illegal label");

    results[i] = res;
    result_sum += res;

    // cout << res << " " << result_sum << endl;
    
#if 0
    
    Anz = SfAnz(&sf);

    ind = (Anz - I0) / IWIDTH;

    if (ind < 0)      ind = 0;
    if (ind > INUM-1) ind = INUM-1;
#endif

    //    SfAus(&sf, 0, 0);
    
    FORS (j, 64) {

      sint4 ind = Tab8to10[j];
      sint4 cont = sf.p[ind];
      sint4 v;
      
      if      (cont == BLACK) v = 2;
      else if (cont == LEER)  v = 1;
      else                    v = 0;

      //      cout << v;
      values[j][byte_ind] <<= 2;
      values[j][byte_ind] |= v;
    }

    //    cout << endl;
    bit_ind += 2;

    if (bit_ind >= 8) {
      bit_ind = 0;
      ++byte_ind;
    }
  }

  real8 delta_sum = 0.0;
    
  avg_result = real4(result_sum)/n;
  cout << "avg.result: " << avg_result << endl;

  FORS (i, n) delta_sum += fabs(avg_result - results[i]);

  cout << "avg. error upper bound: " << delta_sum/n << endl;
  cout << "preprocessing done." << endl;
  
  sint4 squares[n_squares];

  FORS (i, n_squares) squares[i] = i;
  
  FOREVER {

    PatternInfo pinf;

    pinf.name = "foo";

    FORS (i, 100) pinf.sq[i] = 0;
    FORS (i, n_squares) pinf.sq[Tab8to10[squares[i]]] = 1+i;

    if (pinf.max_trans() == 0 && pinf.connected()) {
      process(pinf);
    }

    // next vector

    sint4 i0 = n_squares-1;

    while (i0 >= 0) {

      ++squares[i0];
      if (squares[i0] > 64-(n_squares-i0)) { i0--; continue; }

      if (i0 < n_squares-1) {
	
	// check whether prefix can be connected
	// if not increment square
	
	FORS (j, 100)  pinf.sq[j] = 0;
	FORS (j, i0+1) pinf.sq[Tab8to10[squares[j]]] = 1+j;
	FORS (j, 7)
	  if (j+1+squares[i0] < 64) pinf.sq[Tab8to10[j+1+squares[i0]]] = 1;
	  else break;
	
	if (!pinf.connected()) continue;
      }
      
      break;
    }

    if (i0 == 0) {
      while (squares[0] < 64 && !octant[squares[0]]) squares[0]++;
      if (squares[0] >= 64) i0 = -1;
    }

    if (i0 < 0) break;

    for (sint4 i=i0+1; i < n_squares; i++) {
      squares[i] = squares[i-1] + 1;
      if (squares[i] >= 64) Error("square list corrupt");
    }
  }

  cout << patt_num << " patterns" << endl;
  
  return 0;
}

