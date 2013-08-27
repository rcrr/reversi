// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// genetic algo. for othello edge interaction / 5.00

#include "main.h" 
#include "board.h"
#include "crt.h"
#include "patt.h"
#include "fpatt.h"
#include "eval.h"
#include "trans.h"
#include "sboard.h"
#include "nnpatt.h"

#include <set>
#include <vector>
#include <algorithm>

const sint4 SYMN  = 10;
const sint4 ASYMN = 20;
const uint4 MIN_OCCUR = 4;

void _abort(void) { exit(20); }


static PatternInfo pattern_infos[] = {

 {{
    0,0,0,0,0,0,0,0,0,0,
    0,1,2,3,4,5,6,7,8,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0
  }, "EDGE", 0
  // , { 1,2,3,4,5,6,7,8,0 }
  }

};

const int PATT_NUM = sizeof(pattern_infos)/sizeof(pattern_infos[0]);


class BoardInfo {

public:

  sint2 val;
  uint2 index[4];

  void from_sboard(const SPFELD &sb) {

    const signed char *p = sb.p;
    
    index[0] = PATT8(+,A1,B1,C1,D1,E1,F1,G1,H1);
    index[1] = PATT8(+,H1,H2,H3,H4,H5,H6,H7,H8);
    index[2] = PATT8(+,H8,G8,F8,E8,D8,C8,B8,A8);
    index[3] = PATT8(+,A8,A7,A6,A5,A4,A3,A2,A1);

    val = sb.Marke - (MA_DIFF+64);
    if (val < -64 || val > 64) Error("no diff-label!");
  }
  
};


typedef vector<BoardInfo> BoardInfos;

typedef vector<uint4> SymTable;


class TabEntry {

public:
  
  uint4 n;
  real4 sum, sum_squares;

  void clear() { n = 0; sum = 0; sum_squares = 0; }

  TabEntry() { clear(); }
};


class CellTable {

public:
  
  int symn;  // #symmetric values
  int asymn; // #asymmetric values
  int cn;    // codes
  
  vector<int> map;
  vector<TabEntry> table;

  CellTable(int sn, int an);

  int mirror(int i) {
    if (i < symn) return i;
    if (i < symn + asymn) return i + asymn;
    return i - asymn;
  }
  
  int index1(int i1, int i2, int i3, int i4) {
    return ((i1*cn+i2)*cn+i3)*cn+i4;
  }

  int index2(int i2, int i3, int i4, int i1) {
    return index1(i1,i2,i3,i4);
  }

  int index3(int i3, int i4, int i1, int i2) {
    return index1(i1,i2,i3,i4);
  }

  int index4(int i4, int i1, int i2, int i3) {
    return index1(i1,i2,i3,i4);
  }

  int m_index1(int i1, int i2, int i3, int i4) {
    return index1(mirror(i4), mirror(i3), mirror(i2), mirror(i1));
  }
  
  int m_index2(int i2, int i3, int i4, int i1) {
    return m_index1(i1, i2, i3, i4);
  }
  
  int m_index3(int i3, int i4, int i1, int i2) {
    return m_index1(i1, i2, i3, i4);
  }
  
  int m_index4(int i4, int i1, int i2, int i3) {
    return m_index1(i1, i2, i3, i4);
  }
  
  
  TabEntry &entry(const BoardInfo &bi, vector<int> &code) {
    return table[map[index1(code[bi.index[0]],
			    code[bi.index[1]],
			    code[bi.index[2]],
			    code[bi.index[3]]
			    )]];
  }
};


CellTable::CellTable(int sn, int an) : symn(sn), asymn(an)
{
  TabEntry e;

  cn = sn + 2*an;
  if (cn > 64) ERR("cn > 64");
  
  int n = cn*cn*cn*cn;

  map.reserve(n);

  cout << n << " combinations" << endl;
  
  FORS (i, n) map.push_back(-1);

  FORS (i1, cn) {
    FORS (i2, cn) {
      FORS (i3, cn) {
	FORS (i4, cn) {

#if 0
	  cout << i1 << " " << i2 << " " << i3 << " " << i4 << endl;
	  if (index1(i1,i2,i3,i4) < 0 || index1(i1,i2,i3,i4) >= n) ERR("1");
	  if (index2(i1,i2,i3,i4) < 0 || index2(i1,i2,i3,i4) >= n) ERR("2");
	  if (index3(i1,i2,i3,i4) < 0 || index3(i1,i2,i3,i4) >= n) ERR("3");
	  if (index4(i1,i2,i3,i4) < 0 || index4(i1,i2,i3,i4) >= n) ERR("4");
	  if (m_index1(i1,i2,i3,i4) < 0 || m_index1(i1,i2,i3,i4) >= n) ERR("5");
	  if (m_index2(i1,i2,i3,i4) < 0 || m_index2(i1,i2,i3,i4) >= n) ERR("6");
	  if (m_index3(i1,i2,i3,i4) < 0 || m_index3(i1,i2,i3,i4) >= n) ERR("7");
	  if (m_index4(i1,i2,i3,i4) < 0 || m_index4(i1,i2,i3,i4) >= n) ERR("8");
#endif
	  
	  if (map[index1(i1,i2,i3,i4)] >= 0) continue;

	  uint4 i = table.size();
	  map[index1(i1,i2,i3,i4)] = i;
	  map[index2(i1,i2,i3,i4)] = i;
	  map[index3(i1,i2,i3,i4)] = i;
	  map[index4(i1,i2,i3,i4)] = i;	  
	  map[m_index1(i1,i2,i3,i4)] = i;
	  map[m_index2(i1,i2,i3,i4)] = i;
	  map[m_index3(i1,i2,i3,i4)] = i;
	  map[m_index4(i1,i2,i3,i4)] = i;	  

	  table.push_back(e);
	}
      }
    }
  }
    
  cout << table.size() << " cells" << endl;
}



class Genome {

public:

  CellTable *c_tab;
  SymTable *s_tab;

  bool  fitness_defined;
  real4 fitness;
  vector<int> code;

  void mutate(int n);
  
  void merge(const Genome &genome);

  real4 fit(const BoardInfos &bi);

  Genome() { c_tab = 0; s_tab = 0; fitness_defined = false; }
  
  Genome(CellTable &t, SymTable &s, bool rand) : c_tab(&t), s_tab(&s) {

    fitness_defined = false;

    uint4 l = s_tab->size();
    code.reserve(l);
    FORU (i, l) code.push_back(-1);

    if (rand) {
      FORU (i, l) {
	if (code[i] < 0) {
	  if ((*s_tab)[i] == i) {
	    code[i] = random() % c_tab->symn;
	  } else {
	    int r = (random() % c_tab->asymn) + c_tab->symn;
	    code[i] = r;
	    code[(*s_tab)[i]] = r + c_tab->asymn;
	  }
	}
      }
    }
  }

  friend bool operator < (const Genome &a, const Genome &b);
};

bool operator < (const Genome &a, const Genome &b) {

  if (!a.fitness_defined || !b.fitness_defined) ERR("undefined fitness");

  return a.fitness < b.fitness;
}
  
void Genome::mutate(int n)
{
  int length = code.size();
  int code_n = c_tab->symn + 2*c_tab->asymn;

  ERR("mutate buggy");

  // symmetry?
  
  FORS (i, n) {
    int r = random() % length;
    code[r] = random() % code_n;
  }
}
 

void Genome::merge(const Genome &genome)
{
  uint4 length = code.size();

  if (length != genome.code.size()) ERR("different lengths");
  if (c_tab->symn != genome.c_tab->symn ||
      c_tab->asymn != genome.c_tab->asymn)
    ERR("different (a)symn");
  
  FORU (i, length) {
    if (random() & 1) {
      code[i] = genome.code[i];
      uint4 s = (*s_tab)[i];
      if (i != s) {
	code[s] = genome.code[s];
      }
    }
  }
}


real4 Genome::fit(const BoardInfos &inf)
{
  // clear cells
  
  FORU (i, c_tab->table.size()) c_tab->table[i].clear();
  
  // compute cell statistics

  // cout << inf.size() << " vectors" << endl;
  
  FORU (i, inf.size()) {

    TabEntry &e = c_tab->entry(inf[i], code);

    // cout << "i= " << &e-&c_tab->table[0] << " " << inf[i].val << endl;

    ++e.n;
    e.sum += inf[i].val;
    e.sum_squares += inf[i].val * inf[i].val;

  }

  // average values of low count cells

  real4 low_avg = 0;
  uint4 low_avg_n = 0;

  uint4 good_n = 0;
  const uint4 HIST_N = 10;
  uint4 histo[HIST_N];

  FORU (i, HIST_N) histo[i] = 0;

  FORU (i, c_tab->table.size()) {

    // cout << c_tab->table[i].n << endl;

    uint4 n = c_tab->table[i].n;
    
    if (n < MIN_OCCUR) {
      low_avg += c_tab->table[i].sum;
      ++low_avg_n;
    } else {    
      ++good_n;
    }

    if (n >= HIST_N) n = HIST_N-1;
    ++histo[n];
  }

  cout << "good= " << good_n << endl;

  uint4 sum_inf = 0, sum_cells = 0;
  
  FORU (i, HIST_N) {
    sum_inf += i * histo[i];
    sum_cells += histo[i];
    myform(cout, "%2d %7d %5.1f %5.1f\n", i, histo[i],
	   100.0*histo[i]/c_tab->table.size(),
	   100.0*sum_inf/inf.size());
  }
  
  low_avg /= (low_avg_n+0.001);
    
  // sum squared errors

  real4 sq_err = 0;
  
  FORU (i, c_tab->table.size()) {

    if (c_tab->table[i].n > 0) {
      
      real4 mean;
    
      if (c_tab->table[i].n < MIN_OCCUR) {
	mean = low_avg;
      } else {
	mean = c_tab->table[i].sum/c_tab->table[i].n;
      }

      sq_err +=
	c_tab->table[i].sum_squares -
	2.0*mean*c_tab->table[i].sum +
	c_tab->table[i].n*mean*mean;
    }
  }

  fitness = -sqrt(sq_err/inf.size());
  fitness_defined = true;
  
  cout << "error= " << -fitness << endl;

  return fitness;
}



class Pool {

public:

  vector<Genome> creatures;

  Pool(CellTable &t, SymTable &s, int n);

  void next_generation(BoardInfos &infos);
};


Pool::Pool(CellTable &t, SymTable &s, int n)
{
  FORS (i, n) {
    Genome g(t, s, true);
    creatures.push_back(g);
  }

}

void Pool::next_generation(BoardInfos &infos)
{
  uint4 n = creatures.size();
  uint4 n_new  = (n*10)/100;
  uint4 n_mate = (n*20)/100;
  uint4 n_best = (n*50)/100;
  
  // compute fitness of all new creatures
  
  FORU (i, n) {
    if (!creatures[i].fitness_defined) {
      cout << "fitness " << i << endl;
      creatures[i].fit(infos);
    }
  }

  // sort

  sort(creatures.begin(), creatures.end());

  // replace last n_new by new creatures

  FORU (i, n_new) {
    Genome g(*creatures[0].c_tab, *creatures[0].s_tab, true);
    creatures[n-1-i] = g;
  }

  // replace preceeding n_mate creatures by offstring

  FORU (i, n_mate) {
    int r1 = random() % n_best;
    int r2;

    do { r2 = random() % n_best; } while (r1 == r2);
    
    Genome g = creatures[r1];
    g.merge(creatures[r2]);
    
    creatures[n-1-n_new-i] = g;
  }

  // compute fitness of new creatures

  FORU (i, n) {
    if (!creatures[i].fitness_defined) {
      creatures[i].fit(infos);
    }
  }
  
}




int main(int argc, char **argv)
{
  if (argc != 2) { ERR("usage: ointer sfk-file"); }
  
  FILE *fp = fopen(argv[1], "r");
  if (fp == 0) { ERR("file not found"); }

  Pattern patt;
  SymTable s_tab;
  CellTable c_tab(SYMN, ASYMN);
  BoardInfos infos;
  
  patt.init(pattern_infos[0]);

  if (patt.perm_num != 2) ERR("permnum != 2");
  
  // generate stable

  int n = Pot3[patt.len];

  s_tab.reserve(n);
  FORS (i, n) s_tab.push_back(0);

  FORS (i, n) {

    if (patt.tab[i].p_map != &patt.tab[i]) {

      int j = patt.tab[i].p_map - &patt.tab[0];
      s_tab[i] = j;
      s_tab[j] = i;
      
    } else {

      s_tab[i] = i;
      
    }
  }

#if 0
  FORS (i, n) {
    Pattern::conf_write(stdout, patt.len, i); fflush(stdout); 
    cout << " " << i << " " << s_tab[i] << " " << ((i==s_tab[i]) ? "*" : "") << endl;
  }
#endif


  // read boards

  
  FOREVER {
    SPFELD sb;
    
    if (!fSfRead(fp, &sb, 1)) break;

    BoardInfo bi;

    bi.from_sboard(sb);
    infos.push_back(bi);
  }

  cout << infos.size() << " boards" << endl;

  Pool pool(c_tab, s_tab, 100);

  cout << "bla" << endl;

  pool.next_generation(infos);
  
  return 0;
}
 
