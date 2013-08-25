// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// learn pattern instances using HCGA / 1.98

#include "main.h"
#include "crt.h"
#include "trans.h"
#include "sboard.h"
#include "evolopt.h"
#include "Rnd.H"
#include <algo.h>
#include <vector.h>



void _abort() {}

/////////////////////////////////////////////

class Point {

public:

  uint1 pos;
  sint1 cont;


  void rnd_init()
  {
    do {
      pos = Rnd::Number(0, 100);
    } while (!TabZug[pos]);

    real4 r = Rnd::Uni01();

    if (r < Options::p_empty)
      cont = 0;  // empty
    else if (r < 0.5 * (1+Options::p_empty))
      cont = 1;  // black
    else
      cont = -1; // white
  }

  inline bool match(SPFELD &bo) {
    assert(pos < 100);
    assert(cont >= -1 && cont <= 1);
    return bo.p[pos] == cont;
  }

};


class Points : public vector<Point> {

public:

  void rnd_init()
  {
    uint4 i;

    erase(begin(), end());
    reserve(Options::point_num);

    FOR (i, Options::point_num) {
      Point point;
      point.rnd_init();
      push_back(point);
    }

  }

  inline bool match(SPFELD &bo) {
    uint4 i;

    FOR (i, size()) {
      if (!(*this)[i].match(bo)) return false;
    }

    return true;
  }

};


/////////////////////////////////////////////


class Gene {

public:

  Points pts;
  real4  value;

  int    curr_match_n;
  int    sum_match_n;

  void rnd_init()
  {
    pts.rnd_init();
    init();
  }

  void init()
  {
    value = 0;
  }

  void training_init()
  {
    sum_match_n = 0;
  }

  inline bool match(SPFELD &bo) {
    return pts.match(bo);
  }
};


class Genes : public vector<Gene> {

public:

  void rnd_init()
  {
    uint4 i;

    assert(Options::gene_num > 0);
    erase(begin(), end());
    reserve(Options::gene_num);

    FOR (i, Options::gene_num) {
      Gene gene;
      gene.rnd_init();
      push_back(gene);
    }
  }

  void training_init()
  {
    uint4 i;

    FOR (i, size()) (*this)[i].training_init();
  }

};

/////////////////////////////////////////////

class ScoreHist {

public:

  const int MAX_LEN = 20;
  real4 sum, hist[MAX_LEN];
  int len, pos;

  ScoreHist() 
  {
    init();
  }

  void init()
  {
    len = pos = 0;
    sum = 0.0;
  }

  void add(real4 sc)
  {
    if (len >= MAX_LEN) sum -= hist[pos]; else len++;
    hist[pos++] = sc;
    if (pos >= MAX_LEN) pos = 0;
    sum += sc;
  }

  real4 score()
  {
    assert(len > 0);
    return real4(sum)/len;
  }

};


/////////////////////////////////////////////


class Organism {

  static uint4 curr_id;

public:

  Genes genes;
  int   matched_genes_n;
  ScoreHist sh;
  real4 score, curr_score;
  uint4 id;


  Organism() 
  {
    init();
  }

  void init()
  {
    sh.init();
    stamp();
  }

  void rnd_init()
  {
    init();
    genes.rnd_init();
  }

  void training_init()
  {
    uint4 i;

    FOR (i, genes.size()) genes[i].training_init();

    matched_genes_n = 0;
    curr_score = 0;
  }

  void combine(Organism &mate, Organism &dest)
  {
    uint4 i;

    dest.genes.erase(dest.genes.begin(), dest.genes.end());

    assert((Options::gene_num & 1) == 0);
   
    uint4 size1 = genes.size();
    uint4 size2 = mate.genes.size();

    bool mate1[size1], mate2[size2];

    FOR (i, size1) mate1[i] = false;
    FOR (i, size2) mate2[i] = false;

    if (size1 < Options::gene_num/2 || size2 < Options::gene_num/2)
      Error("combine: number of genes too small");
    
    FOR (i, Options::gene_num/2) {
      
      int r;

      do {
	r = Rnd::Number(0, size1-1);
      } while (mate1[r]);

      mate1[r] = true;
      dest.genes.push_back(genes[r]);

      do {
	r = Rnd::Number(0, size2-1);
      } while (mate2[r]);

      mate2[r] = true;
      dest.genes.push_back(mate.genes[r]);
    }

    dest.init();
  }


  void stamp()
  {
    id = curr_id++;
  }

};

inline bool operator < (const Organism &org1, const Organism &org2)
{
  return org1.score < org2.score;
}


uint4 Organism::curr_id = 0;


class Organisms : public vector<Organism> {

public:

};



/////////////////////////////////////////////


class Examples : public vector<SPFELD> {

public:

};


/////////////////////////////////////////////

#define COPY_CREATE(x,n) x n##1, n##2; n##1 = n##2; // help for g++

void dumb_gcc() {
  COPY_CREATE(vector<Point>, a);
  COPY_CREATE(vector<Gene>, b);
  COPY_CREATE(vector<Organism>, c);
}

/////////////////////////////////////////////

int main()
{
  uint4 i, j, k, l;
  Examples  examples;
  Organisms organisms;

  // 1. read examples

  examples.reserve(650000);

  cout << "reading examples ... " << flush;

  FILE *fp = fopen(Options::example_file, "r");
  if (!fp) Error("can't open sfk-file");
    
  for (i=0;; i++) {
    SPFELD bo;

    if (!fSfRead(fp, &bo, 1)) break;
      
    int val = bo.Marke;
    if (val < MA_DIFF || val > MA_DIFF+128) Error("no diff-label");
    val -= MA_DIFF+64;
    bo.Marke = val;
    examples.push_back(bo);
  } 

  cout << "OK (" << examples.size() << " boards)" << endl;

  // 2. create random organisms

  organisms.reserve(Options::org_num);

  FOR (i, Options::org_num) {
    Organism org;

    org.rnd_init();
    organisms.push_back(org);
  }


  // 3. evolve them

  for (int iter=1;; iter++) {

    // training & fitness

    cout << "iteration: " << iter << endl;

    cout << "training & fitness ... " << endl;

    FOR (i, organisms.size()) organisms[i].training_init();

    FOR (i, Options::training_cycles) {

      int r = Rnd::Number(0, examples.size()-1);
      SPFELD boards[8];
      SPFELD &ex = examples[r];

      Transform(&ex, boards);

      FOR (j, organisms.size()) {

	Organism &org = organisms[j];
	real4 eval = 0;
	int matched_genes_n = 0;

	FOR (k, org.genes.size()) {

	  Gene &gene = org.genes[k];
	  int match = 0;

	  FOR (l, 8) {
	    if (gene.match(boards[l])) {
	      match++;
	      eval += gene.value;
	    }
	  }

	  if (match > 0) matched_genes_n++;
	  gene.curr_match_n = match;
	  gene.sum_match_n += match;

	}

	real4 delta = ex.Marke - eval;
	org.curr_score += delta * delta;
	org.matched_genes_n += matched_genes_n;

	if (matched_genes_n > 0) {

	  delta = (Options::learning_rate*delta)/matched_genes_n;

	  FOR (k, org.genes.size()) {

	    Gene &gene = org.genes[k];

	    if (gene.curr_match_n > 0)
	      gene.value += delta * gene.curr_match_n;
	  }
	}
      }
    }

    // offspring

    // sort organisms regarding their score

    FOR (i, organisms.size()) {
      Organism &org = organisms[i];
      org.sh.add(sqrt(org.curr_score/Options::training_cycles));
      org.score = org.sh.score(); // running average
    }

    sort(organisms.begin(), organisms.end());

    for (i=min(9,int(organisms.size()-1)); i < ~uint4(0); i--) {
      cout << setw(4) << i 
	   << " " << setw(7) << organisms[i].id
	   << " " << organisms[i].score 
	   << " " << float(organisms[i].matched_genes_n/organisms[i].genes.size())/
	              Options::training_cycles
	   << endl;
    }

    
    cout << "generate new organisms" << endl;


    // generate new organisms

    int best_n  = int(organisms.size() * Options::best_size);
    int worst_i = int(organisms.size() * (1.0-Options::worst_size));

    // new guys

    FOR (i, uint4(organisms.size() * Options::new_size)) {
      organisms[worst_i+i].rnd_init();
    }

    // combinations

    for (i += worst_i; i < organisms.size(); i++) {
      
      // pick a good guy and a not so bad guy

      int j = Rnd::Number(0, best_n-1);
      int k = Rnd::Number(0, worst_i-1);

      // and combine them

      organisms[j].combine(organisms[k], organisms[i]);
    }

  }

  return 0;
}
