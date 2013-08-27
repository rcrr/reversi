// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// voting idiots / 1.01

#include "main.h"
#include "crt.h"
#include "patt.h"
#include "eval.h"
#include "trans.h"
#include "disco.h"

#include <vector.h>
#include <algo.h>

#define LEN       4
#define NUM_LIMIT 200
#define DEV_LIMIT (100.0)
#define VAL_LIMIT (6.0)

void _abort(void) { exit(20); }

class TVector {

public:
  
  sint1 x[64];
  sint2 r;
  sint2 matched;

  double prev_delta;
  double sum_pred;
  int   hit_num;
};

class SquareCont {

public:
  
  sint1 index;
  sint1 cont;

  friend bool operator < (const SquareCont &s1, const SquareCont &s2);

};


bool operator < (const SquareCont &s1, const SquareCont &s2) {

  return s1.index < s2.index;

}


double stddev(double sum, double sum_sq, int n)
{
  if (n <= 1) return 0;
  return sqrt(sum_sq/n - sum*sum/(n*n));
}

class Idiot {

public:

  vector<SquareCont> sq;
  int   n;
  double sum;
  double sum_sq;
  double val;
  
  // pick random idiot of length num
  
  void generate(int num) {

    bool a[64];
    memset(a, 0, sizeof(a));

    sq.erase(sq.begin(), sq.end());
    
    FORS (i, num) {

      int r;
      SquareCont sc;
      
      do { r = (random() & 63); } while (a[r]);

      a[r] = true;

      sc.index = r;
      sc.cont = random() % 3 - 1;
      sq.push_back(sc);
    }

    sort(sq.begin(), sq.end());
    reset();
  }

  void reset() {
    n = 0;
    sum = 0;
    sum_sq = 0;
  }
  
  void update(double val, double val_sq) {
    ++n;
    sum += val;
    sum_sq += val_sq;
  }

  void set_val() {
    val = sum/n;
  }
  
  bool match(TVector &v) {

    for (uint4 i = sq.size(); i > 0; --i) {
      if (v.x[sq[i-1].index] != sq[i-1].cont) return false;
    }

    return true;
  }

  
  void write() {

    FORU (i, sq.size()) {
      cout << char(sq[i].index/8 + 'A')  << sq[i].index%8+1
	   << (sq[i].cont == 0 ? "-" : ((sq[i].cont > 0) ? "x" : "o"))
	   << " ";
    }

    if (n > 0)
    
      cout.form("%7d %6.2f %6.2f", n, sum/n, stddev(sum, sum_sq, n));
    else
      cout << "%";
		
  }
  
};


int main(int argc, char **argv)
{
  FILE *fp=0;
  char *training_name = 0;
  char *test_name = 0;

  vector<TVector> training_data;
  
  InitSetzen();

  if (argc <= 2) {

  error: ;

  Error("usage: oidiots training-sfk test-sfk");
  exit(20);
  }

  training_name = argv[1];

  if (argc >= 3)
    test_name = argv[2];
  else
    test_name = training_name;

  fp = fopen(training_name, "r");
  if (!fp) Error("can't open training-sfk");

  cout << "reading training positions ... " << flush;

  double delta = 0, delta_sq = 0;

  FOREVER {
  
    SPFELD bo;
    int num_b, num_w;
    
    if (!fSfReadNum(fp, &bo, num_b, num_w)) break;
    
    int res = bo.Marke - (MA_DIFF+64);
    if (res < -64 || res > 64) Error("no diff-label");
    
    if (res & 1) {
      if (res > 0) res++;
      if (res < 0) res--;
    }
    
    TVector v;
    
    FORS (i, 64) {
      v.x[i] = bo.p[Tab8to10[i]];
    }

    v.r = res;
    v.hit_num = 0;
    v.sum_pred = 0;
    v.prev_delta = 0;
    training_data.push_back(v);

    delta += res;
    delta_sq += res*res;
  }

  fclose(fp);

  cout << training_data.size() << " positions "
       << delta/training_data.size() << " "
       << stddev(delta, delta_sq, training_data.size())
       << endl;

  // generate idiots and test them

  vector<Idiot> idiots;
  
  for (sint4 num = 0;; ++num) {

    Idiot idiot;

    idiot.generate(LEN);

    FORU (i, training_data.size()) {

      TVector &v = training_data[i];

      v.matched = false;
      
      if (idiot.match(v)) {

	v.matched = true;
	idiot.update(v.r, v.r * v.r);
      }
    }

    cout.form("%6d: new idiot ", num+1);
    cout << " ";
    idiot.write();

    if (idiot.n >= NUM_LIMIT) {

      idiot.set_val();

      if (fabs(idiot.val) >= VAL_LIMIT) {

	FORU (i, training_data.size()) {
      
	  TVector &v = training_data[i];

	  if (v.matched) {

	    if (v.hit_num > 0) {

	      delta -= v.prev_delta;
	      delta_sq -= v.prev_delta*v.prev_delta;

	    }

#if 0
	    cout.form("%3d n=%6d idiot=%6.2f  old=%6.2f new=%6.2f\n",
		      v.r, v.hit_num+1, idiot.val, v.sum_pred/(v.hit_num+0.0001),
		      (v.sum_pred+idiot.val)/(v.hit_num+1.0001));
#endif
	    v.sum_pred += idiot.val;
	    ++v.hit_num;

	    double pred = v.sum_pred/v.hit_num;
#if 0
	    cout.form("%6d %6.2f\n", v.r, v.r-pred);
#endif
	    
	    v.prev_delta = v.r - pred;
	  
	    delta += v.prev_delta;
	    delta_sq += v.prev_delta * v.prev_delta;
	  }
	}
      
	double avg = delta/training_data.size();

	cout.form(" --> %6.2f %6.2f",
		  avg,
		  stddev(delta, delta_sq, training_data.size())
		  );

	cout << " ### " << delta << " " << delta_sq;
      }
    }
    cout << endl;

  }
  
  return 0;
}
