// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// compute fraction of "legal" positions

#include "main.h"
#include "sboard.h"
#include <set.h>
#include "trans.h"

#define LEGAL_POSITIONS 0

const int MASK_MOVE_N = 4;
const int ITER_N = 5000000;

inline static int rnd_col() { if (random() & 8) return BLACK; else return WHITE; }

void _abort() {}


static bool three_in_line(SPFELD &bo)
{
  int i, j, k;

  FOR_SFPOS10(i) {

    if (i == D4 || i == D5 || i == E4 || i == E5) continue;
    if (bo.p[i] == LEER) continue;

    FOR (j, 8) {
      int l = i;

      FOR (k, 2) {
	l += ds[j];
	if (!TabZug[l] || bo.p[l] == LEER) break;
      }

      if (k >= 2) break;
    }

    if (j >= 8) {
      //      KoorAus(i); cout << " ";
      return false;
    }
  }

  return true;
}


#if 0

struct lspfeld
{
  bool operator()(const SPFELD &s1, const SPFELD &s2) const
  {
    int i;
    
    FOR (i, 100) 
      if (s1.p[i] != s2.p[i]) {
	return s1.p[i] < s2.p[i];
      }

    return false;
  }
};

typedef set<SPFELD, lspfeld> MaskSet;

MaskSet mask_set;


static void to_mask(SPFELD &bo, SPFELD &ma)
{
  int i;

  FOR (i, 100) {
    ma.p[i] = bo.p[i];
    if (ma.p[i] == WHITE) ma.p[i] = BLACK;
  }
}

static bool mask_fits(const SPFELD &ma, SPFELD &bo)
{
  int i;

  FOR (i, 100) {
    if (ma.p[i] == BLACK && bo.p[i] == LEER) return false;
  }
  return true;
}

static void add_masks(SPFELD &bo, int player, int height)
{
  if (height <= 0) {

    SPFELD ma, tr[8];

    to_mask(bo, ma);
    Transform(&ma, tr);
    int i;
    FOR (i, 8) mask_set.insert(tr[i]);
    return;
  }

  SFPOS moves[65];

  int mn = SfMoeglZuege(&bo, player, moves);

  int i;
  
  FOR (i, mn) {
    SPFELD bo1 = bo;

    if (!SfSetzen(&bo1, player, moves[i])) Error("bla");
    add_masks(bo1, -player, height-1);
  }

}

static void compute_masks(int depth)
{
  SPFELD bo;
  SfGrund(&bo);

  add_masks(bo, BLACK, depth);

#if 0
  MaskSet::iterator i;

  for (i=mask_set.begin(); i != mask_set.end(); i++) {
    SfAus(&(*i), 0, 0);
  }
#endif
}

static bool mask_ok(SPFELD &bo)
{
  MaskSet::iterator i;

  for (i=mask_set.begin(); i != mask_set.end(); i++) {
    if (mask_fits(*i, bo)) return true;
  }

  return false;
}

#endif

class Square {

public:
  
  int sq;

  Square(int s) { sq = s; }
};


struct ltSquare {

  bool operator()(const Square &sq1, const Square &sq2) const
  {
    return sq1.sq < sq2.sq;
  }

};

typedef set<Square, ltSquare> SquareSet;

bool connected(SPFELD &bo)
{
  int i;
  
  FOR (i, 100)
    if (bo.p[i] == BLACK || bo.p[i] == WHITE) break;

  if (i >= 100) Error("no disc");

  // if (bo.p[D4] == LEER) Error("d4 empty");

  SquareSet sq_set, new_set, newnew_set;

  new_set.insert(Square(i));

  while (new_set.size() > 0) {

    newnew_set.erase(newnew_set.begin(), newnew_set.end());
    
    SquareSet::iterator i;

    for (i=new_set.begin(); i != new_set.end(); i++) {
      sq_set.insert(*i);
    }
    
    for (i=new_set.begin(); i != new_set.end(); i++) {
      int j;
      Square sq = *i;
      sq_set.insert(sq);
      FOR (j, 8) {
	int l = sq.sq+ds[j];
	if (TabZug[l] && bo.p[l] != LEER &&
	    sq_set.find(Square(l)) == sq_set.end())
	  newnew_set.insert(l);
      }
    }

    new_set = newnew_set;
  }

  return (int)sq_set.size() == SfAnz(&bo);
}


//  :::
//  :X:  illegal
//  :O:

//  :::
//  :X:*   illegal if one of * is not a disc
//  ::O
//   *




bool limbs_ok(SPFELD &bo)
{
  int i, j;

  FOR_SFPOS10(i) {
    if (bo.p[i] != LEER) {

      // only one disc in vicinity?

      int dn = 0, neighbor_j = 0;
      
      FOR (j, 8) {
	if (bo.p[i+ds[j]] == BLACK || bo.p[i+ds[j]] == WHITE) {
	  dn++;
	  neighbor_j = j;
	}
      }

      if (dn == 0) {
	KoorAus(i);
	return false;  // single disc
      }

      if (dn == 1 && bo.p[i] != bo.p[i+ds[neighbor_j]]) {
	int a = abs(ds[neighbor_j]);

	if (a == 1 || a == 10) {
	  return false;  // one neighbor & colors different!
	}

	if (a == 9 && 
	    (
	     (bo.p[i+ds[neighbor_j]+11] != BLACK &&
	      bo.p[i+ds[neighbor_j]+11] != WHITE)
	     || 
	     (bo.p[i+ds[neighbor_j]-11] != BLACK &&
	      bo.p[i+ds[neighbor_j]-11] != WHITE)
	     )) {
	  
	  //KoorAus(i); cout << "B" << a; 
	  return false;
	}

	if (a == 11 &&
	    (
	     (bo.p[i+ds[neighbor_j]+9] != BLACK &&
	      bo.p[i+ds[neighbor_j]+9] != WHITE)
	     || 
	     (bo.p[i+ds[neighbor_j]-9] != BLACK &&
	      bo.p[i+ds[neighbor_j]-9] != WHITE)
	     )) {

	  //KoorAus(i); KoorAus(i+ds[neighbor_j]); cout << "C" << a; 
	  return false;
	}
      }
    }
  }

  return true;
}

real8 fak(int n)
{
  real8 fak = 1.0;
  for (int i=2; i <= n; i++) fak *= i;
  return fak;
}

int main()
{
  int iter;
  
  //  compute_masks(MASK_MOVE_N);
  // cout << mask_set.size() << " masks" << endl;

  for (int n0=4; n0 <= 64; n0++) {

    int count = 0;
    int illegal = 0;
    
    FOR (iter, ITER_N) {
      
      // generate random position with N discs
      
      SPFELD bo;
      int i, n = n0;
      
      SfGrund(&bo);
      
#if LEGAL_POSITIONS
      bo.p[D4] = rnd_col();
      bo.p[D5] = rnd_col();
      bo.p[E4] = rnd_col();
      bo.p[E5] = rnd_col();
      n -= 4;
#else
      bo.p[D4] = LEER;
      bo.p[D5] = LEER;
      bo.p[E4] = LEER;
      bo.p[E5] = LEER;    
#endif
      
      FOR (i, n) {
	int sq = random() % 100;
	if (bo.p[sq] != LEER) { i--; continue; }
	bo.p[sq] = rnd_col();
      }

      count++;
      bool r;
      
#if LEGAL_POSITIONS      
      r = three_in_line(bo) && limbs_ok(bo);
#else
      r = connected(bo);
#endif

      if (!r) illegal++;

      if (r) {

#if 0
	if (n & 1) cout << "-> ##"; else cout << "-> ()";
	cout << endl;
	SfAus(&bo, 0, 0);
	cout << three_in_line(bo) << endl;
#endif
      
      }
    }

    real8 total;

#if LEGAL_POSITIONS      

    total = (fak(60)/fak(n0-4)/fak(60-n0))*pow(2,n0);

#else

    total = fak(64)/fak(n0)/fak(64-n0);
    
#endif
    
    cout << n0 << " " << total 
	 << " " << count << " " << illegal << " " << (real8)count/(count-illegal)
	 << " -> " << total/((real8)count/(count-illegal))
	 << endl;
    
  }
}

