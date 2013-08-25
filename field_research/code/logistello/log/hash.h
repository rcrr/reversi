// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef HASH_H
#define HASH_H

#include "main.h"
#include "sboard.h"
#include "wert.h"

#define HASH 1    // use hash

class HashTab;

class HashEntry {

public:

  enum {
    VALUE_BITS  = 22,	/* 22 */
    HEIGHT_BITS  = 6,	/* 5  */
    STAMP_BITS  = 26,
    LOCK_BITS   = 32	/* 27 */
  };

  enum VALUE_TYPE { 
    VALUE_EXACT=0, VALUE_LE_MINIMAX, VALUE_GE_MINIMAX, VALUE_UNKNOWN
  };

  enum {
    MAX_HEIGHT = ((1<<HEIGHT_BITS)-2),
    END_HEIGHT = ((1<<HEIGHT_BITS)-1)     // endgame hashentry
  };

  inline static uint4 hash_white(uint4 h) { return h ^ 1; }

  inline static bool locks_equal(uint4 l1, uint4 l2) {
#if 0
    assert(LOCK_BITS < 32);
    return ((l1-l2) & ((1<<LOCK_BITS)-1)) == 0;
#else
    assert(LOCK_BITS == 32);
    return l1 == l2;
#endif
  }


  inline static bool stamps_equal(uint4 s1, uint4 s2) {
#if 1
    assert(STAMP_BITS < 32);
    return ((s1-s2) & ((1<<STAMP_BITS)-1)) == 0;
#else
    assert(STAMP_BITS == 32);
    return s1 == s2;
#endif
  }


  static uint4 board_hash(BRETT *pbr, PARTEI to_move);

  static uint4 board_lock(BRETT *pbr, PARTEI to_move);

  static uint4 sboard_lock(SPFELD *psf);

private:

  // 12 bytes
  
  signed   int best_move : 8;		
  signed   int value	 : VALUE_BITS;	// evaluation or bound value
  unsigned int value_type: 2;		// what is it?

  unsigned int height    : HEIGHT_BITS;	// height of the tree
  unsigned int stamp     : STAMP_BITS;	// time stamp

  unsigned int lock      : LOCK_BITS;	

public:
  
  HashEntry() { clear(); }

  inline void clear() {
    best_move = ZUG_UNBEKANNT;
    lock = 0;
    stamp = 0;
  }
  
  inline void set(
		  uint4 s,
		  uint4 l,
		  int h,
		  WERT a,
		  WERT b,
		  int bm,
		  WERT v) {
    stamp = s;
    lock = l;
    best_move = bm;
    height = h;
    value = v;
    if      (value <= a) value_type = VALUE_GE_MINIMAX;	
    else if (value >= b) value_type = VALUE_LE_MINIMAX;	
    else  	         value_type = VALUE_EXACT;
  }

  inline uint4 get_lock()       const { return lock; }
  inline uint4 get_best_move()  const { return best_move; }
  inline uint4 get_height()     const { return height; }
  inline sint4 get_value()      const { return value; }
  inline void  set_value_type(int t) { value_type = t; }
  inline uint4 get_value_type() const { return value_type; }
  inline uint4 get_stamp()      const { return value_type; }

};



class HashTab {

private:

  enum { INIT_STAMP = 10, MAX_STAMP = 1000000 };
  
  static bool initialized;
  static void init_tabs();
  
  uint4 n;
  uint4 mask;
  HashEntry *tab;
  uint4 stamp;
  int max_depth;
  
public:
  
  static uint4 ZufBLACK1[100], ZufWHITE1[100], ZufBW1[100],  // hash
    ZufBLACK2[100], ZufWHITE2[100], ZufBW2[100];  // lock

  //  static uint4 ZufBLACK1T[8][100], ZufWHITE1T[8][100];       // transpositions

  HashTab() {
    n = 0; mask = 0; tab = 0; stamp = 0; max_depth = 0;
  }

  void init(int bits, int max_d=64, bool save_all = false) {
    assert(bits > 0 && bits < 30);

    init_tabs();

    max_depth = max_d;
    n = 1 << bits;
    mask = n-1;
    stamp = INIT_STAMP;
    // leaves room for hash update heuristic (overwrite if stamp-diff >= 2)
    tab = new HashEntry[n];
  }

  HashTab(int bits, int max_d=64, bool save_all = false) {
    init(bits, max_d, save_all);
  }

  ~HashTab() {
    delete [] tab;
    tab = 0;
    n = 0;
    mask = 0;
  }

  
  inline void clear() {
    uint4 i;
    FOR (i, n) tab[i].clear();
  }

  
  inline void set(HashEntry &e, uint4 l, int h, WERT a, WERT b, SFPOS bm, WERT v) {
    e.set(stamp, l, h, a, b, bm, v);
  }

  
  inline HashEntry &get(uint4 h, PARTEI to_move) {
    if (to_move == WHITE) h = HashEntry::hash_white(h);
    return tab[h & mask];
  } 

  inline HashEntry &get_probing(uint4 hash, uint4 lock, PARTEI to_move) {
    if (to_move == WHITE) hash = HashEntry::hash_white(hash);

    uint4 ha = hash & mask;
    HashEntry *ph, *ph_min;
  
    const int PROBE_N = 6;
    static int probe_deltas[PROBE_N] = { 0 , +2, -8 , +18, -32, +50 };
    int i, min_height=MAXINT;

    FOR (i, PROBE_N) {

      ph = &tab[(ha+probe_deltas[i]) & mask];

      if (HashEntry::locks_equal(ph->get_lock(), lock)) {
	ph_min = ph;
	break;
      }

      if ((int)ph->get_height() < min_height) {
	min_height = ph->get_height();
	ph_min = ph;
      }

    }

    return *ph_min;
  }
  
  inline void next_stamp() {
    stamp++;
    if (stamp > MAX_STAMP) {
      stamp = INIT_STAMP;
      clear();
    }
  }

  inline uint4 get_stamp() const { return stamp; }
  inline uint4 get_max_depth() const { return max_depth; }
  inline sint4 stamp_minus(uint4 my_stamp) const { return stamp-my_stamp; }

};


class HashArray {

private:

  int n;
  HashTab **tabs;

public:

  HashArray(int bits, int n_ = 1) {
    n = n_;
    tabs = new HashTab*[n];

    int i;
    FOR (i, n) {
      printf("%d %d\n", i, bits);
      tabs[i] = new HashTab(bits);
    } 
  }

  HashArray(int bits0, int n_, int *bits_array) {
    n = n_;
    tabs = new HashTab*[n];
    
    int i;
    FOR (i, n) {
      printf("%d %d\n", i, bits0+bits_array[i]);
      tabs[i] = new HashTab(bits0+bits_array[i]);
    } 
  }

  ~HashArray() {
    int i;
    FOR (i, n) delete tabs[i];
    delete [] tabs;
    tabs = 0;
    n = 0;
  }
  
  inline void clear() {
    int i;
    FOR (i, n) tabs[i]->clear();
  }

  inline HashEntry &get(int index, uint4 hash, uint4 lock) {
    assert(0 <= index && index < n);
    return tabs[index]->get(hash, lock);
  }

  inline HashTab &get_tab(int index) {
    assert(0 <= index && index < n);
    return *tabs[index];
  }
  
  inline void next_stamp() {
    int i;
    FOR (i, n) tabs[i]->next_stamp();
  }

  inline uint4 get_stamp() const { return tabs[0]->get_stamp(); }


};

#endif
