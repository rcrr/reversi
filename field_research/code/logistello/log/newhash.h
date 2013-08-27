// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

... under construction

#include "move.h"


class NewHashEntry {

public:

  enum {
    VALUE_BITS  = 22,	/* 22 */
    HEIGHT_BITS = 6,	/* 5  */
    STAMP_BITS  = 26,
    LOCK_BITS   = 32	/* 27 */
  };

  enum VALUE_TYPE { 
    VALUE_EXACT=0, VALUE_LE_MINIMAX, VALUE_GE_MINIMAX, VALUE_UNKNOWN
  };

  enum {
    MAX_DEPTH = ((1<<DEPTH_BITS)-2),
    END_DEPTH = ((1<<DEPTH_BITS)-1)     // endgame hashentry
  }


private:

  // 12 bytes
  
  signed   int best_move : 8;		
  signed   int value	 : VALUE_BITS;	// evaluation or bound value
  unsigned int value_type: 2;		// what is it?

  unsigned int height    : HEIGHT_BITS;	// height of the tree
  unsigned int stamp     : STAMP_BITS;	// time stamp

  unsigned int lock      : LOCK_BITS;	

public:
  
  NewHashEntry() { clear(); }

  inline void clear() {
    best_move = ZUG_UNBEKANNT;
    lock = 0;
    stamp = 0;
  }
  
  inline void set(uint4 s, uint4 l, int h, WERT a, WERT b, int bm, WERT v) {
    stamp = s;
    lock = l;
    best_move = bm;
    height = h;
    value = v;
    if      (value <= al) value_type = VALUE_GE_MINIMAX;	
    else if (value >= be) value_type = VALUE_LE_MINIMAX;	
    else  	          value_type = VALUE_EXACT;
  }

  inline uint4 get_lock()   const { return lock; }
  inline uint4 get_height() const { return height; }
  inline sint4 get_value()  const { return value; }
  inline uint4 get_value_type() const { return value_type; }
  inline uint4 get_stamp()  const { return value_type; }
};



class NewHashTab {

private:

  static bool initialized;
  static uint4 ZufBLACK1[100], ZufWHITE1[100], ZufBW1[100],  // hash
               ZufBLACK2[100], ZufWHITE2[100], ZufBW2[100];  // lock

  //  static uint4 ZufBLACK1T[8][100], ZufWHITE1T[8][100];       // transpositions

  static void init_tabs();
  
  uint4 n;
  uint4 mask;
  NewHashEntry *tab;
  uint4 stamp;
  
public:
  
  NewHash(int bits, bool save_all = false) {
    assert(bits > 0 && bits < 30);

    if (!initialized) { initialized = true; init_tabs(); }
    
    n = 1 << bits;
    mask = n-1;
    stamp = 10; // leaves room for hash update heuristic (overwrite if stamp-diff >= 2)
    tab = new NewHashEntry[n];
  }


  ~NewHash() {
    delete [] tab;
    tab = 0;
    n = 0;
    mask = 0;
  }

  inline void clear() {
    uint4 i;
    FOR (i, n) tab[i].clear();
  }
    
  inline void set(NewEntry &e, uint4 l, int h, WERT a, WERT b, int bm, WERT v) {
    e.set(stamp, l, h, a, b, bm, v);
  }

  inline NewHashEntry &get(uint4 hash, SFPOS to_move) {
    return tab[(hash & mask) ^ (to_move == WHITE)];
  } 

  inline sint4 stamp_minus(uint4 my_stamp) const { return stamp-my_stamp; }
};


class NewHashArray {

private:

  int n;
  NewHashTab *tabs;

public:

  NewHashArray(int n_ = 1) {
    n = n_;
    tabs = new NewHastTab[n];
  }

  ~NewHashLine() {
    delete [] tabs;
    tabs = 0;
    n = 0;
  }
  
  inline void clear() {
    int i;
    FOR (i, n) tabs[i].clear();
  }

  inline NewHashEntry &get(int index, uint4 hash, uint4 lock) {
    assert(0 <= index && index < n);
    return tabs[index].get(hash, lock);
  }
    
  inline void next_stamp() {
    int i;
    FOR (i, n) tabs[i].next_stamp;
  }

  inline uint4 get_stamp() const { return tabs[0].stamp; }

  
};
