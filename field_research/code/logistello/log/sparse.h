// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef SPARSE_H
#define SPARSE_H

#include "main.h"
#include "psconf.h"
#include <vector>

class SparseInfo {

public:

  sint2 id2;
  int   index;

  friend bool operator < (const SparseInfo &si1, const SparseInfo &si2);

};


class SparseInfo2 {

public:

  enum { EMPTY = -1 };

  sint2 id1;
  uint2 index;

  void init() {
    id1 = EMPTY;
  }
    
  SparseInfo2() {
    init();
  };


};


class SparseList : public vector<SparseInfo> {

public:

  int my_size;
  inline int find(int id2) const;
  void sort_all();

  SparseList() {
    my_size = 0;
  }

};


class SparsePattern {

private:

  Config     conf1, conf2;

public:

  int        sq_list1[65], sq_list2[65]; // 10x10, endmarker 0
  int        dsymm_n, dsymm[8];          // different transformations

  int        id1n, id2n;   // number of indices
  int        var_num;
  int        config_total;
  int        check_sum;    // of config-file
  SparseList *tab_id1;

  SparseInfo2 *hash_tab;   // optional hash-table
  int         hash_n;
  int         *hash_id1_seeds;

  int         hash_incr[65];  // for optimized value access
  sint2       *hash_tab2;

  SparsePattern();
  ~SparsePattern();

  void read(const char *file_name);
  void hash_read(const char *file_name);
  inline int index(int id1, int id2) const;
  bool append(int id1, SparseInfo &si); // true iff appended
  inline bool exists(int id1) const;
  inline int  fast_index(int id1, int id2) const;
 
  void tab_write(float *tab, FILE *fp) const;
  bool tab_read(FILE *fp, float *tab);
  void free_lists();

  void construct_hash_tab();
  void hash_speed_adjust();
  inline int hash_addr(int id1, int id2) const;
  inline bool hash_exists(int id1) const;
  inline int hash_fast_index(int id1, int id2) const;
  inline int hash_tab_value(int id1, int id2, short *tab) const;
};

// for speed:

inline int SparsePattern::index(int id1, int id2) const
{
  assert(id1 >= 0 && id1 < id1n);
  assert(id2 >= 0 && id2 < id2n);

  int i = tab_id1[id1].find(id2);

  if (i < 0) return -1;

  return tab_id1[id1][i].index;
}


inline int SparseList::find(int id2) const
{
  if (size() < 1) return -1; // empty list

  int r = size()-1, l;
  l = 0;

  // binary search

  while (l <= r) {

    int m = (l+r)/2;
    int v = (*this)[m].id2;

    if (v == id2) return m;
    if (v < id2) l = m+1; else r = m-1;
  }

  return -1;  // not found
}


inline bool SparsePattern::exists(int id1) const
{
  assert(id1 >= 0 && id1 < id1n);
  assert(tab_id1[id1].my_size == tab_id1[id1].size());
  return tab_id1[id1].my_size >= 1;
}

inline int SparsePattern::fast_index(int id1, int id2) const
{
  //  cout << "id1=" << id1 << " id2=" << id2;

  assert(id1 >= 0 && id1 < id1n);
  assert(id2 >= 0 && id2 < id2n);

  SparseList &sl = tab_id1[id1];

  assert(sl.my_size >= 1);
  assert(sl.my_size == sl.size());

  uint4 r = sl.my_size-1, l = 0;

  // binary search

  //static int tests = 0;
  //static int calls = 0;

//calls++;
//if ((calls & 65535) == 0) cout << calls << " " << double(tests)/calls << endl;
 
  while (l <= r) {

    //tests++;

    int m = (l+r)/2;
    int v = sl[m].id2;

    if (v == id2) { 
      // cout << "-> " << sl[m].index << endl; 
      return sl[m].index; 
    }
    if (v < id2) l = m+1; 
    else {
      if (m == 0) { 
	// cout << "-> -1" << endl; 
	return -1; 
      }
      r = m-1;
    }
  }

  //  cout << "-> -1" << endl; 
  return -1;
}


inline int SparsePattern::hash_addr(int id1, int id2) const
{
  assert(hash_id1_seeds[id1] + id2 < hash_n);
  return (hash_id1_seeds[id1] + id2);
}


inline bool SparsePattern::hash_exists(int id1) const
{
  return hash_id1_seeds[id1] != 0;
}


inline int SparsePattern::hash_fast_index(int id1, int id2) const
{
  assert(id1 >= 0 && id1 < id1n);
  assert(id2 >= 0 && id2 < id2n);

  //  cout << "id1=" << id1 << " id2=" << id2;

  int ha = hash_addr(id1, id2);

  SparseInfo2 &si = hash_tab[ha];
 
  // cout << " " << si.id1 << " " << si.id2 << " " << si.index;

  if (si.id1 == id1 /* && si.id2 == id2 always true!*/) { 
    // if (si.id2 != id2) Error("i2 diff.");
    //  cout << "-> " << si.index << endl; 
    return si.index; 
  }

  // cout << "-> -1" << endl; 
  return -1;
}

inline int SparsePattern::hash_tab_value(int id1, int id2, sint2 *tab) const
{
  register int ret = 0;
  assert(id1 >= 0 && id1 < id1n);
  assert(id2 >= 0 && id2 < id2n);

  //  cout << "id1=" << id1 << " id2=" << id2;

  SparseInfo2 &si = hash_tab[hash_addr(id1, id2)];

  // cout << " " << si.id1 << " " << si.id2 << " " << si.index;

  if (si.id1 == id1) {
    // if (si.id2 != id2) Error("i2 diff.");
    //  cout << "-> " << si.index << endl;
    ret = tab[si.index];
  }

  return ret;
}

#endif
