// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef REGDPATT_H
#define REGDPATT_H

#include "regm.h"
#include "regentry.h"

struct DPatternInfo {
  int sq[100];
  char *name;
};


class DPattern {

public:

  enum { MAX_LEN = 16 };

  char  *name;
  int   sq[100];
  int   len;
  int   tab_num;
  int   tabi[100];    // square->tab index
  Entry *tabs[100];   // tabs: 0..tab_num-1
  int   sq_list[MAX_LEN+1];
  int   height, width;
  int   xmax, ymax;

  DPattern();

  void init(DPatternInfo &pinf, bool allocate_tabs=true);
  void asc_write(FILE *fp);
  void bin_write(FILE *fp);
  bool bin_read(FILE *fp);
  bool bin_write_freq(FILE *fp, int n);

  static void conf_write(FILE *fp, int l, int n);

  int indices(sint1 *bo, int trans, int &tab_index);
  Estimate *e_pointer(SPFELD &bo, int trans);
};


class DPatterns : public vector<DPattern> {};

#endif
