// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef REGSPATT_H
#define REGSPATT_H

#include "regm.h"
#include "regentry.h"
#include "sparse.h"

struct SPatternInfo {
  char *name;
  char *conf_file;
};


class SPattern {

public:

  enum { MAX_LEN = 16 };

  char  *name;
  Entry *tab;
  int   trans_num;
  int   sq_lists1[8][MAX_LEN+1];
  int   sq_lists2[8][MAX_LEN+1];
  SparsePattern sp;

  SPattern();
  void init(SPatternInfo &spinf);
  void conf_write(FILE *fp, int l, int n);
  void asc_write(FILE *fp);
  void bin_write(FILE *fp);
  bool bin_read(FILE *fp);
  bool bin_write_freq(FILE *fp, int n);

};


class SPatterns : public vector<SPattern> {};

#endif
