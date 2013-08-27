// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PSCONF_H
#define PSCONF_H

#include "sboard.h"

typedef unsigned long long ULL;


class BitBoard {

public:

  ULL bits;

  BitBoard() { bits = 0LL; }
};

#if 0

class BoardInfo {

public:

  BitBoard black, white;
  sint1 result;

  friend ostream &operator << (ostream &os, BoardInfo &info);

};


typedef BoardInfo *PBoardInfo;

#endif


class Config {

public:


  int n;      // number of occurrences
  int pred;   // predecessor index
  uint1 size; // number of squares

  BitBoard mask, black, white;

  real4 mean;
  real4 stddev;

  static int glob_res_sum;
  static int glob_n;

  Config() { n = size = 0; pred = -1; }

  bool maximal() const;

  void transform(Config cos[8]) const;
  void to_board(SPFELD &bo) const;
  void from_board(SPFELD &bo);

  enum { MAX_STR_LEN = 200 }; // for to_string

  void to_string(char *s) const;
  int  from_string(char *s);
  void from_line(char *line, int *sq_list);

  int index(int *sq_list);
  
  friend ostream &operator << (ostream &os, const Config &conf);
  friend bool operator < (const Config &conf1, const Config &conf2);
  friend bool operator == (const Config &conf1, const Config &conf2);
};



#endif
