// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

#ifndef PSEARCHM_H
#define PSEARCHM_H

#include "main.h"
#include "sboard.h"


typedef unsigned long long ULL;




class BitBoard {

public:

  ULL bits;

  BitBoard() { bits = 0LL; }
};




class BoardInfo {

public:

  BitBoard black, white;
  sint1 result;

  friend ostream &operator << (ostream &os, BoardInfo &info);

};


typedef BoardInfo *PBoardInfo;



class Config {

public:

  BitBoard mask, black, white;
  int n;        // number of occurrences
  int pred;
  uint1 size;
  uint1 bit_index; // for extension
  static int glob_res_sum;
  static int glob_n;

  Config() { n = size = 0; pred = -1; bit_index = 0; }

  inline bool match(BoardInfo &inf) 
  {
    return (black.bits == (inf.black.bits & mask.bits) && 
	    white.bits == (inf.white.bits & mask.bits));
  }

  bool maximal();

  void transform(Config cos[8]);
  void to_board(SPFELD &bo);
  void from_board(SPFELD &bo);

  friend ostream &operator << (ostream &os, const Config &conf);
  friend bool operator < (const Config &conf1, const Config &conf2);
  friend bool operator == (const Config &conf1, const Config &conf2);
};



#endif
